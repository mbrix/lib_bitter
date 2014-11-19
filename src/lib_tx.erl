%
% [Ember Financial] INC ("COMPANY") CONFIDENTIAL
% Unpublished Copyright (c) 2014-2015 [Ember Financial, Inc], All Rights Reserved.
%
% NOTICE:  All information contained herein is, and remains the property of
% COMPANY. The intellectual and technical concepts contained herein are 
% proprietary to COMPANY and may be covered by U.S. and Foreign Patents, 
% patents in process, and are protected by trade secret or copyright law.
% Dissemination of this information or reproduction of this material is 
% strictly forbidden unless prior written permission is obtained from COMPANY.
% Access to the source code contained herein is hereby forbidden to anyone 
% except current COMPANY employees, managers or contractors who have executed 
% Confidentiality and Non-disclosure agreements explicitly covering such access.
%
% The copyright notice above does not evidence any actual or intended publication
% or disclosure  of  this source code, which includes information that is
% confidential and/or proprietary, and is a trade secret, of  COMPANY.  
% ANY REPRODUCTION, MODIFICATION, DISTRIBUTION, PUBLIC  PERFORMANCE, OR PUBLIC
% DISPLAY OF OR THROUGH USE  OF THIS  SOURCE CODE  WITHOUT  THE EXPRESS WRITTEN
% CONSENT OF COMPANY IS STRICTLY PROHIBITED, AND IN VIOLATION OF APPLICABLE 
% LAWS AND INTERNATIONAL TREATIES.  THE RECEIPT OR POSSESSION OF  THIS SOURCE
% CODE AND/OR RELATED INFORMATION DOES NOT CONVEY OR IMPLY ANY RIGHTS TO
% REPRODUCE, DISCLOSE OR DISTRIBUTE ITS CONTENTS, OR TO MANUFACTURE, USE, OR
% SELL ANYTHING THAT IT  MAY DESCRIBE, IN WHOLE OR IN PART.                
%

-module(lib_tx).
-author('mbranton@emberfinancial.com').

-export([int_to_varint/1,
	     varint_to_int/1,  %duped code
	     int_to_pushdata/1,
	     pushdata_to_int/1,
	     serialize/1,
	     serialize_btxdef/1,
	     create_tx/0,
	     create_input/1,
		 create_output/4,
	     add_input/2,
	     add_output/2,
		 add_unspent/2,
	     hash_tx/1,
	     dhash_tx/1,
	     set_locktime/2,
	     set_version/2,
	     sign_tx/5,
	     intermediate_tx/2,
	     input_type/1,
		 unspent_type/1,
	     replace_input_script/3,
	     find_input/2,
	     find_output/2,
	     calculate_fee/1,
	     create_script/2,
	     is_signed/1,
	     clear_input_scripts/2,
	     readable/1,
	     from_hex/1,
	     vector/1,
	     sigs/1,
	     total/1,
	     to_json/1]).


-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

% TX Verification

decode_script(p2pkh, Script) when is_binary(Script) ->
	<<Length:8/little, Rest/binary>> = Script,
	LengthBits = Length*8,
	<<Signature:LengthBits/bitstring, Rest2/binary>> = Rest,
	<<KeyLength:8/little, Rest3/binary>> = Rest2,
	KeyLengthBits = KeyLength*8,
	<<PublicKey:KeyLengthBits/bitstring>> = Rest3,
	{Signature, PublicKey}.

%decode_script(p2sh, Script) when is_binary(Script) ->
%	ok.

verify_signature(Hash, Signature, PublicKey) ->
	crypto:verify(ecdsa, sha256, {digest, Hash}, Signature, [PublicKey, secp256k1]).
	
% Tx Misc operations

calculate_fee(Tx) when is_binary(Tx) ->
    NSize = size(Tx)+148,
    NFee = erlang:trunc(?FEE_PER_K*(NSize / 1000)),
	if
		NFee < ?DEFAULTFEE -> ?DEFAULTFEE;
		true -> NFee + ?DEFAULTFEE
	end;
calculate_fee(Tx) ->
	calculate_fee(serialize_btxdef(Tx)).

% TX Signing
%

intermediate_tx(SigHashType, Tx) ->
	erlang:iolist_to_binary([serialize_btxdef(Tx), <<SigHashType:32/little>>]).

sign_tx(SigHashType, Tx,
		KeypairDict, Proposals,
		Unspents) ->
	SignedInputs = sign_inputs(SigHashType, Tx#btxdef.txinputs, Tx, KeypairDict, Proposals, Unspents),
	Tx#btxdef{txinputs=SignedInputs}.

sign_inputs(SigHashType, Inputs, Tx, KeypairDict, Proposals, Unspents) ->
	lists:map(fun(I) ->
		{ok, Unspent} = dict:find({I#btxin.txhash, I#btxin.txindex}, Unspents),
		{InputType, Hash160} = unspent_type(Unspent),
		case I#btxin.script of
			<<>> ->
				sign_input(InputType, SigHashType, Tx, Hash160, I, KeypairDict, Proposals, Unspent#utxop.script);
			_ ->
				verify_input(InputType, SigHashType, Tx, Hash160, I, KeypairDict, Proposals, Unspent#utxop.script)
			end
		end, Inputs).


verify_input(p2pkh, SigHashType, Tx, _Key, Input, _KeypairDict, _, Script) ->
	Hash = hash_tx(intermediate_tx(SigHashType, clear_input_scripts(replace_input_script(Tx, Input, Script), Input))),
	{Signature, PublicKey} = decode_script(p2pkh, Input#btxin.script),
	Input#btxin{signed = verify_signature(Hash, Signature, PublicKey)};

verify_input(p2sh, SigHashType, Tx, Key, Input, KeypairDict, Proposals, Script) ->
	{{Info, RedeemScript}, Sigs} = sigs(Input#btxin.script),
	Hash = hash_tx(intermediate_tx(SigHashType,
				clear_input_scripts(replace_input_script(Tx, Input, RedeemScript), Input))),
	% Iterate over every signature and validate
	{{M,_N}, Keylist} = Info,
	ValidSigs = lists:filter(fun(E) ->
					isvalid(E, Hash, SigHashType, Keylist) end, Sigs),
	I2 = Input#btxin{signed = (length(ValidSigs) >= M)},
	case I2#btxin.signed of
		true ->
			I2;
		false ->
			% Lets try and  sign it.	
			sign_input(p2sh, SigHashType, Tx, Key, Input, KeypairDict, Proposals, Script)
	end.

isvalid(_, _,_,[]) -> false;
isvalid({Sig, Type}, Hash, SigHashType, KeyList) when Type =:= SigHashType ->
	[K|R] = KeyList,
	case verify_signature(Hash, Sig, K) of
		true -> true;
		false -> isvalid({Sig, Type}, Hash, SigHashType, R)
	end;
isvalid(_,_,_,_) -> false.

	
sign_input(p2pkh, SigHashType, Tx, Key, Input, KeypairDict, _, Script) ->
	Hash = hash_tx(intermediate_tx(SigHashType, clear_input_scripts(replace_input_script(Tx, Input, Script), Input))),
	case get_keypair(Key, KeypairDict) of
		{ok, {PublicKey, PrivateKey}} ->
			Signature = create_signature(Hash, PublicKey, PrivateKey),
			ScriptSig = create_scriptsig(SigHashType, Signature, PublicKey),
			Input#btxin{script = ScriptSig,
			   signed = verify_signature(Hash, Signature, PublicKey)};
		error ->
			Input
	end;

sign_input(p2sh, SigHashType, Tx, Address, Input, KeypairDict, Proposals, _) ->
	% Keys should be a list of Public keys and/or partially signed inputs
	% 2 keys is assumed to be 2 of 2
	% 3 keys is assumed to be 2 of 3
	% The reason this looks like this is that it is tx re-entrant
	case dict:find(Address, KeypairDict) of
		{ok, KeyList} ->
			{PublicKeyList, _} = lists:unzip(KeyList),
			RedeemScript = lib_address:p2sh_redeemscript(PublicKeyList),
			Hash = hash_tx(intermediate_tx(SigHashType,
						clear_input_scripts(replace_input_script(Tx, Input, RedeemScript), Input))),
			% First thing parse existing scriptsigs and make a stack
			{_, ScriptStack} = sigs(Input#btxin.script),
			{ScriptSigs, _} = lists:foldl(fun(X, {Sigs, Stack}) ->
					case X of
						{PublicKey, proposal} ->
							% Signature has been precalculated
							% Do a Proposal Data Lookup
							% Insert a missing tag into stream if not fulfilled
							case find_proposal(Input, PublicKey, Proposals) of
								missing ->
									case Stack of
									    [] ->
									       {[create_p2sh_scriptsig(SigHashType, ?MISSINGSIG)|Sigs], Stack};
									    _ ->
									    	[{S,T}|R] = Stack,
									    	{[create_p2sh_scriptsig(T, S)|Sigs], R}
									end;
								Signature ->
									{[Signature|Sigs], Stack} 
							end;
						{PublicKey, PrivateKey} ->
							Signature = create_signature(Hash, PublicKey, PrivateKey),
							{[create_p2sh_scriptsig(SigHashType, Signature)|Sigs], Stack}
							end end, {[], ScriptStack}, KeyList),
			RLength = size(RedeemScript),
			ScriptSigsBin = erlang:iolist_to_binary([<<?OP_0:8/little>>,
													lists:reverse(ScriptSigs),
													<<RLength:8/little>>,
													RedeemScript]),
			Input#btxin{script = ScriptSigsBin,
			        signed = is_complete(ScriptSigsBin)};
		error ->
			Input
	end.


is_complete(ScriptSigs) when is_binary(ScriptSigs) ->
	case binary:match(ScriptSigs, ?MISSINGSIG) of
		nomatch ->
			true;
		_ ->
			partial
	end.

sigs(<<>>) -> {<<>>, []};
sigs(<<?OP_0:8/little, ScriptSigs/binary>>) ->
	sigs(ScriptSigs, []);
sigs(_) -> 
	{<<>>, []}. % some formatting error

sigs(<<>>, Acc) ->
	[RedeemInfo|Sigs]  = Acc,
	{RedeemInfo, lists:reverse(Sigs)};

sigs(ScriptSig, Acc) ->
	<<SigLength:8/little, Rest/binary>> = ScriptSig,
	SigLengthBits = (SigLength-1)*8,
	case lib_parse:parse_script(Rest) of
		{multisig, Info} ->
			sigs(<<>>, [{Info, Rest}|Acc]);
		_ ->
			<<Signature:SigLengthBits/bitstring, Type:8/little, Rest2/binary>> =  Rest,
			sigs(Rest2, [{Signature, Type}|Acc])
	end.
	

is_signed(Tx) ->
	not lists:any(fun(I) -> I#btxin.signed =/= true end,
		Tx#btxdef.txinputs).

readable(Tx) when is_record(Tx, btxdef) ->
	hex:bin_to_hexstr(serialize_btxdef(Tx)).

from_hex(Hexstr) when is_list(Hexstr) ->
	RawTX = hex:hexstr_to_bin(Hexstr),
	[T|_] = lib_parse:getTransactions(1, RawTX),
	[Tx|_] = T,
	Tx.

create_signature(Hash, PublicKey, PrivateKey) ->
    PrivKey =  {'ECPrivateKey',1,
    			binary:bin_to_list(PrivateKey),
                {namedCurve,{1,3,132,0,10}},
                {0, PublicKey}},
	public_key:sign({digest, Hash}, sha256, PrivKey).

create_p2sh_scriptsig(SigHashType, Signature) ->
	SigLength = size(Signature),
	SigLengthAdjusted = SigLength+1,
	<<SigLengthAdjusted:8/little,
	  Signature/bitstring,
	  SigHashType:8/little>>.
	

create_scriptsig(SigHashType, Signature, PublicKey) ->
	SigLength = size(Signature),
	SigLengthAdjusted = SigLength+1,
	KeyLength = size(PublicKey),
	<<SigLengthAdjusted:8/little,
	  Signature/bitstring,
	  SigHashType:8/little,
	  KeyLength:8/little,
	  PublicKey/bitstring>>.

find_proposal(Input, PublicKey, ProposalDict) ->
	case dict:find({Input#btxin.txhash, Input#btxin.txindex, PublicKey},
			ProposalDict) of
		{ok, Signature} ->
			Signature;
		error ->
			missing
	end.

get_keypair(Key, KeypairDict) ->
	dict:find(Key, KeypairDict).

unspent_type(Output) ->
	lib_parse:parse_script(Output#utxop.script).

input_type(Input) ->
	lib_parse:parse_script(Input#btxin.script).

dhash_tx(Tx) when is_record(Tx, btxdef) ->
	hash_tx(serialize_btxdef(Tx));

dhash_tx(Tx) when is_binary(Tx) ->
	crypto:hash(sha256, Tx).


hash_tx(Tx) when is_record(Tx, btxdef) ->
	hash_tx(serialize_btxdef(Tx));

hash_tx(Tx) when is_binary(Tx) ->
	crypto:hash(sha256, crypto:hash(sha256, Tx)).

% TX builder pattern

create_tx() ->
	#btxdef{txversion=1,
		    inputcount=0,
		    outputcount=0,
		    txlocktime=0,
		    txinputs=[],
		    txoutputs=[]}.

create_input(U) when is_record(U, utxop) ->
	{Hash, Index} = U#utxop.hash_index,
	%Script = Unspent#utxop.script,
	% Empty unsigned script
	SeqNum = 4294967295, % 0xFFFFFFFF
	#btxin{txhash = Hash,
		   txindex = Index,
	   	   script = <<>>,
		   seqnum = SeqNum};

create_input(I) when is_record(I, btxin) ->
	I.

% Required for P2SH signing

% Clear input scripts for every input but Input#btxin.txhash
clear_input_scripts(Tx, Input) ->
	Hash = Input#btxin.txhash,
	Index = Input#btxin.txindex,
	Tx#btxdef{txinputs = lists:map(fun(I) ->
				case I#btxin.txhash of
					Hash ->
					    case I#btxin.txindex of
					            Index -> I;
                                _ -> I#btxin{script = <<>>}
                            end;
					_ ->
							I#btxin{script = <<>>}
			end end, Tx#btxdef.txinputs)}.

replace_input_script(Tx, Input, NewScript) ->
	Hash = Input#btxin.txhash,
	Index = Input#btxin.txindex,
	Tx#btxdef{txinputs = lists:map(fun(I) ->
				case I#btxin.txhash of
					Hash ->
					    case I#btxin.txindex of
					            Index -> I#btxin{script = NewScript};
					            _ -> I
                            end;
					_ -> I
			end end, Tx#btxdef.txinputs)}.

create_script(p2pkh, PubkeyBin) ->
	<<?OP_DUP:8, ?OP_HASH160:8, 16#14:8, PubkeyBin:160/bitstring, ?OP_EQUALVERIFY:8, ?OP_CHECKSIG:8>>;

% 2 of 2 or 2 of 3
create_script(p2sh, ScriptHash160) when is_binary(ScriptHash160) ->
	erlang:iolist_to_binary([<<?OP_HASH160:8, 16#14:8>>,
			                 ScriptHash160,
			                 <<?OP_EQUAL:8>>]);

create_script(p2sh, PublicKeyList) ->
	RedeemScriptHash = lib_address:p2sh_redeemscript_hash(PublicKeyList),
	create_script(p2sh, RedeemScriptHash).

create_output(Type, ?Uncolored, ValueBits, PubkeyBin) ->
	Script = create_script(Type, PubkeyBin),
	Info = lib_parse:parse_script(Script),
	#btxout{value = ValueBits,
            script = Script,
            info = Info,
            address = lib_address:script_to_address(Info, Script),
            color = ?Uncolored,
            quantity = 0};
create_output(Type, Color, ColorQuant, PubkeyBin) ->
	Script = create_script(Type, PubkeyBin),
	Info = lib_parse:parse_script(Script),
	#btxout{value = ?DUSTLIMIT,
		    script = Script,
            info = Info,
            address = lib_address:script_to_address(Info, Script),
		    color = Color,
		    quantity = ColorQuant}.

% Add unspents to a TX
% Translated into Inputs

add_unspent(Tx, Unspent) when is_record(Unspent, utxop) ->
	add_unspent(Tx, [Unspent]);

add_unspent(Tx, UnspentList) when is_list(UnspentList) ->
	add_input(Tx, lists:map(fun(E) ->
				    create_input(E)
		      end, UnspentList)).

add_input(Tx, Inputs) when is_list(Inputs) ->
	lists:foldl(fun(E,Acc) ->
				add_input(Acc, E)
		end, Tx, Inputs); 
add_input(Tx, Input) ->
	NumInputs = Tx#btxdef.inputcount + 1,
	NewInputs = Tx#btxdef.txinputs ++ [Input],
	Tx#btxdef{inputcount=NumInputs,
		      txinputs=NewInputs}.

add_output(Tx, Outputs) when is_list(Outputs) ->
	lists:foldl(fun(E,Acc) ->
				add_output(Acc, E)
		end, Tx, Outputs); 
add_output(Tx, Output) ->
	NewOutput = Output#btxout{txindex = Tx#btxdef.outputcount},
	NumOutputs = Tx#btxdef.outputcount + 1,
	NewOutputs = Tx#btxdef.txoutputs ++ [NewOutput],
	Tx#btxdef{outputcount=NumOutputs,
		      txoutputs=NewOutputs}.

set_locktime(Tx, Locktime) ->
	Tx#btxdef{txlocktime = Locktime}.

set_version(Tx, Version) ->
	Tx#btxdef{txversion = Version}.

% Search

find_input(Tx, Input) ->
	lists:keyfind(Input#btxin.txhash,
		          #btxin.txhash,
		          Tx#btxdef.txinputs).

find_output(Tx, Output) ->
	lists:keyfind(Output#btxout.txindex,
		          #btxout.txindex,
		          Tx#btxdef.txoutputs).

% Serialization
%
serialize(Tx) when is_record(Tx, btxdef) ->
	serialize_btxdef(Tx).

serialize_btxdef(Tx) ->
	TransactionVersion    	=  Tx#btxdef.txversion,  
	InputCount            	=  Tx#btxdef.inputcount, 
	OutputCount           	=  Tx#btxdef.outputcount,
	TransactionLockTime   	=  Tx#btxdef.txlocktime,
	TxInputs	            =  Tx#btxdef.txinputs,
	TxOutputs	            =  Tx#btxdef.txoutputs,  
	erlang:iolist_to_binary(
	[<< TransactionVersion:32/little>>,
		int_to_varint(InputCount),
		serialize_inputs(TxInputs),
		int_to_varint(OutputCount),
		serialize_outputs(TxOutputs),
		<<TransactionLockTime:32/little>>]).


serialize_inputs(Inputs) when is_list(Inputs) ->
	lists:map(fun(I) ->
				serialize_inputs(I)
		      end, Inputs);
serialize_inputs(I) ->
	Txhash = I#btxin.txhash, 
	TxIndex =I#btxin.txindex,
	Script = I#btxin.script,
	SeqNum = I#btxin.seqnum,
	ScriptLength = size(Script),
	BitLength = ScriptLength*8,
	[<<Txhash:256/bitstring,
	   TxIndex:32/little>>,
	   int_to_varint(ScriptLength),
	   <<Script:BitLength/bitstring,
	     SeqNum:32/little>>].

serialize_outputs(Outputs) when is_list(Outputs) ->
	lists:map(fun(O) ->
				serialize_outputs(O)
		      end, Outputs);
serialize_outputs(O) ->
	Value = O#btxout.value,
	Script = O#btxout.script,
    ScriptLength = size(Script),
    BitLength = ScriptLength*8,
	[<<Value:64/little>>,
	int_to_varint(ScriptLength),
	<<Script:BitLength/bitstring>>].


%% varint

int_to_varint(Int) when Int < 253 -> <<Int:8/little>>;
int_to_varint(Int) when Int =< 65535 -> <<253:8, Int:16/little>>;
int_to_varint(Int) when Int =< 4294967295 -> <<254:8, Int:32/little>>;
int_to_varint(Int) when Int > 4294967295  -> <<255:8, Int:64/little>>;
int_to_varint(_) -> error.

varint_to_int(<< TXCount:8, BinRest/binary >>) when TXCount < 253 -> [TXCount, BinRest];
varint_to_int(<< 253:8, TXCount:16/little, BinRest/binary >>) -> [TXCount, BinRest];
varint_to_int(<< 254:8, TXCount:32/little, BinRest/binary >>) -> [TXCount, BinRest];
varint_to_int(<< 255:8, TXCount:64/little, BinRest/binary >>) -> [TXCount, BinRest];
varint_to_int(_) -> error.

%% pushdata

int_to_pushdata(Int) when Int < 76 -> <<Int:8/little>>;
int_to_pushdata(Int) when Int =< 255 -> <<?OP_PUSHDATA1:8, Int:8/little>>;
int_to_pushdata(Int) when Int =< 65535 -> <<?OP_PUSHDATA2:8, Int:16/little>>;
int_to_pushdata(Int) when Int =< 4294967295 -> <<?OP_PUSHDATA4:8, Int:32/little>>;
int_to_pushdata(_) -> error.

pushdata_to_int(<< TXCount:8, BinRest/binary >>) when TXCount < 76 -> [TXCount, BinRest];
pushdata_to_int(<< ?OP_PUSHDATA1:8, TXCount:8/little, BinRest/binary >>) -> [TXCount, BinRest];
pushdata_to_int(<< ?OP_PUSHDATA2:8, TXCount:16/little, BinRest/binary >>) -> [TXCount, BinRest];
pushdata_to_int(<< ?OP_PUSHDATA4:8, TXCount:32/little, BinRest/binary >>) -> [TXCount, BinRest];
pushdata_to_int(_) -> error.


% Vector conversion
vector(V) when is_record(V, btxdef) ->
	H = V#btxdef.txhash,
	[?INV_TX, << H:256/bitstring >>];
vector(V) when is_record(V, bbdef) ->
	H = V#bbdef.blockhash,
	[?INV_BLOCK, << H:256/bitstring >>];
vector(_) ->
	[?INV_ERROR, << 0:256 >>].


total(Tx) when is_record(Tx, btxdef) ->
    add_outputs(Tx#btxdef.txoutputs).

add_outputs(Tx) -> add_outputs(Tx, 0).

add_outputs([], Value) -> Value;
add_outputs(O, Value) ->
    [H|T] = O,
    add_outputs(T, Value + H#btxout.value).


to_json(Tx) when is_binary(Tx) ->
    [Tx2] = lib_parse:parse_tx(Tx),
    to_json(Tx2);
to_json(Tx) when is_record(Tx, btxdef) ->
    Hexstr = iolist_to_binary(hex:bin_to_hexstr(serialize(Tx))),
    Txid = iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(Tx#btxdef.txhash))),
    jiffy:encode(#{
            hex => Hexstr,
            txid => Txid,
            version => Tx#btxdef.txversion,
            locktime => Tx#btxdef.txlocktime,
            vin => inputs_to_json(Tx#btxdef.txinputs),
            vout => outputs_to_json(Tx#btxdef.txoutputs)
            %% blockhash not stored outside of block
            %% confirmations not stored outside of unspent pool
            %% time not stored outside of block
            %% blocktime now stored outside of block
        }).

inputs_to_json(Txinputs) ->
    lists:foldl(fun(E, Acc) ->
                Txid = iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(E#btxin.txhash))),
                case E#btxin.txhash of
                    ?COINBASE ->
                        [#{coinbase => iolist_to_binary(hex:bin_to_hexstr(E#btxin.script)),
                           sequence => E#btxin.seqnum}|Acc];
                    _ ->
                        [#{txid => Txid,
                           vout => E#btxin.txindex,
                           scriptsig => scriptsig_to_json(E#btxin.script),
                           sequence => E#btxin.seqnum}|Acc]
                        end end, [], Txinputs).

scriptsig_to_json(Script) ->
    Hexbin = iolist_to_binary(hex:bin_to_hexstr(Script)),
    #{hex => Hexbin,
      asm => <<"">>}.

outputs_to_json(Txoutputs) ->
    {_, O} = lists:foldl(fun(E, {Vcounter, Outputs}) ->
                {Vcounter+1, [#{value => lib_transact:satoshi_to_btc(E#btxout.value),
                                vout => Vcounter,
                                scriptPubKey => scriptpubkey_to_json(E#btxout.script)}|Outputs]}
            end, {0, []}, Txoutputs),
    O.

scriptpubkey_to_json(Script) ->
    Hexbin = iolist_to_binary(hex:bin_to_hexstr(Script)),
    #{hex => Hexbin,
      asm => <<"">>}.
