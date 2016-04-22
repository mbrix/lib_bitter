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

-export([serialize/1,
	     serialize_btxdef/1,
	     create_tx/0,
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
	     readable/2,
	     readable_txhash/1,
	     readable_txhash/2,
	     hexstr_to_txhash/1,
	     from_hex/1,
	     vector/1,
	     sigs/1,
	     total/1,
	     to_json/2,
	     to_map/2]).


-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

% TX Verification

decode_script(p2pkh, <<>>) ->
	empty_script;

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
	case libsecp256k1:ecdsa_verify(Hash, Signature, PublicKey) of
		ok -> true;
		_ -> false
	end.
	
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

sign_tx(SigHashType, Tx, KeypairDict, Proposals, Unspents) ->
	SignedInputs = sign_inputs(SigHashType, Tx#btxdef.txinputs, Tx, KeypairDict, Proposals, Unspents),
	Tx#btxdef{txinputs=SignedInputs}.

sign_inputs(SigHashType, Inputs, Tx, KeypairDict, Proposals, Unspents) ->
	lists:map(fun(I) ->
                      case dict:find({bblock:hash(I), bblock:index(I)}, Unspents) of
		{ok, Unspent} ->
			case input_type(I) of
				{unrecognized, Hash160} ->
					verify_input(unrecognized, SigHashType, Tx, Hash160, I, KeypairDict, Proposals, lib_unspent:script(Unspent));
                {InputType, Hash160} ->
					sign_input(InputType, SigHashType, Tx, Hash160, I, KeypairDict, Proposals, lib_unspent:script(Unspent));
                {InputType, _SubType, Hash160} ->
					sign_input(InputType, SigHashType, Tx, Hash160, I, KeypairDict, Proposals, lib_unspent:script(Unspent))
				end;
			_ -> I
		end
		end, Inputs).


verify_input(p2pkh, SigHashType, Tx, _Key, Input, _KeypairDict, _, Script) ->
	Hash = hash_tx(intermediate_tx(SigHashType, clear_input_scripts(replace_input_script(Tx, Input, Script), Input))),
    case decode_script(p2pkh, bblock:script(Input)) of
		{Signature, PublicKey} ->
            bblock:signed(Input, verify_signature(Hash, Signature, PublicKey));
		empty_script -> bblock:signed(Input, false)
	end;

verify_input(p2sh, SigHashType, Tx, Key, Input, KeypairDict, Proposals, Script) ->
	{{Info, RedeemScript}, Sigs} = sigs(bblock:script(Input)),
	Hash = hash_tx(intermediate_tx(SigHashType,
				clear_input_scripts(replace_input_script(Tx, Input, RedeemScript), Input))),
	% Iterate over every signature and validate
	{{M,_N}, Keylist} = Info,
	ValidSigs = lists:filter(fun(E) ->
					isvalid(E, Hash, SigHashType, Keylist) end, Sigs),
	IsSigned = (length(ValidSigs) >= M),
	I2 = bblock:signed(Input, IsSigned),
	case IsSigned of
		true -> I2;
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
			bblock:signed(bblock:replace_script(Input, ScriptSig), verify_signature(Hash, Signature, PublicKey));
		error -> Input
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
			{_, ScriptStack} = sigs(bblock:script(Input)),
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
			bblock:signed(bblock:replace_script(Input, ScriptSigsBin), is_complete(ScriptSigsBin));
		error -> Input
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
	not lists:any(fun(I) -> bblock:is_signed(I) =/= true end, Tx#btxdef.txinputs).

readable_txhash(binary, TxHash) ->
	iolist_to_binary(readable_txhash(TxHash)).

readable_txhash(TxHash) when is_binary(TxHash) ->
	hex:bin_to_hexstr(hex:bin_reverse(TxHash));
readable_txhash(Tx) when is_record(Tx, btxdef) ->
    hex:bin_to_hexstr(hex:bin_reverse(Tx#btxdef.txhash)).

readable(binary, Tx) when is_record(Tx, btxdef) ->
	iolist_to_binary(readable(Tx)).

readable(Tx) when is_record(Tx, btxdef) ->
	hex:bin_to_hexstr(serialize_btxdef(Tx)).

hexstr_to_txhash(Hexstr) -> hex:bin_reverse(hex:hexstr_to_bin(Hexstr)).

from_hex(Hexstr) ->
	RawTX = hex:hexstr_to_bin(Hexstr),
	[T|_] = lib_parse:getTransactions(1, RawTX),
	[Tx|_] = T,
	Tx.

create_signature(Hash, _PublicKey, PrivateKey) ->
	{ok, Signature} = libsecp256k1:ecdsa_sign(Hash, PrivateKey, default, <<>>),
	Signature.

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
    case dict:find({bblock:hash(Input), bblock:index(Input), PublicKey},
			ProposalDict) of
		{ok, Signature} ->
			Signature;
		error ->
			missing
	end.

get_keypair(Key, KeypairDict) ->
	dict:find(Key, KeypairDict).

unspent_type(Output) -> lib_parse:parse_script(lib_unspent:script(Output)).

input_type(Input) ->
	lib_parse:parse_script(bblock:script(Input)).

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

% Required for P2SH signing

% Clear input scripts for every input but Input's txhash
clear_input_scripts(Tx, Input) ->
	Hash = bblock:hash(Input),
	Index = bblock:index(Input),
	Tx#btxdef{txinputs = lists:map(fun(I) ->
                                           case bblock:hash(I) of
                                               Hash ->
                                                   case bblock:index(I) of
                                                       Index -> I;
                                                       _ -> bblock:replace_script(I, <<>>)
                                                   end;
                                               _ ->
                                                   bblock:replace_script(I, <<>>)
                                           end end, Tx#btxdef.txinputs)}.

replace_input_script(Tx, Input, NewScript) ->
	Hash = bblock:hash(Input),
	Index = bblock:index(Input),
	Tx#btxdef{txinputs = lists:map(fun(I) ->
                                           case bblock:hash(I)  of
                                               Hash ->
                                                   case bblock:index(I) of 
                                                       Index -> bblock:replace_script(I, NewScript);
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
            address = lib_address:script_to_address(Info, Script)};
create_output(Type, Color, ColorQuant, PubkeyBin) ->
	Script = create_script(Type, PubkeyBin),
	Info = lib_parse:parse_script(Script),
	#btxout{value = ?DUSTLIMIT,
		    script = Script,
            info = Info,
            address = lib_address:script_to_address(Info, Script),
            attributes = #{color => Color, quantity => ColorQuant}}.

% Add unspents to a TX
% Translated into Inputs

add_unspent(Tx, UnspentList) when is_list(UnspentList) ->
	add_input(Tx, lists:map(fun(E) -> lib_unspent:create_input(E) end, UnspentList));

add_unspent(Tx, Unspent) -> add_unspent(Tx, [Unspent]).


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
	lists:foldl(fun(E,Acc) -> add_output(Acc, E) end, Tx, Outputs);

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

serialize(Tx) when is_record(Tx, btx) -> bblock:serialize(Tx);

serialize(Tx) when is_record(Tx, btxdef) -> serialize_btxdef(Tx).

serialize_btxdef(Tx) ->
	TransactionVersion    	=  Tx#btxdef.txversion,  
	InputCount            	=  length(Tx#btxdef.txinputs), 
	OutputCount           	=  length(Tx#btxdef.txoutputs),
	TransactionLockTime   	=  Tx#btxdef.txlocktime,
	TxInputs	            =  Tx#btxdef.txinputs,
	TxOutputs	            =  Tx#btxdef.txoutputs,  
	erlang:iolist_to_binary(
	[<< TransactionVersion:32/little>>,
		lib_parse:int_to_varint(InputCount),
		serialize_inputs(TxInputs),
		lib_parse:int_to_varint(OutputCount),
		serialize_outputs(TxOutputs),
		<<TransactionLockTime:32/little>>]).


serialize_inputs(Inputs) when is_list(Inputs) ->
	lists:map(fun(I) -> serialize_inputs(I) end, Inputs);

serialize_inputs(I) ->
	Txhash = bblock:hash(I), 
	TxIndex = bblock:index(I),
	Script = bblock:script(I),
	SeqNum = bblock:seqnum(I),
	ScriptLength = size(Script),
	[<<Txhash:32/binary,
	   TxIndex:32/little>>,
	   lib_parse:int_to_varint(ScriptLength),
	   <<Script:ScriptLength/binary,
	     SeqNum:32/little>>].

serialize_outputs(Outputs) when is_list(Outputs) ->
	lists:map(fun(O) -> serialize_outputs(O) end, Outputs);

serialize_outputs(O) ->
	Value = bblock:value(O),
	Script = bblock:script(O),
	[<<Value:64/little>>,
	lib_parse:int_to_varint(size(Script)),
	<<Script/binary>>].



% Vector conversion
vector([A,B]) -> [A, B]; 
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
    add_outputs(T, Value + bblock:value(H)).

to_json(NetworkParams, Tx) -> jiffy:encode(to_map(NetworkParams, Tx)).

to_map(NetworkParams, Tx) when is_binary(Tx) ->
    to_map(NetworkParams, lib_parse:parse_tx(Tx));
to_map(NetworkParams, Tx) when is_record(Tx, btxdef) ->
	{Hexstr, Txid} = hexstr_txhash(Tx),
    #{
            hex => Hexstr,
            txid => Txid,
            version => Tx#btxdef.txversion,
            locktime => Tx#btxdef.txlocktime,
            vin => lists:reverse(inputs_to_json(Tx#btxdef.txinputs)),
            vout => lists:reverse(outputs_to_json(NetworkParams, Tx#btxdef.txoutputs))
            %% blockhash not stored outside of block
            %% confirmations not stored outside of unspent pool
            %% time not stored outside of block
            %% blocktime now stored outside of block
    }.

hexstr_txhash(#btxdef{txhash = undefined}) -> {<<"not signed">>, <<"not signed">>};
hexstr_txhash(Tx) ->
	{iolist_to_binary(hex:bin_to_hexstr(serialize(Tx))),
	 iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(Tx#btxdef.txhash)))}.


inputs_to_json(Txinputs) ->
    lists:foldl(fun(E, Acc) ->
                        Hash = bblock:hash(E),
                Txid = iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(Hash))),
                case Hash of
                    ?COINBASE ->
                        [#{coinbase => iolist_to_binary(hex:bin_to_hexstr(bblock:script(E))),
                           sequence => bblock:seqnum(E)}|Acc];
                    _ ->
                        [#{txid => Txid,
                           vout => bblock:index(E),
                           scriptsig => scriptsig_to_json(bblock:script(E)),
                           sequence => bblock:seqnum(E)}|Acc]
                        end end, [], Txinputs).

scriptsig_to_json(Script) ->
    Hexbin = iolist_to_binary(hex:bin_to_hexstr(Script)),
    #{hex => Hexbin,
      asm => <<"">>}.

outputs_to_json(NetworkParams, Txoutputs) ->
    {_, O} = lists:foldl(fun(E, {Vcounter, Outputs}) ->
                {Vcounter+1, [#{value => lib_transact:satoshi_to_btc(bblock:value(E)),
                                vout => Vcounter,
                                scriptPubKey => scriptpubkey_to_json(bblock:script(E)),
                                color => color_to_json(NetworkParams, lib_unspent:get_attribute(color, E, ?Uncolored)),
                                quantity => lib_unspent:get_attribute(quantity, E, 0)}|Outputs]}
            end, {0, []}, Txoutputs),
    O.

scriptpubkey_to_json(Script) ->
    Hexbin = iolist_to_binary(hex:bin_to_hexstr(Script)),
    #{hex => Hexbin,
      asm => <<"">>}.

color_to_json(_, ?Uncolored) -> <<"uncolored">>;
color_to_json(NetworkParams, C) when is_binary(C) -> lib_color:readable(binary, NetworkParams, lib_color:new(C)).



