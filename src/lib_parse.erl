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

-module(lib_parse).
-author('mbranton@emberfinancial.com').

-export([parse/2,
	     parse_block/2,
	     parse_tx/1,
		 parse_script/1,
	     getTransactions/2]).

-define(MAGICBYTE, 16#D9B4BEF9).
-include("bitter.hrl").

parse(Fname, Fun) ->
	case file:read_file(Fname) of
		{ok, Data} ->
			extractLoop(Data, Fun),
			{reply, ok, {}};
		{error, Reason} ->
			{stop, Reason, {Fname}}
	end.

parse_block(Block, Fun) ->
	Size = size(Block),
	B = erlang:iolist_to_binary([<<?MAGICBYTE:32/little, 
    						     Size:32/little>>,
								 Block]),
	extractLoop(B, Fun).

parse_tx(TxData) ->
	[T, _] = getTransactions(1, TxData),
	T.

extractLoop(Data, CallBackFun) -> extractLoop(Data, 10000000, CallBackFun).
extractLoop(_, 0, _) -> ok;
extractLoop(Data, Loops, CallBackFun) ->
	case extract(Data) of
		{ok, Block2, Next} ->
			   CallBackFun(Block2),
			   extractLoop(Next, Loops-1, CallBackFun);
		{scan, Next} ->
			<<_:8, Bin/binary>> = Next,
			extractLoop(Bin, Loops, CallBackFun);
		ok ->
			ok
	end.

% Normalize the return into a finger count
norm(?OP_0) -> 0;
norm(OP_X) -> OP_X - 80.

% Script classification
parse_script(<<?OP_DUP:8, ?OP_HASH160:8, 16#14:8, Pubkey:160/bitstring, ?OP_EQUALVERIFY:8, ?OP_CHECKSIG:8>>) ->
	{p2pkh, Pubkey};
parse_script(<<?OP_HASH160:8, 16#14:8, Hash:160/bitstring, ?OP_EQUAL:8>>) -> {p2sh, Hash};
% P2SH classification
parse_script(<<?OP_HASH160:8, Hash:160/bitstring, ?OP_EQUAL:8>>) -> {p2sh, Hash};
% Less common types
parse_script(<<?OP_SHA256:8, Pubkey:264/bitstring, ?OP_EQUAL:8>>) -> {p2pkh2, Pubkey};
parse_script(<<65:8, Pubkey:520/bitstring, ?OP_CHECKSIG:8>>) -> {full_pubkey, Pubkey};
parse_script(<<33:8, Pubkey:264/bitstring, ?OP_CHECKSIG:8>>) -> {compressed_pubkey, Pubkey};
parse_script(<<32:8, Pubkey:256>>) -> {malformed, Pubkey};

% Common Multisig parsing
% N of 1 ??
parse_script(<<M:8, 33:8, _:8, Pubkey:256/bitstring, ?OP_1:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),1}, [Pubkey]}};
parse_script(<<M:8, 65:8, _:8, Pubkey:512/bitstring, ?OP_1:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),1}, [Pubkey]}};

% N of 2
parse_script(<<M:8, 33:8, Pubkey:264/bitstring, 33:8, Pubkey2:264/bitstring, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};
parse_script(<<M:8, 33:8, Pubkey:264/bitstring, 65:8, Pubkey2:520/bitstring, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};
parse_script(<<M:8, 65:8, Pubkey:520/bitstring, 33:8, Pubkey2:264/bitstring, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};
parse_script(<<M:8, 65:8, Pubkey:520/bitstring, 65:8, Pubkey2:520/bitstring, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};

% 1 of 3
parse_script(<<M:8, 33:8, Pubkey:264/bitstring, 33:8, Pubkey2:264/bitstring, 33:8, Pubkey3:264/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 33:8, Pubkey:264/bitstring, 33:8, Pubkey2:264/bitstring, 65:8, Pubkey3:520/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 33:8, Pubkey:264/bitstring, 65:8, Pubkey2:520/bitstring, 33:8, Pubkey3:264/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:520/bitstring, 33:8, Pubkey2:264/bitstring, 33:8, Pubkey3:264/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:520/bitstring, 33:8, Pubkey2:264/bitstring, 65:8, Pubkey3:520/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:520/bitstring, 65:8, Pubkey2:520/bitstring, 33:8, Pubkey3:264/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 33:8, Pubkey:264/bitstring, 65:8, Pubkey2:520/bitstring, 65:8, Pubkey3:520/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:520/bitstring, 65:8, Pubkey2:520/bitstring, 65:8, Pubkey3:520/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

% Open Assets Marker parsing
parse_script(<<?OP_RETURN, B/binary>>) ->
	case getVarInt(B) of
		error ->
			parse_script_2(B);
		[Length,R] ->
			try parse_openassets(Length, R) of
				{openassets, Result} ->
					{openassets, Result};
				notopenassets ->
					parse_script_2(B)
			catch
				_:_ ->
					{malformed_openassets, B}
			end
	end;

parse_script(Bin) ->
	{unrecognized, Bin}.

strip_info_result({openassets, B}) ->
	{openassets,B};
strip_info_result({A,_B}) -> A.

parse_openassets(_L, <<16#4f:8, 16#41:8, 16#01, 16#00, B/binary>>) ->
	[AssetQuantityCount, R] = getVarInt(B),
	{QList, R2} = parse_oa_qlist(AssetQuantityCount, R),
	MetaData = parse_oa_metadata(R2),
	{openassets, {QList, MetaData}};
parse_openassets(_L, <<_/binary>>) ->
	notopenassets.

parse_oa_qlist(Q, B) -> parse_oa_qlist(Q,B,[]).
parse_oa_qlist(0, B, Acc) ->
	{lists:reverse(Acc), B};
parse_oa_qlist(Q, B, Acc) ->
	{Value, Tail} = leb128:decode(B, unsigned),
	parse_oa_qlist(Q-1, Tail, [Value|Acc]).

parse_oa_metadata(B) ->
	[MetaLength, R] = getVarInt(B),
	Bits = MetaLength*8,
	<<D:Bits/bitstring, _/binary>> = R,
	D.

parse_script_2(B) ->
	{op_return, B}.

getVarInt(<< TXCount:8, BinRest/binary >>) when TXCount < 253 -> [TXCount, BinRest];
getVarInt(<< 253:8, TXCount:16/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(<< 254:8, TXCount:32/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(<< 255:8, TXCount:64/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(_) -> error.

getTxInputs(InputCount, Rest) -> getTxInputs(InputCount, Rest, []).
getTxInputs(0, Rest, Acc) -> [Acc, Rest];
getTxInputs(InputCount, Rest, Acc) ->
	<< Txhash:256/bitstring,
	   TxIndex:32/little,
	   BinRest/binary>> = Rest,
	   [ScriptLength, TxRest] = getVarInt(BinRest),
	   AdjustedScriptLength = ScriptLength*8,
	   << Script:AdjustedScriptLength/bitstring, SeqNum:32/little, Next/binary >> = TxRest,
	   getTxInputs(InputCount-1, Next, [#btxin{txhash=Txhash,
	   			                               txindex=TxIndex,
	   			                               script=Script,
	   			                               seqnum=SeqNum}|Acc]).

getTxOutputs(OutputCount, Rest) -> getTxOutputs(OutputCount, Rest, [], 0).
getTxOutputs(0, Rest, Acc, _) -> [Acc, Rest];
getTxOutputs(OutputCount, Rest, Acc, Index) ->
	<< Value:64/little,
	   BinRest/binary>> = Rest,
	   [ScriptLength, TxRest] = getVarInt(BinRest),
	   AdjustedScriptLength = ScriptLength*8,
	   << Script:AdjustedScriptLength/bitstring, Next/binary >> = TxRest,
	   ScriptExtendedInfo = parse_script(binary:copy(Script)),
	   Address = lib_address:script_to_address(ScriptExtendedInfo, Script),
	   getTxOutputs(OutputCount-1, Next, [#btxout{txindex=Index, value=Value, script=Script, address=Address, info=strip_info_result(ScriptExtendedInfo)}|Acc], Index+1).

getTransactions(TXCount, Tbin) -> getTransactions(TXCount, Tbin, []).
getTransactions(0, Tbin, Acc) -> [Acc, Tbin];
getTransactions(TXCount, Tbin, Acc) ->
	<< TransactionVersion:32/little, Rest/binary >> = Tbin,
	[InputCount, Inputs] = getVarInt(Rest),
	[TxInputs, Rest2] = getTxInputs(InputCount, Inputs),
	[OutputCount, Outputs] = getVarInt(Rest2),
	[TxOutputs, Rest3] = getTxOutputs(OutputCount, Outputs),
    <<TransactionLockTime:32/little, Next/binary>> = Rest3,
    TXLength = byte_size(Tbin) - byte_size(Next),
	TransactionHash = crypto:hash(sha256, crypto:hash(sha256, binary:part(Tbin, {0,TXLength}))),
	getTransactions(TXCount-1, Next, [#btxdef{txhash=TransactionHash,
										      txversion=TransactionVersion,
										      inputcount=InputCount,
										      outputcount=OutputCount,
										      txlocktime=TransactionLockTime,
										      txinputs=lists:reverse(TxInputs),
										      txoutputs=lists:reverse(TxOutputs)} | Acc]).

extract(<< >>) -> ok;
extract(<<?MAGICBYTE:32/little, 
    HeaderLength:32/little,
    VersionNumber:32/little, 
    PreviousHash:256/bitstring, 
    MerkleRoot:256/bitstring, 
    TimeStamp:32/little, 
    TargetDifficulty:32/little, 
    Nonce:32/little,
    BinRest/binary>>) ->
    % Repack subset to create Block Hash
    HashBin = <<VersionNumber:32/little,
                PreviousHash:256/bitstring,
                MerkleRoot:256/bitstring,
                TimeStamp:32/little,
                TargetDifficulty:32/little,
                Nonce:32/little>>,
	BlockHash = crypto:hash(sha256, crypto:hash(sha256, HashBin)),
   [TXCount, Tbin] = getVarInt(BinRest),
   [Tdata, _Rest] = getTransactions(TXCount, Tbin),
   {ok, #bbdef{network=?MAGICBYTE,
   		                 blockhash=BlockHash,
   		                 headerlength=HeaderLength,
   		                 version=VersionNumber,
   		                 previoushash=PreviousHash,
   		                 merkleroot=MerkleRoot,
   		                 timestamp=TimeStamp,
   		                 difficulty=TargetDifficulty,
   		                 nonce=Nonce,
   		                 txcount=TXCount,
						 txdata=lists:reverse(Tdata)}, _Rest};
 extract(<<R:8, _Bin/binary>>) when R > 0 ->
	io:format("Problem: ~w~n", [binary:bin_to_list(_Bin, {0, 10})]),
	{scan, _Bin};
extract(Data) -> {scan, Data}.
