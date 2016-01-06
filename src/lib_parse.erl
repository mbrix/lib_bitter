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

-export([parse/1,
         parse_file/1,
	     parse_block/2,
	     parse_raw_block/1,
	     parse_raw_block/2,
	     parse_raw/1,
	     parse_tx/1,
		 parse_script/1,
	     getTransactions/2,
	     getTransactions/3,
	     getVarInt/1,
	     headers/2,
	     extract/1,
	     extract_header/1]).

-include("bitter.hrl").

parse_raw(Bin) ->
    {continue, BlockRecord, _BlockOffset, _Offsets} = extract(Bin),
    BlockRecord.

parse(Fname) ->
	case file:read_file(Fname) of
		{ok, Data} -> extractLoop(Data, 0);
		{error, Reason} -> {stop, Reason, {Fname}}
	end.

%% For debugging, breaks file up into an array in ram.
parse_file(Filename) ->
    parse_file(parse(Filename), []).

parse_file({continue, Block, BlockOffsets, TxOffsets, Fun}, Acc) ->
    parse_file(Fun(), [{Block, BlockOffsets, TxOffsets}|Acc]);

parse_file(done, Acc) -> Acc.


parse_block(MagicByte, Block) ->
	Size = size(Block),
	B = erlang:iolist_to_binary([<<MagicByte:32/little, 
    						     Size:32/little>>,
								 Block]),
	extractLoop(B, 0).

parse_raw_block(Block) ->
    extractLoop(Block, 0).

parse_raw_block(Block, Offset) ->
    extractLoop(Block, Offset).

parse_tx(TxData) ->
	[[T], _, _] = getTransactions(1, TxData),
	T.

extractLoop(Data, StartOffset) ->
%	try
	    case extract(Data) of
	    	{ok, Block2, TxOffsets, Next} ->
	    	    BlockSize = byte_size(Data) - byte_size(Next),
	    	    NewTxOffsets = adjust_offsets(TxOffsets, StartOffset),
                {continue, Block2,
                 {StartOffset, BlockSize}, NewTxOffsets,
                 fun() -> extractLoop(Next, StartOffset+BlockSize) end};
	    	{scan, Next} ->
	    		<<_:8, Bin/binary>> = Next,
	    		extractLoop(Bin, StartOffset+1);
	    	ok -> 
	    	    erlang:garbage_collect(self()),
	    	    done
	    end.
%	catch
%		_:_ -> done
%	end.

adjust_offsets(TxOffsets, Adjustment) ->
    lists:map(fun(E) ->
                {Hash, Offset, Length} = E,
                {Hash, Offset+Adjustment, Length} end, TxOffsets).

% Normalize the return into a finger count
norm(?OP_0) -> 0;
norm(OP_X) -> OP_X - 80.

% Script classification
parse_script(<<?OP_DUP:8, ?OP_HASH160:8, 16#14:8, Pubkey:20/binary, ?OP_EQUALVERIFY:8, ?OP_CHECKSIG:8>>) ->
	{p2pkh, Pubkey};
parse_script(<<?OP_HASH160:8, 16#14:8, Hash:20/binary, ?OP_EQUAL:8>>) -> {p2sh, Hash};
% P2SH classification
parse_script(<<?OP_HASH160:8, Hash:20/binary, ?OP_EQUAL:8>>) -> {p2sh, Hash};
% Less common types
parse_script(<<?OP_SHA256:8, Pubkey:33/binary, ?OP_EQUAL:8>>) -> {p2pkh2, Pubkey};
parse_script(<<65:8, Pubkey:65/binary, ?OP_CHECKSIG:8>>) -> {full_pubkey, Pubkey};
parse_script(<<33:8, Pubkey:33/binary, ?OP_CHECKSIG:8>>) -> {compressed_pubkey, Pubkey};
parse_script(<<32:8, Pubkey:32>>) -> {malformed, Pubkey};

% Common Multisig parsing
% N of 1 ??
parse_script(<<M:8, 33:8, _:8, Pubkey:32/binary, ?OP_1:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),1}, [Pubkey]}};
parse_script(<<M:8, 65:8, _:8, Pubkey:64/binary, ?OP_1:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),1}, [Pubkey]}};

% N of 2
parse_script(<<M:8, 33:8, Pubkey:33/binary, 33:8, Pubkey2:33/binary, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};
parse_script(<<M:8, 33:8, Pubkey:33/binary, 65:8, Pubkey2:65/binary, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};
parse_script(<<M:8, 65:8, Pubkey:65/binary, 33:8, Pubkey2:33/binary, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};
parse_script(<<M:8, 65:8, Pubkey:65/binary, 65:8, Pubkey2:65/binary, ?OP_2:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),2}, [Pubkey, Pubkey2]}};

% 1 of 3
parse_script(<<M:8, 33:8, Pubkey:33/binary, 33:8, Pubkey2:33/binary, 33:8, Pubkey3:33/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 33:8, Pubkey:33/binary, 33:8, Pubkey2:33/binary, 65:8, Pubkey3:65/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 33:8, Pubkey:33/binary, 65:8, Pubkey2:65/binary, 33:8, Pubkey3:33/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:65/binary, 33:8, Pubkey2:33/binary, 33:8, Pubkey3:33/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:65/binary, 33:8, Pubkey2:33/binary, 65:8, Pubkey3:65/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:65/binary, 65:8, Pubkey2:65/binary, 33:8, Pubkey3:33/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 33:8, Pubkey:33/binary, 65:8, Pubkey2:65/binary, 65:8, Pubkey3:65/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

parse_script(<<M:8, 65:8, Pubkey:65/binary, 65:8, Pubkey2:65/binary, 65:8, Pubkey3:65/binary, ?OP_3:8, ?OP_CHECKMULTISIG>>) ->
	{multisig, {{norm(M),3}, [Pubkey, Pubkey2, Pubkey3]}};

% Open Assets Marker parsing
parse_script(<<?OP_RETURN, B/binary>>) ->
	case getVarInt(B) of
		error ->
			parse_script_2(B);
		[Length,R] ->
			try parse_openassets(Length, R) of
				{openassets, Result} -> {openassets, Result};
				notopenassets -> parse_pushdata_script(B)
			catch
				_:_ -> {malformed_openassets, B}
			end
	end;


parse_script(Bin) ->
	{unrecognized, Bin}.

%% If varint fails we need to try the new pushdata spec
parse_pushdata_script(B) ->
    case getPushData(B) of
		error -> parse_script_2(B);
		[Length,R] ->
			try parse_openassets(Length, R) of
				{openassets, Result} -> {openassets, Result};
				notopenassets -> parse_script_2(B)
			catch
				_:_ -> {malformed_openassets, B}
			end
    end.

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
	if Value > 9223372036854775807 ->
	        throw(marker_invalid);
	    true ->
	        parse_oa_qlist(Q-1, Tail, [Value|Acc])
    end.

parse_oa_metadata(B) ->
	[MetaLength, R] = getVarInt(B),
	<<D:MetaLength/binary, _/binary>> = R,
	D.

parse_script_2(B) ->
	{op_return, B}.

getPushData(<< TXCount:8, BinRest/binary >>) when TXCount < 76 -> [TXCount, BinRest];
getPushData(<< ?OP_PUSHDATA1:8, TXCount:8/little, BinRest/binary >>) -> [TXCount, BinRest];
getPushData(<< ?OP_PUSHDATA2:8, TXCount:16/little, BinRest/binary >>) -> [TXCount, BinRest];
getPushData(<< ?OP_PUSHDATA4:8, TXCount:32/little, BinRest/binary >>) -> [TXCount, BinRest];
getPushData(_) -> error.

getVarInt(<<TXCount:8, BinRest/binary >>) when TXCount < 253 -> [TXCount, BinRest];
getVarInt(<<253:8, TXCount:16/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(<<254:8, TXCount:32/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(<<255:8, TXCount:64/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(_) -> error.

getTxInputs(InputCount, Rest) -> getTxInputs(InputCount, Rest, []).
getTxInputs(0, Rest, Acc) -> [Acc, Rest];
getTxInputs(InputCount, Rest, Acc) ->
	<< Txhash:32/binary,
	   TxIndex:32/little,
	   BinRest/binary>> = Rest,
	   [ScriptLength, TxRest] = getVarInt(BinRest),
	   << Script:ScriptLength/binary, SeqNum:32/little, Next/binary >> = TxRest,
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
	   << Script:ScriptLength/binary, Next/binary >> = TxRest,
	   ScriptExtendedInfo = parse_script(Script),
	   Address = lib_address:script_to_address(ScriptExtendedInfo, Script),
	   getTxOutputs(OutputCount-1, Next, [#btxout{txindex=Index,
	                                           value=Value,
	                                           script=Script,
	                                           address=Address,
	                                           info=strip_info_result(ScriptExtendedInfo)}|Acc],
	             Index+1).

getTransactions(TXCount, Tbin) -> getTransactions(TXCount, Tbin, [], [], 0).
getTransactions(TXCount, Tbin, StartOffset) -> getTransactions(TXCount, Tbin, [], [], StartOffset).
getTransactions(0, Tbin, Acc, Offsets, _StartOffset) -> [Acc, Tbin, Offsets];
getTransactions(TXCount, Tbin, Acc, Offsets, StartOffset) ->
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
										      txoutputs=lists:reverse(TxOutputs)} | Acc],
				[{TransactionHash, StartOffset, TXLength}|Offsets],
				StartOffset + TXLength).

extract(<<>>) -> ok;
extract(<<MagicByte:32/little, 
    HeaderLength:32/little,
    VersionNumber:32/little, 
    PreviousHash:32/binary, 
    MerkleRoot:32/binary, 
    TimeStamp:32/little, 
    TargetDifficulty:32/little, 
    Nonce:32/little,
    BinRest/binary>> = L) ->
    try
    % Repack subset to create Block Hash
    HashBin = <<VersionNumber:32/little,
                PreviousHash:32/binary,
                MerkleRoot:32/binary,
                TimeStamp:32/little,
                TargetDifficulty:32/little,
                Nonce:32/little>>,
	BlockHash = crypto:hash(sha256, crypto:hash(sha256, HashBin)),
   [TXCount, Tbin] = getVarInt(BinRest),
    TxDataOffset = 88 + (byte_size(BinRest) - byte_size(Tbin)),
   [Tdata, Rest, TxOffsets] = getTransactions(TXCount, Tbin, TxDataOffset),
   check_difficulty(hex:bin_reverse(BlockHash)),
   {ok, #bbdef{network=MagicByte,
   		       blockhash=BlockHash,
   		       headerlength=HeaderLength,
   		       version=VersionNumber,
   		       previoushash=PreviousHash,
   		       merkleroot=MerkleRoot,
   		       timestamp=TimeStamp,
   		       difficulty=TargetDifficulty,
   		       nonce=Nonce,
   		       txcount=TXCount,
			   txdata=lists:reverse(Tdata)},
	lists:reverse(TxOffsets), Rest}
	catch _:_ ->  {scan, L}
	end;
extract(Data) -> {scan, Data}.

%% This isn't a real difficulty check
%% it's here to filter out crap data that results in
%% partial writes of bad blocks onto disk
check_difficulty(<<0:32, _/binary>>) -> ok;
check_difficulty(_) -> throw(difficulty_check_failure).

extract_header(<<MagicByte:32/little, 
    HeaderLength:32/little,
    VersionNumber:32/little, 
    PreviousHash:32/binary, 
    MerkleRoot:32/binary, 
    TimeStamp:32/little, 
    TargetDifficulty:32/little, 
    Nonce:32/little,
    BinRest/binary>>) ->
    % Repack subset to create Block Hash
    HashBin = <<VersionNumber:32/little,
                PreviousHash:32/binary,
                MerkleRoot:32/binary,
                TimeStamp:32/little,
                TargetDifficulty:32/little,
                Nonce:32/little>>,
	BlockHash = crypto:hash(sha256, crypto:hash(sha256, HashBin)),
   [TXCount, _Tbin] = getVarInt(BinRest),
   {ok, #bbdef{network=MagicByte,
   			   blockhash=BlockHash,
   		       headerlength=HeaderLength,
   		       version=VersionNumber,
   		       previoushash=PreviousHash,
   		       merkleroot=MerkleRoot,
   		       timestamp=TimeStamp,
   		       difficulty=TargetDifficulty,
   		       nonce=Nonce,
   		       txcount=TXCount,
               txdata=[]}}.


%% Parse network headers
headers(MagicByte, Packet) ->  
 [header_to_bbdef(MagicByte, Head) || <<Head:81/binary>> <= Packet].

header_to_bbdef(MagicByte, Bin) ->
     <<VersionNumber:32/little,
     PreviousHash:32/binary, 
     MerkleRoot:32/binary, 
     TimeStamp:32/little, 
     TargetDifficulty:32/little, 
     Nonce:32/little,
     0:8>> = Bin,
	BlockHash = crypto:hash(sha256, crypto:hash(sha256, Bin)),
     #bbdef{network = MagicByte,
     		blockhash = BlockHash,
     		headerlength = 80,
     		version = VersionNumber,
     		previoushash = PreviousHash,
     		merkleroot = MerkleRoot,
     		timestamp = TimeStamp,
     		difficulty = TargetDifficulty,
     		nonce = Nonce,
     		txcount = 0,
     		txdata  = []}.
