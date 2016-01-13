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

-module(bblock).
-author('mbranton@emberfinancial.com').

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([native/2,
		 parse/2,
		 parse_tx/1,
		 serialize/1,
		 serialize_meta/1,
		 hash/1,
		 previoushash/1,
		 merkleroot/1,
		 version/1,
		 timestamp/1,
		 difficulty/1,
		 nonce/1,
		 tx/2,
		 tx_hashes/1,
		 txs/1,
		 inputs/1,
		 outputs/1,
		 input/2,
		 output/2,
		 tx_count/1,
		 merge/2,
		 data/1,
		 offset/1,
		 meta/1,
		 copy/1,
		 bytesize/1,
		 getattr/2,
		 setattr/3,
		 foldl/3,
		 foldl_inputs/3,
		 foldl_outputs/3,
		 foldr/3,
		 foldr_inputs/3,
		 foldr_outputs/3,
		 foreach/2,
		 map/2,
		 map_inputs/2,
		 map_outputs/2,
		 input_count/1,
		 output_count/1,
		 to_json/1,
		 to_map/1,
		 bbdef/1,
		 btxdef/1,
		 bblock/1]).

%% Transaction Inputs
-export([index/1,
		 script/1,
		 seqnum/1]).

%% Transaction Outputs
-export([value/1]).

%% A bblock is data structure composed of record
%% with a data field pointing to binary blob
%% representing raw blockchain data
%% an offset field with TX offset data into the blob
%% and a generic meta data map
%% where any offset, i.e txinput, txoutput, version, etc, can 
%% have data structures appended to it. 

%% A binary block is an index prefixed
%% binary blob
%% with a postfix meta data blob where each element that wants
%% to store meta data



bbdef(#bblock{data = <<MagicByte:32/little, _/binary>> = D}=B) ->
	{_, Bbdef, _, _, _} = lib_parse:parse_raw_block(bbdef, MagicByte, D),
	map_attributes(Bbdef, B).

btxdef(#btx{data = D}) -> lib_parse:parse_tx(btxdef, D).

bblock(#bbdef{network = N}=B) ->
	{ok, Bblock} = bblock:native(N, iolist_to_binary(lib_block:serialize(B))),
	%% Now we have to map every attribute in the btxdef to the sparse offset / attribute in bblock
	%% We want to skip adding color or quant values that aren't assigned.
	Bblock#bblock{meta = map_attributes(Bblock, B)}.

calc_offset(#btx{data = D, parent = P}) -> byte_size(P) - byte_size(D);
calc_offset(#binput{data = D, parent = P}) -> byte_size(P) - byte_size(D);
calc_offset(#boutput{data = D, parent = P}) -> byte_size(P) - byte_size(D).

putAttribute(Map, #boutput{}, #btxout{color = ?Uncolored, quantity = 0}) -> Map;
putAttribute(Map, #boutput{} = Out, #btxout{color = C, quantity = Q}) -> 
	maps:put(calc_offset(Out), #{color => C, quantity => Q}, Map).

putAttributeMap(#btx{}, AttrMap, #{}=NewMap) when map_size(NewMap) == 0 -> AttrMap;
putAttributeMap(#btx{}=B, AttrMap, NewMap) -> maps:put(calc_offset(B), NewMap, AttrMap).

%% Right now only Outputs hold attributes
%% Convert a bbdef structure to a bblock map
map_attributes(#bblock{}=Bblock, Bbdef) ->
	{FinalMap, []} = foldl(fun(Btx, {AttrMap, Txs}) ->
				  [CurrentTxDef|T] = Txs,
				  {NewMap, []} = foldl_outputs(fun(Output, {OutputAttrMap, Outputs}) ->
				  					   [CurrentOutput|OT] = Outputs,
									   {putAttribute(OutputAttrMap, Output, CurrentOutput) , OT}
								end, {#{}, CurrentTxDef#btxdef.txoutputs}, Btx), 
				  {putAttributeMap(Btx, AttrMap, NewMap), T}
		  end, {#{}, Bbdef#bbdef.txdata}, Bblock),
	FinalMap;

%% Convert a bblock map to a bbdef structure
%% Fast Path
map_attributes(#bbdef{}=Bbdef, #bblock{meta = M}) when map_size(M) == 0 ->  Bbdef;
%% Sloooow path
map_attributes(#bbdef{}=Bbdef, Bblock) ->
	{NewTxs, []} = foldl(fun(Btx, {Txs, OldTxs}) ->
								 [CurrentTxDef|T] = OldTxs,
								 {NewOutputs, []} = foldl_outputs(fun(Output, {NewOutputs, Outputs}) ->
								 									 [CurrentOutput|Ot] = Outputs,
																	 {[map_attrs(Output, CurrentOutput)|NewOutputs], Ot}
															 end, {[], CurrentTxDef#btxdef.txoutputs}, Btx),
								 {[CurrentTxDef#btxdef{txoutputs = lists:reverse(NewOutputs)}|Txs], T}
						 end, {[], Bbdef#bbdef.txdata}, Bblock),
	Bbdef#bbdef{txdata = lists:reverse(NewTxs)}.

map_attrs(#boutput{meta = M}, Btxout) ->
	Out =  case maps:find(color, M) of
			   {ok, C} -> Btxout#btxout{color = C};
			   error -> Btxout
		   end,
	Out2 = case maps:find(quantity, M) of
			   {ok, Q} -> Out#btxout{quantity = Q};
			   error -> Out
		   end,
	Out2.

to_json(B) -> jiffy:encode(to_map(B)).

to_map(#bblock{}=B) ->
    #{
            hash => json_readable(hash(B)),
            version => version(B),
            merkleroot => json_readable(merkleroot(B)),
            tx => lists:map(fun(E) -> json_readable(E) end, tx_hashes(B)),
            time => timestamp(B),
            nonce => nonce(B),
            difficulty => difficulty(B),
            %% Chainwork would have to query chaind
            previousblockhash => json_readable(previoushash(B))
            %% nextblockhash would have to query chaind
    };

to_map(#btx{data = D}=B) ->
    #{
    	    hex => json_readable(D),
            txid => json_readable(hash(B)),
            version => version(B),
            locktime => locktime(B),
            vin => inputs_to_json(B),
            vout => outputs_to_json(B)
    }.


json_readable(X) -> iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(X))).

inputs_to_json(#btx{}=B) ->
	map_inputs(fun(Input) ->
					   H = hash(Input),
					   case H of
					   	   ?COINBASE ->
					   	   	   #{coinbase => json_readable(script(Input)),
					   	   	   	 sequence => seqnum(Input)};
					   	   _ ->
							   #{txhash => json_readable(H),
							   	 vout => index(Input),
							   	 scriptsig => json_readable(script(Input)),
							   	 sequence => seqnum(Input)}
					   end
					   end, B).

outputs_to_json(#btx{}=B) ->
	{_, O} = foldl_outputs(fun(Output, {Vcounter, Outputs}) ->
								   {Vcounter+1, [#{value => lib_transact:satoshi_to_btc(value(Output)),
								   				   vout => Vcounter,
								   				   scriptPubKey => scriptpubkey_to_json(script(Output)),
								   				   color => color_to_json(getattr(color, Output)),
								   				   quantity => quant_to_json(getattr(quantity, Output))}|Outputs]}
						   end, {0, []}, B),
	O.


scriptpubkey_to_json(Script) ->
	#{hex => json_readable(Script),
	  asm => <<"">>}.

color_to_json(error) -> <<"uncolored">>;
color_to_json(?Uncolored) -> <<"uncolored">>;
color_to_json(C) when is_binary(C) -> lib_color:readable(binary, lib_color:new(C)).

quant_to_json(error) -> 0;
quant_to_json(X) -> X.

%% A native entry, is a block being imported from a native file or network packet

% serialize all bblock/btx/binput/boutput types.
serialize({_, Data, _}) -> Data; %% bblock
serialize({_, Data, _, _}) -> Data. %% everything else

serialize_meta(#bblock{meta = M}) -> serialize_attributes(lists:sort(maps:keys(M)), M).

serialize_attributes(Attributes, Meta) -> serialize_attributes(Attributes, Meta, []).

serialize_attributes([], _Meta, Serialized) -> lists:reverse(Serialized);
serialize_attributes([H|T], Meta, Serialized) ->
	V = maps:get(H, Meta),
	case V of
		X when is_map(X), is_integer(H) ->
			serialize_attributes(T, Meta, [[<<H:32/little>>,
											<<1:8/little>>,
											<<(maps:size(X)):32/little>>,
										   serialize_attributes(lists:sort(maps:keys(X)), X)]|Serialized]);
		Y ->
			serialize_attributes(T, Meta, [serialize_attr(H, Y)|Serialized])
	end.


serialize_attr(color, C) -> [<<2:8>>, C];
serialize_attr(quantity, Quant) -> [<<3:8>>, leb128:encode(Quant, unsigned)].

getattr(Attr, {_, _, _, Meta}) -> maps:find(Attr, Meta);
getattr(Attr, {_, _, Meta}) -> maps:find(Attr, Meta).
setattr(Attr, Value, {A,B,C, Meta}) -> {ok, {A,B,C,maps:put(Attr, Value, Meta)}};
setattr(Attr, Value, {B,C, Meta}) -> {ok, {B,C,maps:put(Attr, Value, Meta)}}.

%% Probably should do a deep copy on meta?
copy(#bblock{data = D,
			 meta = M}) -> #bblock{data = binary:copy(D),
								   meta = M};

copy(#btx{data = D,
		  parent = P,
		  meta = M}) -> #btx{data = binary:copy(D),
		  					 parent = P,
							meta = M}.

%% Merges changed maps in sub elements back into the master
%% block
merge(#bblock{}=B, ModifiedElements) ->
	lists:foldl(fun({_, E_Data, E_Parent, E_Meta},
	 			   #bblock{meta = M}=Block) ->
	 			   	   Offset = byte_size(E_Parent) - byte_size(E_Data),
	 			   	   Block#bblock{meta = maps:put(Offset, E_Meta, M)}
	 		   end, B, ModifiedElements);

%% Same thing but for TXs
merge(#btx{}=B, ModifiedElements) ->
	lists:foldl(fun({_, E_Data, E_Parent, E_Meta},
	 			   #btx{meta = M}=Btx) ->
	 			   	   Offset = byte_size(E_Parent) - byte_size(E_Data),
	 			   	   Btx#btx{meta = maps:put(Offset, E_Meta, M)}
	 		   end, B, ModifiedElements).


bytesize(#bblock{data = D}) -> byte_size(D).

native(#{magicbyte := MB}, BinData) -> native(MB, BinData);

native(MagicByte, BinData) when is_integer(MagicByte) ->
	case extract(MagicByte, BinData) of
		error -> error;
		{ok, <<>>} -> {ok, native(BinData, size(BinData))};
		{ok, Remainder} ->
			{ok, native(BinData, byte_size(BinData) - byte_size(Remainder)), Remainder}
	end;

native(Binary, Size) -> 
	#bblock{data = binary:part(Binary, {0, Size}), meta = #{}}.


parse(#{magicbyte := MB}, BinData) when is_integer(MB) -> parse(MB, BinData);

parse(MagicByte, BinData) ->
	case extract_offsets(MagicByte, BinData) of
		error -> error;
		done -> done;
		{ok, <<>>, Offsets} -> {ok, native(BinData, size(BinData)), Offsets, <<>>};
		{ok, Remainder, Offsets} -> {ok, native(BinData, byte_size(BinData) - byte_size(Remainder)), Offsets, Remainder}
	end.


extract_offsets(_MagicByte, <<>>) -> done;
extract_offsets(MagicByte, <<MagicByte:32/little, _:84/binary, BinRest/binary>>) ->
    [TXCount, Tbin] = read_tx_count(BinRest),
    TxDataOffset = 88 + (byte_size(BinRest) - byte_size(Tbin)),
    [Rest, TxOffsets] = getTransactions_offsets(TXCount, Tbin, TxDataOffset),
	{ok, Rest, lists:reverse(TxOffsets)};
extract_offsets(_MagicByte, InvalidHeaderBinary) -> {scan, InvalidHeaderBinary}.

getTransactions_offsets(TXCount, Tbin, StartOffset) -> getTransactions_offsets(TXCount, Tbin, [], StartOffset).
getTransactions_offsets(0, Tbin, Offsets, _StartOffset) -> [Tbin, Offsets];
getTransactions_offsets(TXCount, Tbin, Offsets, StartOffset) ->
	<< _TransactionVersion:32/little, Rest/binary >> = Tbin,
	EndTx = read_outputs(read_inputs(Rest)),
    <<_TransactionLockTime:32/little, Next/binary>> = EndTx,
    TXLength = byte_size(Tbin) - byte_size(Next),
    TransactionHash = libsecp256k1:dsha256(binary:part(Tbin, {0,TXLength})),
	getTransactions_offsets(TXCount-1, Next, 
							[{TransactionHash, StartOffset, TXLength}|Offsets], StartOffset + TXLength).

%% lookup methods

hash(#bblock{data = <<_:8/binary, H:80/binary, _/binary>>}) -> libsecp256k1:dsha256(H);

hash(#btx{data = D}) -> 
	EndTx = getTransaction(D),
	libsecp256k1:dsha256(binary:part(D, {0, byte_size(D) - byte_size(EndTx)}));

hash(#binput{data = <<Txhash:32/binary, _/binary>>}) -> Txhash.

version(#bblock{data = <<_:64, Version:32/little, _/binary>>}) -> Version;
version(#btx{data = <<Version:32/little, _/binary>>}) -> Version.

previoushash(#bblock{data = <<_:12/binary, Previous:32/binary, _/binary>>}) -> Previous.
merkleroot(#bblock{data = <<_:44/binary, Merkle:32/binary, _/binary>>}) -> Merkle.
timestamp(#bblock{data = <<_:76/binary, Timestamp:32/little, _/binary>>}) -> Timestamp.
difficulty(#bblock{data = <<_:80/binary, Difficulty:32/little, _/binary>>}) -> Difficulty.
nonce(#bblock{data = <<_:84/binary, Nonce:32/little, _/binary>>}) -> Nonce.

locktime(#btx{data = <<_32/little, Rest/binary>>}) ->
	<<NLocktime:32/little, _/binary>> = read_outputs(read_inputs(Rest)),
	NLocktime.

%% INPUTS
index(#binput{data = <<_:32/binary, I:32/little, _/binary>>}) -> I.

script(#binput{data = <<_:36/binary, Rest/binary>>}) ->
	[Length, Rest2] = lib_parse:getVarInt(Rest),
	<< Script:Length/binary, _/binary >> = Rest2,
	Script;

script(#boutput{data = <<_:64/little, Rest/binary>>}) ->
	[ScriptLength, Rest2] = lib_parse:getVarInt(Rest),
	<<Script:ScriptLength/binary, _Next/binary >> = Rest2,
	Script.

seqnum(#binput{data = <<_:36/binary, Rest/binary>>}) ->
	[Length, Rest2] = lib_parse:getVarInt(Rest),
	<< _:Length/binary, SeqNum:32/little, _/binary >> = Rest2,
	SeqNum.


%% OUTPUTS
%%

value(#boutput{data = <<V:64/little, _/binary>>}) -> V.

tx(#bblock{data = <<_:88/binary, D/binary>>=B, meta = M}, Index) ->
	[_TXCount, Tbin] = read_tx_count(D),
	Rest = getTransactions(Index-1, Tbin),
	#btx{data = Rest,
		 parent = B,
		 meta = get_meta(B, Rest, M)}.

tx_hashes(#bblock{} = B) -> map(fun(Btx) -> hash(Btx) end, B).

txs(#bblock{} = B) -> map(fun(Btx) -> Btx end, B).

inputs(#btx{} = Btx) -> foldl_inputs(fun(Binput, Acc) -> [Binput|Acc] end, [], Btx).

outputs(#btx{} = Btx) -> foldl_outputs(fun(Boutput, Acc) -> [Boutput|Acc] end, [], Btx).

input(#btx{data = <<_:32, D/binary>>=B, meta = M}, Index) ->
	[_, D2] = read_input_count(D),
	InterestingInputStart = getTxInputs(Index-1, D2),
	#binput{data = InterestingInputStart,
			parent = B,
			meta = get_meta(B, InterestingInputStart, M)}.

output(#btx{data = <<_:32, D/binary>>=B, meta = M}, Index) ->
	Next = read_inputs(D),
	[_, OutputStart] = read_output_count(Next),
	InterestingOutputStart = getTxOutputs(Index-1, OutputStart),
	#boutput{data = InterestingOutputStart,
			 parent = B,
			 meta = get_meta(B, InterestingOutputStart, M)}.

get_meta(ParentBin, ChildBin, M) ->
	Offset = byte_size(ParentBin) - byte_size(ChildBin),
	case maps:find(Offset, M) of
		error -> #{};
		{ok, MetaData} -> MetaData
	end.

data(#bblock{data=D}) -> D;
data(#btx{data=D}) -> D.

meta(#bblock{meta=M}) -> M;
meta(#btx{meta=M}) -> M.

offset({_, _, O, _}) -> O.

%% 16 bytes per offset entry in a bblock
tx_count(#bblock{data = <<_:88/binary, D/binary>>}) ->
	[Count, _] = read_tx_count(D),
	Count.

input_count(#btx{data = <<_:32, D/binary>>}) ->
	[Count, _] = read_input_count(D),
	Count.

output_count(#btx{data = <<_:32, D/binary>>}) ->
	[Count, _Rest] = read_output_count(read_inputs(D)),
	Count.

%% Fast Native parsing

extract(MagicByte, <<MagicByte:32/little, _:84/binary, BinRest/binary>>) ->
    [TXCount, Tbin] = read_tx_count(BinRest),
	{ok, getTransactions(TXCount, Tbin)};

extract(_MagicByte, _Data) ->  error.

read_tx_count(Tbin) -> lib_parse:getVarInt(Tbin).

getTransactions(X, Tbin) when X < 1 -> Tbin;
getTransactions(TXCount, Tbin) ->
	Next = getTransaction(Tbin),
	getTransactions(TXCount-1, Next).

getTransaction(<<_TransactionVersion:32/little, Rest/binary>>) ->
	Rest2 = read_outputs(read_inputs(Rest)),
	%% Seqnum
    <<_:32/little, Next/binary>> = Rest2,
    Next.

read_input_count(Tbin) -> lib_parse:getVarInt(Tbin).

read_inputs(Tbin) ->
	[InputCount, Inputs] = read_input_count(Tbin),
	getTxInputs(InputCount, Inputs).

read_output_count(Tbin) -> lib_parse:getVarInt(Tbin).

read_outputs(Tbin) ->
	[OutputCount, Outputs] = read_output_count(Tbin),
	getTxOutputs(OutputCount, Outputs).

getTxInputs(0, Rest) -> Rest;
getTxInputs(InputCount, Rest) ->
	   {Next,_,_,_,_} = getInput(Rest),
	   getTxInputs(InputCount-1, Next).

getInput(<<Txhash:32/binary, TxIndex:32/little, BinRest/binary>>) ->
	   [ScriptLength, TxRest] = lib_parse:getVarInt(BinRest),
	   << Script:ScriptLength/binary, SeqNum:32/little, Next/binary >> = TxRest,
	   {Next, Txhash, TxIndex, Script, SeqNum}.

getTxOutputs(0, Rest) -> Rest;
getTxOutputs(OutputCount, Rest) ->
	{Next, _, _} = getOutput(Rest),
	getTxOutputs(OutputCount-1, Next).

getOutput(<<Value:64/little, BinRest/binary>>) ->
	[ScriptLength, TxRest] = lib_parse:getVarInt(BinRest),
	<<Script:ScriptLength/binary, Next/binary >> = TxRest,
	{Next, Value, Script}.


%% Foldl / foreach / map /etc

%% Iterate over Transactions

%% Iterate over Transactions
foldl(FoldFun, StartAcc, #bblock{data = <<_:88/binary, D/binary>>=B, meta = M}) ->
	[TxCount, Txbin] = read_tx_count(D),
	next_tx(TxCount, B, Txbin, M, FoldFun, StartAcc).

%% Iterate over Inputs
foldl_inputs(FoldFun, StartAcc, #btx{data = <<_:32, D/binary>> = T, meta = M}) ->
	[InputCount, Inputbin] = read_input_count(D),
	next_input(InputCount, T, Inputbin, M, FoldFun, StartAcc).


foldl_outputs(FoldFun, StartAcc, #btx{data = <<_:32, D/binary>> = T, meta = M}) ->
	Next = read_inputs(D),
	[OutputCount, Outputbin] = read_output_count(Next),
	next_output(OutputCount, T, Outputbin, M, FoldFun, StartAcc);


%% Iterate over the Outputs using the last returned input as reference
foldl_outputs(FoldFun, StartAcc, #binput{data = D, meta = M}) ->
	Next = getTxInputs(1, D),
	[OutputCount, Outputbin] = read_output_count(Next),
	next_output(OutputCount, D, Outputbin, M, FoldFun, StartAcc).


foreach(EachFun, #bblock{}=B) -> foldl(fun(Btx, _) -> EachFun(Btx), ok end, ok, B).

map(MapFun, #bblock{}=B) ->
	lists:reverse(foldl(fun(Btx, Acc) -> [MapFun(Btx)|Acc] end, [], B)).

map_inputs(MapFun, #btx{}=B) ->
	lists:reverse(foldl_inputs(fun(Input, Acc) -> [MapFun(Input)|Acc] end, [], B)).

map_outputs(MapFun, #btx{}=B) ->
	lists:reverse(foldl_outputs(fun(Output, Acc) -> [MapFun(Output)|Acc] end, [], B)).

parse_tx(Bin) -> next_tx(1, Bin, Bin, #{}, fun(Btx, _) -> Btx end, ok).

next_tx(0, _, _, _, _, Acc) -> Acc;
next_tx(Count, BlockBin, CurrentBin, Meta, Fun, Acc) ->
	Rest = getTransactions(1, CurrentBin),
	Btx = #btx{data = CurrentBin,
			   parent = BlockBin,
			   meta = get_meta(BlockBin, CurrentBin, Meta)},
	Acc2 = Fun(Btx, Acc),
	next_tx(Count-1, BlockBin, Rest, Meta, Fun, Acc2).

next_input(0, _, _, _, _, Acc) -> Acc;
next_input(Count, ParentBin, CurrentBin, Meta, Fun, Acc) ->
	Rest = getTxInputs(1, CurrentBin),
	Input = #binput{data = CurrentBin,
					parent = ParentBin,
					meta = get_meta(ParentBin, CurrentBin, Meta)},
	Acc2 = Fun(Input, Acc),
	next_input(Count-1, ParentBin, Rest, Meta, Fun, Acc2).

next_output(0, _, _, _, _, Acc) -> Acc;
next_output(Count, ParentBin, CurrentBin, Meta, Fun, Acc) ->
	Rest = getTxOutputs(1, CurrentBin),
	Output = #boutput{data = CurrentBin,
					  parent = ParentBin, 
					  meta = get_meta(ParentBin, CurrentBin, Meta)},
	Acc2 = Fun(Output, Acc),
	next_output(Count-1, ParentBin, Rest, Meta, Fun, Acc2).


% Reverse foldr operations (are expensive)
% Block structure is forward tracking

foldr(FoldFun, StartAcc, #bblock{}=B) ->
	%% Build a forward TX binary list
	lists:foldl(FoldFun, StartAcc, lists:reverse(txs(B))).

foldr_inputs(FoldFun, StartAcc, #btx{}=Tx) ->
	lists:foldl(FoldFun, StartAcc, lists:reverse(inputs(Tx))).

foldr_outputs(FoldFun, StartAcc, #btx{}=Tx) ->
	lists:foldl(FoldFun, StartAcc, lists:reverse(outputs(Tx))).


