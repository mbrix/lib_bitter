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
		 header/1,
		 serialize/1,
		 serialize_meta/1,
		 deserialize_meta/1,
		 apply_meta/2,
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
		 magicbyte/1,
		 headerlength/1,
		 input/2,
		 output/2,
		 tx_count/1,
		 merge/2,
		 data/1,
		 offset/1,
		 coinbase/1,
		 meta/1,
		 copy/1,
		 bytesize/1,
		 getattr/2,
		 getattr/3,
		 setattr/3,
		 delattr/2,
		 foldl/3,
		 foldl_inputs/3,
		 foldl_outputs/3,
		 inputs_outputs/1,
		 match_outputs/2,
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
		 bblock/1,
		 info/1,
		 address/1,
		 output_set/1,
		 strip_output/2,
		 strip_meta/1,
		 find_opreturn/1,
		 compress_output/1
		]).

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


%print_meta(B) ->
%	foldl(fun(Btx, _) ->
%	                    foldl_outputs(fun(Output, _) ->
%	                             ?debugFmt("X: ~p~n", [Output#boutput.offset])
%                         end, ok, Btx)
%                 end, ok, B).
%

putAttribute(Map, #boutput{}, #btxout{attributes = A}) when map_size(A) == 0 -> Map;
putAttribute(Map, #boutput{offset = O}, #btxout{attributes = A}) -> 
	maps:put(O, A, Map).

putAttributeMap(#btx{}, AttrMap, #{}=NewMap) when map_size(NewMap) == 0 -> AttrMap;
putAttributeMap(#btx{offset = O}, AttrMap, NewMap) -> maps:put(O, NewMap, AttrMap).


add_basic_attributes(Map, B) ->
	M2 = sparse_add(Map, e_height, B#bbdef.e_height, 1),
	M3 = sparse_add(M2, e_sumdiff, B#bbdef.e_sumdiff, 0),
	sparse_add(M3, e_next, B#bbdef.e_next, undefined).

sparse_add(Map, _Name, A, A) -> Map;
sparse_add(Map, Name, A, _) -> maps:put(Name, A, Map).

%% Right now only Outputs hold attributes
%% Convert a bbdef structure to a bblock map
map_attributes(#bblock{}=Bblock, Bbdef) ->
	{FinalMap, []} = foldl(fun(Btx, {AttrMap, Txs}) ->
				  [CurrentTxDef|T] = Txs,
				  {NewMap, []} = foldl_outputs(fun(Output, {OutputAttrMap, Outputs}) ->
				  					   [CurrentOutput|OT] = Outputs,
				  					   ?assertEqual(CurrentOutput#btxout.script, script(Output)),
								 	   ?assertEqual(CurrentOutput#btxout.txindex, index(Output)),
									   {putAttribute(OutputAttrMap, Output, CurrentOutput) , OT}
								end, {#{}, CurrentTxDef#btxdef.txoutputs}, Btx), 
				  {putAttributeMap(Btx, AttrMap, NewMap), T}
		  end, {#{}, Bbdef#bbdef.txdata}, Bblock),
	add_basic_attributes(FinalMap, Bbdef);

%% Convert a bblock map to a bbdef structure
%% Fast Path
map_attributes(#bbdef{}=Bbdef, #bblock{meta = M}) when map_size(M) == 0 ->  Bbdef;
%% Sloooow path
map_attributes(#bbdef{}=Bbdef, Bblock) ->
	{NewTxs, []} = foldl(fun(Btx, {Txs, OldTxs}) ->
								 [CurrentTxDef|T] = OldTxs,
								 {NewOutputs, []} = foldl_outputs(fun(Output, {NewOutputs, Outputs}) ->
								 									 [CurrentOutput|Ot] = Outputs,
								 									 ?assertEqual(CurrentOutput#btxout.script,
								 									              script(Output)),
								 									 ?assertEqual(CurrentOutput#btxout.txindex,
								 									              index(Output)),
																	 {[map_attrs(Output, CurrentOutput)|NewOutputs], Ot}
															 end, {[], CurrentTxDef#btxdef.txoutputs}, Btx),
								 {[CurrentTxDef#btxdef{txoutputs = lists:reverse(NewOutputs)}|Txs], T}
						 end, {[], Bbdef#bbdef.txdata}, Bblock),
	B2 = Bbdef#bbdef{txdata = lists:reverse(NewTxs)},
	B2#bbdef{e_sumdiff = add_attr(maps:find(e_sumdiff, Bblock#bblock.meta), 0),
			 e_height  = add_attr(maps:find(e_height, Bblock#bblock.meta), 1),
			 e_next    = add_attr(maps:find(e_next, Bblock#bblock.meta), undefined)}.

add_attr({ok, A}, _) -> A;
add_attr(error, Default) -> Default.

map_attrs(#boutput{meta = M}, Btxout) when map_size(M) < 1 -> Btxout;
map_attrs(#boutput{meta = M}, Btxout) -> 
    Btxout#btxout{attributes = M}.

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

%% OP return

find_opreturn([]) -> false;
find_opreturn([H|T]) ->
    case is_opreturn(info(H)) of
        false -> find_opreturn(T);
        Result -> Result
    end;

find_opreturn(Btx) ->
    try
        foldl_outputs(fun(Output, _) ->
                              case is_opreturn(info(Output)) of 
                                  false -> false;
                                  Result -> throw(Result)
                              end
                              end, ok, Btx)
    catch
        throw:Result -> Result
    end.

is_opreturn({openassets, Data}) -> {openassets, Data};
is_opreturn({op_return, Data}) -> {op_return, Data};
is_opreturn(_) -> false.

%% A native entry, is a block being imported from a native file or network packet

% serialize all bblock/btx/binput/boutput types.
serialize({_, Data, _, _}) -> Data; %% bblock
serialize({_, Data, _, _, _}) -> Data. %% everything else

serialize_meta(#boutput{meta = M}) when map_size(M) == 0 -> <<>>;
serialize_meta(#boutput{meta = M}) -> erlang:term_to_binary(M);
serialize_meta(#bblock{meta = M}) when map_size(M) == 0 -> <<>>;
serialize_meta(#bblock{meta = M}) -> erlang:term_to_binary(M);
serialize_meta(#btx{meta = M}) when map_size(M) == 0 -> <<>>;
serialize_meta(#btx{meta = M}) -> erlang:term_to_binary(M).

deserialize_meta(<<>>) -> #{};
deserialize_meta(MetaBin) -> erlang:binary_to_term(MetaBin).

apply_meta(#bblock{}=B, MetaBin) -> B#bblock{meta = deserialize_meta(MetaBin)};
apply_meta(#btx{}=B, MetaBin) -> B#btx{meta = deserialize_meta(MetaBin)}.

getattr(Attr, {_, _, _, _, Meta}) -> maps:find(Attr, Meta);
getattr(Attr, {_, _, _, Meta}) -> maps:find(Attr, Meta).

getattr(Attr, Thing, Default) ->
	case getattr(Attr, Thing) of
		error -> Default;
		AttributeValue -> AttributeValue
	end.

setattr(Attr, Value, {A,B,C,D,Meta}) -> {ok, {A,B,C,D,maps:put(Attr, Value, Meta)}};
setattr(Attr, Value, {B,C,D, Meta}) -> {ok, {B,C,D,maps:put(Attr, Value, Meta)}}.

delattr(Attr, {A,B,C,D,Meta}) -> {ok, {A,B,C,D,maps:remove(Attr, Meta)}};
delattr(Attr, {B,C,D,Meta}) -> {ok, {B,C,D,maps:remove(Attr, Meta)}}.

%% Probably should do a deep copy on meta?
copy(#bblock{data = D,
			 meta = M}) -> #bblock{data = binary:copy(D),
								   meta = M};

copy(#btx{data = D,
          offset = O,
		  meta = M}) -> #btx{data = binary:copy(D),
                             offset = O,
                             meta = M}.

%% Merges changed maps in sub elements back into the master
%% block
merge(#bblock{}=B, ModifiedElements) ->
	lists:foldl(fun({_, _E_Data, E_Offset, _Ext, E_Meta}, #bblock{meta = M}=Block) ->
						case maps:size(E_Meta) of
							0 -> Block;
							_ -> Block#bblock{meta = maps:put(E_Offset, E_Meta, M)}
						end
	 		   end, B, ModifiedElements);

%% Same thing but for TXs
merge(#btx{}=B, ModifiedElements) ->
	lists:foldl(fun({_, _E_Data, E_Offset, _Ext, E_Meta}, #btx{meta = M}=Btx) ->
						case maps:size(E_Meta) of
							0 -> Btx;
							_ -> Btx#btx{meta = maps:put(E_Offset, E_Meta, M)}
						end
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

header(Data) -> #bblock{data = Data}.


parse(#{magicbyte := MB}, BinData) when is_integer(MB) -> parse(MB, BinData);

parse(MagicByte, BinData) ->
	case extract_offsets(MagicByte, BinData) of
		error -> error;
		done -> done;
	    {scan, Next} -> {scan, Next};
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

hash(#btx{data = D}) -> libsecp256k1:dsha256(D);

hash(#binput{data = <<Txhash:32/binary, _/binary>>}) -> Txhash;

hash(#btxin{txhash = H}) -> H;

hash(#utxop{hash_index = {H,_}}) -> H.

coinbase(#binput{data = <<C:32/binary, _/binary>>}) ->
	case C of ?COINBASE -> true;
			  _ -> false
	end;
coinbase(#binput{}) -> false.

version(#bblock{data = <<_:64, Version:32/little, _/binary>>}) -> Version;
version(#btx{data = <<Version:32/little, _/binary>>}) -> Version.

previoushash(#bblock{data = <<_:12/binary, Previous:32/binary, _/binary>>}) -> Previous.
merkleroot(#bblock{data = <<_:44/binary, Merkle:32/binary, _/binary>>}) -> Merkle.
timestamp(#bblock{data = <<_:76/binary, Timestamp:32/little, _/binary>>}) -> Timestamp.
difficulty(#bblock{data = <<_:80/binary, Difficulty:32/little, _/binary>>}) -> Difficulty.
nonce(#bblock{data = <<_:84/binary, Nonce:32/little, _/binary>>}) -> Nonce.

magicbyte(#bblock{data = <<M:32/little, _/binary>>}) -> M.

headerlength(#bblock{data = <<_:32, L:32/little, _/binary>>}) -> L. 

locktime(#btx{data = <<_32/little, Rest/binary>>}) ->
	<<NLocktime:32/little, _/binary>> = read_outputs(read_inputs(Rest)),
	NLocktime.

%% INPUTS
index(#binput{data = <<_:32/binary, I:32/little, _/binary>>}) -> I;
index(#boutput{ext = E}) -> maps:get(index, E);
index(#btxin{txindex=I})->I;
index(#utxop{hash_index = {_, I}}) -> I.

script(#binput{data = <<_:36/binary, Rest/binary>>}) ->
	[Length, Rest2] = lib_parse:getVarInt(Rest),
	<< Script:Length/binary, _/binary >> = Rest2,
	Script;

script(#boutput{data = <<_:64/little, Rest/binary>>}) ->
	[ScriptLength, Rest2] = lib_parse:getVarInt(Rest),
	<<Script:ScriptLength/binary, _Next/binary >> = Rest2,
	Script;

script(#utxop{script = S}) -> S.

seqnum(#binput{data = <<_:36/binary, Rest/binary>>}) ->
	[Length, Rest2] = lib_parse:getVarInt(Rest),
	<< _:Length/binary, SeqNum:32/little, _/binary >> = Rest2,
	SeqNum.


%% OUTPUTS
%%

value(#boutput{data = <<V:64/little, _/binary>>}) -> V.

info(#btxout{}=Out) -> Out#btxout.info;
info(#boutput{}=Out) -> lib_parse:parse_script(script(Out)).

address(#boutput{}=Out) -> lib_address:script_to_adddress(info(Out), script(Out));
address(#btxout{address = A}) -> A.

tx(#bblock{data = <<_:88/binary, D/binary>>=B, meta = M}, Index) ->
	[_TXCount, Tbin] = read_tx_count(D),
	Rest = getTransactions(Index, Tbin),
	Offset = byte_size(B) - byte_size(Rest),
	EndTx = getTransactions(1, Rest),
	#btx{data = binary:part(Rest, {0, size(Rest) - size(EndTx)}),
         offset = Offset,
		 meta = get_meta(Offset, M)}.

tx_hashes(#bblock{} = B) -> map(fun(Btx) -> hash(Btx) end, B).

txs(#bblock{} = B) -> map(fun(Btx) -> Btx end, B).

inputs(#btx{} = Btx) -> lists:reverse(foldl_inputs(fun(Binput, Acc) -> [Binput|Acc] end, [], Btx)).

outputs(#btx{} = Btx) -> lists:reverse(foldl_outputs(fun(Boutput, Acc) -> [Boutput|Acc] end, [], Btx));
outputs(#btxdef{} = Tx) -> Tx#btxdef.txoutputs.

input(#btx{data = <<_:32, D/binary>>=B, offset = O, meta = M}, Index) ->
	[_, D2] = read_input_count(D),
	InterestingInputStart = getTxInputs(Index, D2),
	Offset = O + (byte_size(B) - byte_size(InterestingInputStart)),
	#binput{data = InterestingInputStart,
            offset = Offset,
			meta = get_meta(Offset, M)}.

output(#btx{data = <<_:32, D/binary>>=B, meta = M}, Index) ->
	Next = read_inputs(D),
	[_, OutputStart] = read_output_count(Next),
	InterestingOutputStart = getTxOutputs(Index, OutputStart),
	Offset = byte_size(B) - byte_size(InterestingOutputStart),
	#boutput{data = InterestingOutputStart,
             offset = Offset,
			 ext = #{index => Index},
			 meta = get_meta(Offset, M)}.

get_meta(Offset, M) ->
	case maps:find(Offset, M) of
		error -> #{};
		{ok, MetaData} -> MetaData
	end.

data(#bblock{data=D}) -> D;
data(#btx{data=D}) -> D.

meta(#bblock{meta=M}) -> M;
meta(#btx{meta=M}) -> M.

%% Converts a BTX into a BTX without inputs
output_set(#btx{data = <<N:32, D/binary>>, offset = O, meta = M}) ->
    Start = read_inputs(D),
    End = read_outputs(Start),
    <<SeqNum:32/little, _/binary>> = End,
    #btx{data = <<N:32, 0:8, (binary:part(Start, {0, size(Start) - size(End)}))/binary, SeqNum:32/little>>,
         offset = O,
         meta = M}.

%% Replaces an output with a null output
strip_output(#btx{data = <<F:4/binary, D/binary>>}=B, Index) ->
	Next = read_inputs(D),
	[_, Outputbin] = read_output_count(Next),
	strip_next_output(B, F, D, Index, Outputbin, byte_size(D) - byte_size(Outputbin)).

strip_meta(#bblock{}=B) -> B#bblock{meta = #{}};
strip_meta(#btx{}=B) -> B#btx{meta = #{}};
strip_meta(#boutput{}=B) -> B#boutput{meta = #{}}.

strip_next_output(B, F, D, 0, Rest, PreSize) -> 
        Next = getTxOutputs(1, Rest),
        TxSize = byte_size(Rest) - byte_size(Next),
        B#btx{data = iolist_to_binary([F,
                                       binary:part(D, {0, PreSize}),
                                       <<0:64, 0:8>>,
                                       binary:part(D, {PreSize+TxSize, byte_size(Next)})])};

strip_next_output(B, F, D, Index, Rest, PreSize) ->
	Next = getTxOutputs(1, Rest),
	Size = byte_size(Rest) - byte_size(Next),
	strip_next_output(B, F,D, Index-1, Next, PreSize+Size).

offset({_, D, P, _, _}) -> byte_size(P) - byte_size(D).

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

getTransactions(0, Tbin) -> Tbin;
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
	StartOffset = byte_size(B) - byte_size(Txbin),
	next_tx(TxCount, B, Txbin, StartOffset, M, FoldFun, StartAcc).

%% Iterate over Inputs
foldl_inputs(FoldFun, StartAcc, #btx{data = <<_:32, D/binary>> = T, meta = M}) ->
	[InputCount, Inputbin] = read_input_count(D),
	StartOffset = (byte_size(T) - byte_size(Inputbin)),
    {_, Inputs} = next_input(InputCount, T, Inputbin, StartOffset, M, FoldFun, StartAcc),
    Inputs.


foldl_outputs(FoldFun, StartAcc, #btx{data = <<_:32, D/binary>> = T, meta = M}) ->
	Next = read_inputs(D),
	[OutputCount, Outputbin] = read_output_count(Next),
	StartOffset = byte_size(T) - byte_size(Outputbin),
	next_output(OutputCount, OutputCount, T, Outputbin, StartOffset, M, FoldFun, StartAcc);


%% Iterate over the Outputs using the last returned input as reference
foldl_outputs(FoldFun, StartAcc, #binput{data = D, meta = M}) ->
	Next = getTxInputs(1, D),
	[OutputCount, Outputbin] = read_output_count(Next),
	StartOffset = (byte_size(D) - byte_size(Outputbin)),
	next_output(OutputCount, OutputCount, D, Outputbin, StartOffset, M, FoldFun, StartAcc).

inputs_outputs(#btx{data = <<_:32, D/binary>> = T, meta = M}) ->
    FoldFun = fun(Object,Acc) -> [Object|Acc] end,
    [InputCount, Inputbin] = read_input_count(D),
	StartOffset = (byte_size(T) - byte_size(Inputbin)),
    {Next, Inputs} = next_input(InputCount, T, Inputbin, StartOffset, M, FoldFun, []),
	[OutputCount, Outputbin] = read_output_count(Next),
	OutputStartOffset = (byte_size(T) - byte_size(Outputbin)),
	Outputs= next_output(OutputCount, OutputCount, D, Outputbin, OutputStartOffset, M, FoldFun, []),
    {lists:reverse(Inputs), lists:reverse(Outputs)}.


match_outputs(Btx, MatchFun) ->
    try
        foldl_outputs(fun(Output, _) ->
                              case MatchFun(Output) of
                                  false -> false;
                                  true -> throw(matched)
                              end
                      end, ok, Btx)
    catch
        throw:_ -> true
    end.

foreach(EachFun, #bblock{}=B) -> foldl(fun(Btx, _) -> EachFun(Btx), ok end, ok, B).

map(MapFun, #bblock{}=B) ->
	lists:reverse(foldl(fun(Btx, Acc) -> [MapFun(Btx)|Acc] end, [], B)).

map_inputs(MapFun, #btx{}=B) ->
	lists:reverse(foldl_inputs(fun(Input, Acc) -> [MapFun(Input)|Acc] end, [], B)).

map_outputs(MapFun, #btx{}=B) ->
	lists:reverse(foldl_outputs(fun(Output, Acc) -> [MapFun(Output)|Acc] end, [], B)).

parse_tx(Bin) -> next_tx(1, Bin, Bin, 0, #{}, fun(Btx, _) -> Btx end, ok).

next_tx(0, _, _, _, _, _, Acc) -> Acc;
next_tx(Count, BlockBin, CurrentBin, StartOffset, Meta, Fun, Acc) ->
	Rest = getTransactions(1, CurrentBin),
	Size = byte_size(CurrentBin) - byte_size(Rest),
	NextOffset = StartOffset + Size,
	Btx = #btx{data = binary:part(CurrentBin, {0, Size}),
               offset = StartOffset,
			   meta = get_meta(StartOffset, Meta)},
	Acc2 = Fun(Btx, Acc),
	next_tx(Count-1, BlockBin, Rest, NextOffset, Meta, Fun, Acc2).

next_input(0, _, CurrentBin, _, _, _, Acc) -> {CurrentBin, Acc};
next_input(Count, ParentBin, CurrentBin, StartOffset, Meta, Fun, Acc) ->
	Rest = getTxInputs(1, CurrentBin),
	Size = byte_size(CurrentBin) - byte_size(Rest),
	NextOffset = StartOffset + Size,
	Input = #binput{data = binary:part(CurrentBin, {0, Size}),
                    offset = StartOffset,
					meta = get_meta(StartOffset, Meta)},
	Acc2 = Fun(Input, Acc),
	next_input(Count-1, ParentBin, Rest, NextOffset, Meta, Fun, Acc2).

next_output(0, _, _, _, _, _, _, Acc) -> Acc;
next_output(Count, TotalCount, ParentBin, CurrentBin, StartOffset, Meta, Fun, Acc) ->
	Rest = getTxOutputs(1, CurrentBin),
	Size = byte_size(CurrentBin) - byte_size(Rest),
	NextOffset = StartOffset + Size,
	Output = #boutput{data = binary:part(CurrentBin, {0, Size}),
                      offset = StartOffset, 
					  ext = #{index => TotalCount-Count},
					  meta = get_meta(StartOffset, Meta)},
	Acc2 = Fun(Output, Acc),
	next_output(Count-1, TotalCount, ParentBin, Rest, NextOffset, Meta, Fun, Acc2).


% Reverse foldr operations (are expensive)
% Block structure is forward tracking

foldr(FoldFun, StartAcc, #bblock{}=B) ->
	%% Build a forward TX binary list
	lists:foldl(FoldFun, StartAcc, lists:reverse(txs(B))).

foldr_inputs(FoldFun, StartAcc, #btx{}=Tx) ->
	lists:foldl(FoldFun, StartAcc, lists:reverse(inputs(Tx))).

foldr_outputs(FoldFun, StartAcc, #btx{}=Tx) ->
	lists:foldl(FoldFun, StartAcc, lists:reverse(outputs(Tx))).



%% Output compression for most common types
compress_output(O) -> do_compress_output(O, leb128:encode(value(O), unsigned), info(O)).

do_compress_output(_O, V, {p2pkh, Pubkey}) -> <<V/binary, Pubkey/binary>>;
do_compress_output(_, V, {p2pkh2, Pubkey}) -> <<1:8, V/binary, Pubkey/binary>>;
do_compress_output(_, V, {p2sh, 1, Hash}) -> <<2:8, V/binary, Hash/binary>>;
do_compress_output(_, V, {p2sh, 2, Hash}) -> <<3:8, V/binary, Hash/binary>>;
do_compress_output(O, _, _) -> <<2:8, (serialize(O))/binary>>.
