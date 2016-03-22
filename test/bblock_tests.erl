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

-module(bblock_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	btr_net_params:init(main),
	ok.

stop(_) ->
	ok.

native() ->
	?assertMatch({ok, _}, bblock:native(btr_net_params:params(), rawblock())).

counts() ->
	{ok, B} = bblock:native(btr_net_params:params(), rawblock()),
	?assertEqual(1, bblock:tx_count(B)),
	Tx = bblock:tx(B, 0),
	?assertEqual(1, bblock:input_count(Tx)),
	?assertEqual(1, bblock:output_count(Tx)).

hash() ->
	{ok, B} = bblock:native(btr_net_params:params(), rawblock()),
	CorrectBlockHash = lib_block:from_string("00000000e218d46cb2ebea79906e0052ae1c5bb6b224b0da3bcee63ea53e33d0"),
	CorrectTxHash = lib_block:from_string("aa653511d368baae692060b2554ddbfbf1efbd49167cb25c8213f327d4362d4e"),
	?assertEqual(CorrectBlockHash, bblock:hash(B)),
	Tx = bblock:tx(B, 0),
	?assertEqual(CorrectTxHash, bblock:hash(Tx)).


txs() ->
	{ok, B} = bblock:native(btr_net_params:params(), rawblock()),
	[BTx|_] = bblock:txs(B),
	CorrectTxHash = lib_block:from_string("aa653511d368baae692060b2554ddbfbf1efbd49167cb25c8213f327d4362d4e"),
	?assertEqual(CorrectTxHash, bblock:hash(BTx)).

inputs() ->
	{ok, B} = bblock:native(btr_net_params:params(), rawblock()),
	[BTx|_] = bblock:txs(B),
	Inputs = bblock:inputs(BTx),
	?assertEqual(1, length(Inputs)).

outputs() ->
	{ok, B} = bblock:native(btr_net_params:params(), rawblock()),
	[BTx|_] = bblock:txs(B),
	Outputs = bblock:outputs(BTx),
	?assertEqual(1, length(Outputs)).

many_blocks() ->
	%% Lets parse bigger more complicated blocks into bblocks
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    ?assertMatch({ok, _}, bblock:native(btr_net_params:params(), RawBlock)),
    RawChunk = iolist_to_binary([RawBlock, RawBlock, RawBlock, RawBlock]),
	{ok, _B2, Next} = bblock:native(btr_net_params:params(), RawChunk),
	{ok, _B3, Next2} = bblock:native(btr_net_params:params(), Next),
	{ok, _B4, _Next3} = bblock:native(btr_net_params:params(), Next2).


meta_serialization() ->
	Meta = #{color => ?Uncolored,
			 quantity => 2000},
	MetaBlock = #bblock{meta = Meta},
	Ser = iolist_to_binary(bblock:serialize_meta(MetaBlock)),
	?assertEqual(Meta, bblock:deserialize_meta(Ser)).
	%MetaBlock2 = #bblock{meta = #{10 => #{color => ?Uncolored,
	%									  quantity => 2000},
	%							  20 => #{color => crypto:rand_bytes(32)}}},
	%_Ser2 = bblock:serialize_meta(MetaBlock2),
	%NestedMeta = #bblock{meta = #{10 => #{20 => #{color => ?Uncolored,
	%											  quantity => 100}
	%									 }
	%							 }
	%					},
	%_Ser3 = bblock:serialize_meta(NestedMeta).

parse_integration() ->
	%% Test integration with lib_parse
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
	{continue, Bbdef, _, _, _} = lib_parse:parse_raw_block(bbdef, btr_net_params:params(), RawBlock),
	{continue, Bblock, _, _, _} = lib_parse:parse_raw_block(bblock, btr_net_params:params(), RawBlock),
	{ok, Bblock2} = bblock:native(btr_net_params:params(), RawBlock),
    ?assertEqual(Bbdef, bblock:bbdef(Bblock)),
    ?assertEqual(Bbdef, bblock:bbdef(Bblock2)),
	?assertEqual(Bbdef#bbdef.version, bblock:version(Bblock)),
	?assertEqual(Bbdef#bbdef.previoushash, bblock:previoushash(Bblock)),
	?assertEqual(Bbdef#bbdef.merkleroot, bblock:merkleroot(Bblock)),
	?assertEqual(Bbdef#bbdef.timestamp, bblock:timestamp(Bblock)),
	?assertEqual(Bbdef#bbdef.difficulty, bblock:difficulty(Bblock)),
	?assertEqual(Bbdef#bbdef.nonce, bblock:nonce(Bblock)),
	?assertEqual(Bbdef#bbdef.blockhash, bblock:hash(Bblock)),
	N = lists:zip(Bbdef#bbdef.txdata, bblock:txs(Bblock)),
	lists:foreach(fun({A, B}) -> ?assertEqual(A#btxdef.txhash, bblock:hash(B)) end, N),
    %% Conver the bbdef back to a bblock
    NewBblock = bblock:bblock(Bbdef),
    ?assertEqual(Bblock, NewBblock).

meta_transfer() ->
	%% Serialization from bbdef to bblock, and from bblock to bbdef 
	%% should preserve meta data correctly.
	B = lib_test:term_from_file("colorblock.bin"),
	Bblock = bblock:bblock(B),
	B2 = bblock:bbdef(Bblock),
	%?debugFmt("XX: ~p~n", [B2]),
	?assertEqual(B, B2).

%meta_transfer_test_() ->
%	Timeout = 60 * 60, %% 1 hour
%    {timeout, Timeout, 
%         fun() -> 
%	B = lib_test:term_from_file("colorblock.bin"),
%	Bblock = bblock:bblock(B),
%	B2 = bblock:bbdef(Bblock),
%	?assertEqual(B, B2)
%         end}.
%


%% Let's duplicate lib_block tests except with bblocks

parse_serialize() ->
    RawBlock = hex:hexstr_to_bin("F9BEB4D9D80000000100000046441C1EB8D69E9ABBE79DF5D965DD9F30A60476EC810CB83C8851B5000000004E2D36D427F313825CB27C1649BDEFF1FBDB4D55B2602069AEBA68D3113565AAA4257249FFFF001D2EF42DD50101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF0804FFFF001D022002FFFFFFFF0100F2052A010000004341046D8F7D934354FE18806DDE9C2362A4693A4C6F55E4E23823419545BF82C39BF6AF29E38CE585F570960897BC60A3B616C4028322A891046DCD3C4373A24A1577AC00000000"),
    {ok, BlockRecord, _, _} = bblock:parse(btr_net_params:params(), RawBlock),
    ?assertEqual(RawBlock, bblock:serialize(BlockRecord)),
    {ok, BlockRecord2, _, _} = bblock:parse(btr_net_params:params(), bblock:serialize(BlockRecord)),
    ?assertEqual(BlockRecord, BlockRecord2).

complicated_serialize() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {ok, BlockRecord, _, _} = bblock:parse(btr_net_params:params(), RawBlock),
    BinBlock = bblock:serialize(BlockRecord),
    ?assertEqual(RawBlock, BinBlock).

json_serialize() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {ok, BlockRecord, _, _} = bblock:parse(btr_net_params:params(), RawBlock),
    JsonBlock = bblock:to_json(BlockRecord),
    D = jiffy:decode(JsonBlock, [return_maps]),
    ?assertEqual(hex:bin_reverse(hex:hexstr_to_bin(binary_to_list(maps:get(<<"hash">>, D)))),
                 bblock:hash(BlockRecord)).

offset_checking() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {continue, B, _BlockOffset, Offsets, _} = lib_parse:parse_raw_block(bblock, btr_net_params:params(), RawBlock),
    Hashes = bblock:tx_hashes(B),
    lists:foreach(fun(E) ->
                {RawHash, {Hash, Offset, Length}} = E,
                ?assertEqual(Hash, RawHash),
                ParsedTx = lib_parse:parse_tx(btx, binary:part(RawBlock, {Offset, Length})),
                ?assertEqual(RawHash, bblock:hash(ParsedTx))
        end, lists:zip(Hashes, Offsets)).


multiblock_offset_checking_bblock() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    RawChunk = iolist_to_binary([RawBlock, RawBlock, RawBlock, RawBlock]),
    {continue, _, _, _, NextBlock2} = lib_parse:parse_raw_block(bblock, btr_net_params:params(), RawChunk),
    {continue, _, _, _, NextBlock3} = NextBlock2(),
    {continue, B, {BlockOffset, BlockLength}, Offsets, _} = NextBlock3(),
    lists:foreach(fun(E) ->
                {Btx, {Hash, Offset, Length}} = E,
                HashBtx = bblock:hash(Btx),
                ?assertEqual(HashBtx, Hash),
                ParsedTx = lib_parse:parse_tx(btx, binary:part(RawChunk, {Offset, Length})),
                %% We don't compare the actual TX because txs() lazily evaluates and includes
                %% the entire remaining block binary
                ?assertEqual(HashBtx, bblock:hash(ParsedTx))
        end, lists:zip(bblock:txs(B), Offsets)),
    {continue, B2, _, _, _} = lib_parse:parse_raw_block(bblock, btr_net_params:params(), binary:part(RawChunk, {BlockOffset, BlockLength})),
    ?assertEqual(B, B2).


extended_attributes() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
	{continue, Bbdef, _, _, _} = lib_parse:parse_raw_block(bbdef, btr_net_params:params(), RawBlock),
	B = Bbdef#bbdef{e_height = 2000},
	C = bblock:bblock(B),
	D = bblock:bbdef(C),
	?assertEqual(2000, D#bbdef.e_height).

slim_btx() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
	{continue, Block, _, _, _} = lib_parse:parse_raw_block(bblock, btr_net_params:params(), RawBlock),
	Tx = bblock:tx(Block, 0),
	SlimTx = bblock:output_set(Tx),
	lists:foreach(fun({A,B}) ->
	                      ?assertEqual(bblock:script(A), bblock:script(B))
                  end, lists:zip(bblock:outputs(Tx), bblock:outputs(SlimTx))),
    %% Remove outputs.
    bblock:strip_output(SlimTx, 0).

single_outputs() ->
 A = {btx,<<1,0,0,0,0,2,64,66,15,0,0,0,0,0,25,118,169,20,165,85,193,0,227,48,
              242,120,150,223,221,53,225,193,221,13,255,145,178,168,136,172,
              160,187,13,0,0,0,0,0,25,118,169,20,246,106,219,32,20,106,241,29,
              61,110,130,149,36,57,194,56,21,92,16,0,136,172,0,0,0,0>>,
            undefined,#{},#{}},
 bblock:output(A, 0).


strip_btx() ->
    %Btx = {btx,<<1,0,0,0,1,174,178,6,75,48,191,149,75,93,167,75,213,92,199,142,136,79,
    %   183,235,55,240,121,240,253,83,24,72,233,91,79,66,98,0,0,0,0,139,72,48,
    %   69,2,33,0,137,217,198,44,213,12,65,76,172,78,9,148,128,30,64,148,234,90,
    %   164,142,133,58,39,153,41,181,233,246,167,142,3,249,2,32,14,194,215,46,
    %   227,202,194,109,196,44,2,187,32,137,239,94,1,195,78,9,30,161,29,24,26,
    %   95,148,229,234,171,33,81,1,65,4,90,167,232,98,73,242,208,99,124,33,171,
    %   166,14,133,67,81,36,240,219,202,0,110,207,197,92,221,105,165,196,11,58,
    %   35,138,100,78,93,96,45,30,151,95,210,191,32,240,166,174,78,170,180,22,
    %   17,199,45,203,145,85,110,68,229,68,114,9,6,255,255,255,255,3,54,73,102,
    %   207,4,0,0,0,25,118,169,20,42,177,97,66,230,167,196,132,152,184,178,126,
    %   79,93,95,124,28,60,117,200,136,172,48,12,84,2,0,0,0,0,25,118,169,20,84,
    %   212,72,199,151,38,157,32,168,70,157,64,56,191,10,10,48,96,141,223,136,
    %   172,183,227,107,2,0,0,0,0,25,118,169,20,249,121,102,126,173,26,128,62,
    %   207,231,74,165,204,183,44,12,96,115,63,21,136,172,0,0,0,0>>,
    % 14752,#{},#{}},
    %Btx2 = bblock:strip_output(Btx, 0),
    %bblock:strip_output(Btx2, 1),

    Btx3 = {btx,<<1,0,0,0,1,162,246,82,203,130,161,174,52,229,162,91,133,49,254,210,19,
       246,45,47,4,237,82,202,10,129,102,171,139,96,205,96,57,0,0,0,0,138,71,
       48,68,2,32,127,104,247,218,234,152,132,74,81,62,208,98,96,86,60,52,105,
       250,93,44,138,224,129,34,35,70,87,24,180,13,123,144,2,32,86,69,124,168,
       1,184,182,144,218,61,156,177,31,88,102,236,255,40,143,116,104,126,34,
       153,45,89,183,41,231,118,186,179,1,65,4,213,128,48,77,50,3,150,182,127,
       174,0,22,131,195,209,149,91,49,233,168,76,149,53,88,88,200,44,199,138,
       35,147,144,154,69,236,47,135,61,27,234,226,181,201,87,194,163,157,169,
       20,45,174,231,9,178,6,82,21,82,123,119,180,51,211,199,255,255,255,255,3,
       56,183,111,0,0,0,0,0,25,118,169,20,70,153,28,187,124,50,78,154,245,254,
       186,114,255,218,121,37,101,48,44,40,136,172,64,174,235,2,0,0,0,0,25,118,
       169,20,6,241,182,112,121,31,146,86,191,252,137,143,71,66,113,194,47,75,
       185,73,136,172,128,159,213,0,0,0,0,0,25,118,169,20,6,241,182,111,254,73,
       223,127,206,104,77,241,108,98,245,157,201,173,189,63,136,172,0,0,0,0>>,
     48805,#{},#{}},

    Btx4 = bblock:strip_output(Btx3, 0),
    bblock:strip_output(Btx4, 1).

inputs_and_outputs() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {ok, BlockRecord, _, _} = bblock:parse(btr_net_params:params(), RawBlock),
    bblock:foldl(fun(Btx, _) ->
                         {Inputs, Outputs} = bblock:inputs_outputs(Btx),
                         %[O|_] = Outputs,
                         %[O2|_] = bblock:outputs(Btx),
                         %?debugFmt("~p ~n ~p ~n", [O, O2]),
                         ?assertEqual(Inputs, bblock:inputs(Btx)),
                         ?assertEqual(Outputs, bblock:outputs(Btx))
                 end, ok, BlockRecord).


compare_tx() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {ok, BlockRecord, _, _} = bblock:parse(btr_net_params:params(), RawBlock),
    Txs = bblock:txs(BlockRecord),
    lists:foldl(fun(Index, TxList) ->
                          [NextTx|T] = TxList,
                          Tx = bblock:tx(BlockRecord, Index),
                          ?assertEqual(NextTx, Tx),
                          ?assertEqual(bblock:hash(NextTx), bblock:hash(Tx)),
                          T
                end, Txs, lists:seq(0, length(Txs)-1)).

output_compression() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {ok, BlockRecord, _, _} = bblock:parse(btr_net_params:params(), RawBlock),
    bblock:foldl(fun(Btx, _) ->
                         bblock:foldl_outputs(fun(Output, _) ->
                                                      % Compress the output
                                                      Compressed = bblock:compress_output(Output),
                                                      Decompressed = bblock:decompress_output(Compressed),
                                                      ?assertEqual(Output#boutput.meta, Decompressed#boutput.meta),
                                                      ?assertEqual(Output#boutput.data, Decompressed#boutput.data)
                                              end, ok, Btx)
                 end, ok, BlockRecord).

%% Long running test to verify compatibility
%% between bbdef and bblock types.
%
%parse_blockchain_test_() ->
%	%% Lets parse the whole local blockchain and compare
%	Network = testnet3,
%	btr_net_params:init(Network),
%	Timeout = 60 * 60, %% 1 hour
%	{timeout, Timeout, 
%          fun() -> 
%          		  lists:foreach(fun(FileName) ->
%          		  						?debugFmt("Comparing file: ~p~n", [FileName]),
%          		  						compare_parse_file(
%          		  						  lib_parse:parsef(bblock, btr_net_params:params(), FileName),
%          		  						  lib_parse:parsef(bbdef, btr_net_params:params(), FileName))
%								end, filelist(Network))
%          end}.	
%
%
%parse_time_test_() ->
%	%% Lets parse the whole local blockchain and compare
%	Network = main,
%	btr_net_params:init(Network),
%	Timeout = 60 * 60, %% 1 hour
%	Type = bblock,
%	{timeout, Timeout, 
%          fun() -> 
%          		  T = lists:foldl(fun(FileName, Acc) ->
%									  {Time, _} = 
%									  timer:tc(fun() -> parse_file(lib_parse:parsef(Type, btr_net_params:params(), FileName)) end),
%									   Acc + Time
%								end, 0, filelist(Network)),
%								?debugFmt("Time: ~p seconds ~n", [T/1000000])
%          end}.	
%

bblock_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"Parse a native block", fun native/0},
		{"Counts", fun counts/0},
		{"Hashing", fun hash/0},
		{"Txs", fun txs/0},
		{"Many blocks", fun many_blocks/0},
		{"Inputs", fun inputs/0},
		{"Outputs", fun outputs/0},
		{"Sparse serialize meta data", fun meta_serialization/0},
		{"Parse Integration", fun parse_integration/0},
		{"Meta conversion", fun meta_transfer/0},
		{"Parse and Serialize", fun parse_serialize/0},
		{"More complicated", fun complicated_serialize/0},
		{"Json Serialize", fun json_serialize/0},
		{"Offset checking", fun offset_checking/0},
		{"Multiple blocks", fun multiblock_offset_checking_bblock/0},
		{"Extended attributes", fun extended_attributes/0},
        {"Slim block", fun slim_btx/0},
        {"strip btx", fun strip_btx/0},
        {"single_outputs", fun single_outputs/0},
        {"Inputs and outputs", fun inputs_and_outputs/0},
        {"Compare tx methods", fun compare_tx/0},
        {"Output compression", fun output_compression/0}

   ]
  }.


rawblock() ->
	hex:hexstr_to_bin("F9BEB4D9D80000000100000046441C1EB8D69E9ABBE79DF5D965DD9F30A60476EC810CB83C8851B5000000004E2D36D427F313825CB27C1649BDEFF1FBDB4D55B2602069AEBA68D3113565AAA4257249FFFF001D2EF42DD50101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF0804FFFF001D022002FFFFFFFF0100F2052A010000004341046D8F7D934354FE18806DDE9C2362A4693A4C6F55E4E23823419545BF82C39BF6AF29E38CE585F570960897BC60A3B616C4028322A891046DCD3C4373A24A1577AC00000000").


%parse_file({continue, _, _, _, Fun}) -> parse_file(Fun());
%parse_file(done) -> done.
%
%compare_parse_file({continue, BBlock, BBlockOffsets, BTxOffsets, BFun},
%		   {continue, Block, BlockOffsets, TxOffsets, Fun}) ->
%	?assertEqual(Block, bblock:bbdef(BBlock)),
%	?assertEqual(BBlock, bblock:bblock(Block)),
%	?assertEqual(BBlockOffsets, BlockOffsets),
%	?assertEqual(BTxOffsets, TxOffsets), 
%	compare_parse_file(BFun(), Fun());
%
%compare_parse_file(done, done) ->
%    erlang:garbage_collect(),
%    ok.
%
%
%%% Filelist
%filelist(Network) ->
%	RealPath = lib_utils:block_directory(btr_net_params:params(Network), auto),
%	SFile = filename:basename("blk00000.dat"),
%	lists:filter(fun(X) -> filename:basename(X) >= SFile end,
%			filelib:wildcard(filename:join(RealPath, "blk*.dat"))).
%
