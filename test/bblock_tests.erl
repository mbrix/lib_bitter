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
	Tx = bblock:tx(B, 1),
	?assertEqual(1, bblock:input_count(Tx)),
	?assertEqual(1, bblock:output_count(Tx)).

hash() ->
	{ok, B} = bblock:native(btr_net_params:params(), rawblock()),
	CorrectBlockHash = lib_block:from_string("00000000e218d46cb2ebea79906e0052ae1c5bb6b224b0da3bcee63ea53e33d0"),
	CorrectTxHash = lib_block:from_string("aa653511d368baae692060b2554ddbfbf1efbd49167cb25c8213f327d4362d4e"),
	?assertEqual(CorrectBlockHash, bblock:hash(B)),
	Tx = bblock:tx(B, 1),
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
	MetaBlock = #bblock{meta = #{color => ?Uncolored,
								 quantity => 2000}},
	_Ser = bblock:serialize_meta(MetaBlock),
	MetaBlock2 = #bblock{meta = #{10 => #{color => ?Uncolored,
										  quantity => 2000},
								  20 => #{color => crypto:rand_bytes(32)}}},
	_Ser2 = bblock:serialize_meta(MetaBlock2),
	NestedMeta = #bblock{meta = #{10 => #{20 => #{color => ?Uncolored,
												  quantity => 100}
										 }
								 }
						},
	_Ser3 = bblock:serialize_meta(NestedMeta).

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
	?assertEqual(B, B2).


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
		{"Multiple blocks", fun multiblock_offset_checking_bblock/0}

   ]
  }.


rawblock() ->
	hex:hexstr_to_bin("F9BEB4D9D80000000100000046441C1EB8D69E9ABBE79DF5D965DD9F30A60476EC810CB83C8851B5000000004E2D36D427F313825CB27C1649BDEFF1FBDB4D55B2602069AEBA68D3113565AAA4257249FFFF001D2EF42DD50101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF0804FFFF001D022002FFFFFFFF0100F2052A010000004341046D8F7D934354FE18806DDE9C2362A4693A4C6F55E4E23823419545BF82C39BF6AF29E38CE585F570960897BC60A3B616C4028322A891046DCD3C4373A24A1577AC00000000").
