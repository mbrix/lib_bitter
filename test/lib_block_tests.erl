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

-module(lib_block_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	ok.

stop(_) ->
	ok.

parse_serialize() ->
    RawBlock = hex:hexstr_to_bin("F9BEB4D9D80000000100000046441C1EB8D69E9ABBE79DF5D965DD9F30A60476EC810CB83C8851B5000000004E2D36D427F313825CB27C1649BDEFF1FBDB4D55B2602069AEBA68D3113565AAA4257249FFFF001D2EF42DD50101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF0804FFFF001D022002FFFFFFFF0100F2052A010000004341046D8F7D934354FE18806DDE9C2362A4693A4C6F55E4E23823419545BF82C39BF6AF29E38CE585F570960897BC60A3B616C4028322A891046DCD3C4373A24A1577AC00000000"),
    {_, BlockRecord, _, _} = lib_parse:extract(RawBlock),
    BinBlock = lib_block:serialize(BlockRecord),
    ?assertEqual(RawBlock,BinBlock),
    {_, BlockRecord2, _, _} = lib_parse:extract(BinBlock),
    ?assertEqual(BlockRecord, BlockRecord2).

complicated_serialize() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {_, BlockRecord, _, _} = lib_parse:extract(RawBlock),
    BinBlock = lib_block:serialize(BlockRecord),
    %?debugFmt("~p~n~p~n", [RawBlock, BinBlock]),
    ?assertEqual(RawBlock, BinBlock).

color() ->
    B = lib_test:fake_block("color_tests1.bin"),
    ColorBin = lib_block:color_serialize(B),
    ColorBlock = lib_block:apply_color(B, ColorBin),
    %?debugFmt("~p~n~n~p~n", [B, ColorBlock]),
    ?assertEqual(B, ColorBlock).

json_serialize() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {_, BlockRecord, _, _} = lib_parse:extract(RawBlock),
    JsonBlock = lib_block:to_json(BlockRecord),
    D = jiffy:decode(JsonBlock, [return_maps]),
    ?assertEqual(hex:bin_reverse(hex:hexstr_to_bin(binary_to_list(maps:get(<<"hash">>, D)))),
                 BlockRecord#bbdef.blockhash).

blockhash_to_binary() ->
    BinaryHash = <<218,134,101,225,0,13,128,90,15,31,160,92,183,164,16,133,177,145,170,117,43,
  203,91,0,0,0,0,0,0,0,0,0>>,
    A = lib_block:blockhash("0000000000000000005bcb2b75aa91b18510a4b75ca01f0f5a800d00e16586da"),
    B = lib_block:blockhash(<<"0000000000000000005bcb2b75aa91b18510a4b75ca01f0f5a800d00e16586da">>),
    ?assertEqual(A, B),
    ?assertEqual(A, BinaryHash).

offset_checking() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {continue, B, _BlockOffset, Offsets, _} = lib_parse:parse_raw_block(RawBlock),
    lists:foreach(fun(E) ->
                {Tx, {Hash, Offset, Length}} = E,
                ?assertEqual(Hash, Tx#btxdef.txhash),
                ParsedTx = lib_parse:parse_tx(binary:part(RawBlock, {Offset, Length})),
                ?assertEqual(Tx, ParsedTx)
        end, lists:zip(B#bbdef.txdata, Offsets)).

multiblock_offset_checking() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    RawChunk = iolist_to_binary([RawBlock, RawBlock, RawBlock, RawBlock]),
    {continue, _, _, _, NextBlock2} = lib_parse:parse_raw_block(RawChunk),
    {continue, _, _, _, NextBlock3} = NextBlock2(),
    {continue, B, {BlockOffset, BlockLength}, Offsets, _} = NextBlock3(),
    lists:foreach(fun(E) ->
                {Tx, {Hash, Offset, Length}} = E,
                ?assertEqual(Hash, Tx#btxdef.txhash),
                ParsedTx = lib_parse:parse_tx(binary:part(RawChunk, {Offset, Length})),
                ?assertEqual(Tx, ParsedTx)
        end, lists:zip(B#bbdef.txdata, Offsets)),
    {continue, B2, _, _, _} = lib_parse:parse_raw_block(binary:part(RawChunk, {BlockOffset, BlockLength})),
    ?assertEqual(B, B2).

rawblock_parsing() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock3.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {continue, _, _, _, _NextBlock} = lib_parse:parse_raw_block(RawBlock).


block_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"Parse and Serialize", fun parse_serialize/0},
		{"More complicated", fun complicated_serialize/0},
		{"Color Serialize / Deserialize", fun color/0},
		{"Json Serialize", fun json_serialize/0},
		{"Blockhash to binary", fun blockhash_to_binary/0},
		{"Offset checking", fun offset_checking/0},
		{"Multiple blocks", fun multiblock_offset_checking/0},
		{"Rawblock parsing", fun rawblock_parsing/0}
   ]
  }.
