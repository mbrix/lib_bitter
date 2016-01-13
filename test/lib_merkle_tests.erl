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

-module(lib_merkle_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() -> 
	btr_net_params:init(main),
	ok.

stop(_) -> ok.

one() -> 
	Data = <<"a">>,
	RootHash = doublehash(<<(doublehash(Data))/binary,
								  (doublehash(Data))/binary>>),
	{ok, MerkleRoot} = lib_merkle:build([Data]),
	?assertEqual(RootHash, lib_merkle:hash(MerkleRoot)).

two() ->
	A = <<"a">>,
	B = <<"b">>,
	RootHash = doublehash(<<(doublehash(A))/binary,
								  (doublehash(B))/binary>>),
	{ok, MerkleRoot} = lib_merkle:build([A, B]),
	?assertEqual(RootHash, lib_merkle:hash(MerkleRoot)).

block() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	?assertEqual(Block#bbdef.merkleroot, lib_merkle:hash(Root)).

simplepartial() ->
	Tx1 = lib_test:create_transaction(),
	Tx2 = lib_test:create_transaction(),
	{ok, MerkleRoot} = lib_merkle:build([Tx1, Tx2]),
	Root2 = lib_merkle:match(MerkleRoot, fun(Tx) when Tx#btxdef.txhash =:= Tx1#btxdef.txhash -> 1;
											(_) -> 0 end),
	{BitString, Hashes} = lib_merkle:partial(Root2),
	?assertMatch(<<1:1, 1:1, 0:1, _/bitstring>>, BitString),
	?assertEqual([Tx1#btxdef.txhash, Tx2#btxdef.txhash], Hashes),

	%% Now switch match
	Root3 = lib_merkle:match(MerkleRoot, fun(Tx) when Tx#btxdef.txhash =:= Tx2#btxdef.txhash -> 1;
											(_) -> 0 end),
	{BitString2, Hashes2} = lib_merkle:partial(Root3),
	?assertMatch(<<1:1, 0:1, 1:1, _/bitstring>>, BitString2),
	?assertEqual([Tx1#btxdef.txhash, Tx2#btxdef.txhash], Hashes2).

twolevels() ->
	Tx1 = lib_test:create_transaction(),
	Tx2 = lib_test:create_transaction(),
	Tx3 = lib_test:create_transaction(),
	Tx4 = lib_test:create_transaction(),
	{ok, MerkleRoot} = lib_merkle:build([Tx1, Tx2, Tx3, Tx4]),
	Root2 = lib_merkle:match(MerkleRoot, fun(Tx) when Tx#btxdef.txhash =:= Tx3#btxdef.txhash -> 1;
											(_) -> 0 end),
	{BitString, Hashes} = lib_merkle:partial(Root2),
	?assertMatch(<<1:1, 0:1, 1:1, 1:1, 0:1, _/bitstring>>, BitString),
	[_, Hash2, Hash3] = Hashes,
	?assertEqual(Tx3#btxdef.txhash, Hash2),
	?assertEqual(Tx4#btxdef.txhash, Hash3).


partial() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	{BitString, Hashes} = lib_merkle:partial(Root),
	?assertEqual(<<0:8>>, BitString),
	?assertEqual([lib_merkle:hash(Root)], Hashes).

%% Every transaction matches.
fullmatch() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	Root2 = lib_merkle:match(Root, fun(_) -> 1 end),
	{_BitString, Hashes} = lib_merkle:partial(Root2),
	?assertEqual(length(Block#bbdef.txdata), length(Hashes)).

%% Match a single transaction
matchone() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	TxHash = <<94,117,13,180,167,75,222,155,150,50,84,75,174,165,70,26,182,31,208,187,41,43,212,9,247,192,27,11,128,255,80,171>>,
	Root2 = lib_merkle:match(Root, fun(Tx) when Tx#btxdef.txhash =:= TxHash -> 1;
								   (_) -> 0 end),
	{_BitString, _Hashes} = lib_merkle:partial(Root2).

simplevalidate() ->
	Tx1 = lib_test:create_transaction(),
	Tx2 = lib_test:create_transaction(),
	{ok, MerkleRoot} = lib_merkle:build([Tx1, Tx2]),
	Root2 = lib_merkle:match(MerkleRoot, fun(Tx) when Tx#btxdef.txhash =:= Tx1#btxdef.txhash -> 1;
											(_) -> 0 end),
	{BitString, Hashes} = lib_merkle:partial(Root2),
	?assertEqual({valid, [Tx1#btxdef.txhash]}, lib_merkle:validate(Root2, BitString, Hashes)).

validate() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	TxHash = <<94,117,13,180,167,75,222,155,150,50,84,75,174,165,70,26,182,31,208,187,41,43,212,9,247,192,27,11,128,255,80,171>>,
	Root2 = lib_merkle:match(Root, fun(Tx) when Tx#btxdef.txhash =:= TxHash -> 1;
								   (_) -> 0 end),
	{BitString, Hashes} = lib_merkle:partial(Root2),
	%?debugFmt("~p~n", [Hashes]),
	?assertEqual({valid, [TxHash]}, lib_merkle:validate(Root2, BitString, Hashes)).

serialize_block() ->
	Block = loadblock(),
	TxHash = <<94,117,13,180,167,75,222,155,150,50,84,75,174,165,70,26,182,31,208,187,41,43,212,9,247,192,27,11,128,255,80,171>>,
	MatchFun = fun(Tx) when Tx#btxdef.txhash =:= TxHash -> 1; (_) -> 0 end,
	lib_merkle:serialize(lib_merkle:serialize(Block, MatchFun)).



count() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	?assertEqual(296, lib_merkle:count(Root)).

countleafs() ->
	Block = loadblock(),
	{ok, Root} = lib_merkle:build(Block),
	?assertEqual(length(Block#bbdef.txdata), lib_merkle:countleafs(Root)).


merkle_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"One datum", fun one/0},
		{"Two datum", fun two/0},
		{"Count nodes", fun count/0},
		{"Count leafs", fun countleafs/0},
		{"Block merkleroot", fun block/0},
		{"Simple partial tree", fun simplepartial/0},
		{"Two levels of txs", fun twolevels/0},
		{"Build partial empty tree", fun partial/0},
		{"Build partial full tree", fun fullmatch/0},
		{"Match one transaction", fun matchone/0},
		{"Simple validate", fun simplevalidate/0},
		{"Validate partial", fun validate/0},
		{"Serialize", fun serialize_block/0}
   ]
  }.

%% Utility
doublehash(A) -> crypto:hash(sha256, crypto:hash(sha256, A)).
loadblock() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {_, BlockRecord, _, _} = lib_parse:extract(btr_net_params:params(), RawBlock),
    BlockRecord.

%print_bits (<<>>) -> ok;
%print_bits (<<H:1/bitstring, Rest/bitstring>>) ->
%	?debugFmt("~p", [H]),
%    print_bits(Rest).
