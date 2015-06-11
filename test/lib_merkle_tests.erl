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


start() -> ok.

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


merkle_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"One datum", fun one/0},
		{"Two datum", fun two/0},
		{"Block merkleroot", fun block/0}
   ]
  }.

%% Utility
doublehash(A) -> crypto:hash(sha256, crypto:hash(sha256, A)).
loadblock() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {_, BlockRecord, _, _} = lib_parse:extract(RawBlock),
    BlockRecord.

