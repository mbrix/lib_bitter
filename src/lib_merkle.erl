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

-module(lib_merkle).
-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").
-author('mbranton@emberfinancial.com').

%% Merkle trees for Bitcoin blocks and SPV

-export([build/1,
		 hash/1,
		 match/2,
		 partial/1,
		 serialize/1,
		 serialize/2,
		 count/1,
		 countleafs/1,
		 validate/3]).

-record(merkle_block, {version,
					   prev_block,
					   merkle_root,
					   timestamp,
					   bits,
					   nonce,
					   total_transactions,
					   hashes,
					   flags}).

-record(merkle_node, {hash, left, right, leaf, tx, match}).

build(Block) when is_record(Block, bbdef) ->
	build(Block#bbdef.txdata);
build(Objs) when is_list(Objs) ->
	build(Objs, []);
build(_) -> error.

build([], []) -> error;
build([], [M]) -> {ok, M};
build([], Nodes) -> 
	build(lists:reverse(Nodes), []);
build([H,H2|T], Nodes) ->
	build(T, [#merkle_node{hash  = hash(<<(hash(H))/binary, (hash(H2))/binary>>),
						   left  = mnode(H),
						   right = mnode(H2),
						   leaf  = false,
						   tx    = undefined,
						   match = 0}|Nodes]);

build([H], Nodes) -> 
	build([], [#merkle_node{hash  = hash(<<(hash(H))/binary, (hash(H))/binary>>),
						   left  = mnode(H),
						   right = undefined,
						   leaf  = false,
						   tx    = undefined,
						   match = 0}|Nodes]).

mnode(H) when is_record(H, btxdef) -> #merkle_node{hash = H#btxdef.txhash,
												   left = undefined,
												   right = undefined,
												   leaf = true,
												   tx   = H,
												   match = 0};
mnode(H) -> H.

hash(H) when is_record(H, merkle_node) -> H#merkle_node.hash;
hash(H) when is_record(H, btxdef) -> H#btxdef.txhash;
hash(H) -> crypto:hash(sha256, crypto:hash(sha256, H)).

%% leafs
countleafs(undefined) -> 0;
countleafs(#merkle_node{leaf = true}) -> 1;
countleafs(#merkle_node{leaf = false, left = L, right = R}) ->
	countleafs(L) + countleafs(R).

%% Count util
count(undefined) -> 0;
count(#merkle_node{leaf = true}) -> 1;
count(Node) ->
	1 + count(Node#merkle_node.left) + count(Node#merkle_node.right).

%% Match TX based on fun, and recursively modify match attribute

match(undefined, _Matchfun) -> undefined;
match(#merkle_node{leaf = true} = Node, MatchFun) ->
	Node#merkle_node{match = MatchFun(Node#merkle_node.tx)};
match(Node, MatchFun) ->
	Left = match(Node#merkle_node.left, MatchFun),
	Right = match(Node#merkle_node.right, MatchFun),
	Node#merkle_node{left = Left,
					 right = Right,
					 match = check_match(Left, Right)}.

check_match(#merkle_node{match=M}, undefined) -> M;
check_match(#merkle_node{match=LM},#merkle_node{match=RM}) -> LM bor RM.

%% Partial merkle tree construction

partial(Node) when is_record(Node, merkle_node) ->
	{Bits, Hashes} = partial(Node, <<>>, []),
	{rev(pad_to(8, Bits)), lists:reverse(Hashes)}.

partial(undefined, BitArray, Hashes) -> {BitArray, Hashes};

partial(#merkle_node{match = 1, leaf = true, hash = H}, BitArray, Hashes) ->
	{<<1:1, BitArray/bitstring>>, [H|Hashes]};

partial(#merkle_node{match = 1, left = L, right = R}, BitArray, Hashes) ->
	{BitArray2, Hashes2} = partial(L, <<1:1, BitArray/bitstring>>, Hashes),
	partial(R, BitArray2, Hashes2);

partial(#merkle_node{match = 0, hash = H}, BitArray, Hashes) ->
	{<<0:1, BitArray/bitstring>>, [H|Hashes]}.

%% Serialize a merkleblock
serialize(B) when is_record(B, merkle_block) ->
    erlang:iolist_to_binary(
    [<<(B#merkle_block.version):32/little, 
    (B#merkle_block.prev_block):256/bitstring, 
    (B#merkle_block.merkle_root):256/bitstring, 
    (B#merkle_block.timestamp):32/little, 
    (B#merkle_block.bits):32/little,
    (B#merkle_block.nonce):32/little,
    (B#merkle_block.total_transactions):32/little>>,
     lib_tx:int_to_varint(length(B#merkle_block.hashes)),
     B#merkle_block.hashes,
     lib_tx:int_to_varint(size(B#merkle_block.flags)),
     B#merkle_block.flags]).

%% Take a block, run matcher again merkletree, generate merkleblock
serialize(Block, MatchFun) ->
	{ok, MerkleRoot} = build(Block),
	MerkleRoot2 = match(MerkleRoot, MatchFun),
	{BitArray, Hashes} = partial(MerkleRoot2),
	#merkle_block{version = Block#bbdef.version,
				  prev_block = Block#bbdef.previoushash,
				  merkle_root = Block#bbdef.merkleroot,
				  timestamp = Block#bbdef.timestamp,
				  bits = Block#bbdef.difficulty,
				  nonce = Block#bbdef.nonce,
				  total_transactions = Block#bbdef.txcount,
				  hashes = Hashes,
				  flags = BitArray}.


validate(_Node, <<0:1, Rest/bitstring>>, [H|Hashes], Matched) ->
	{H, Rest, Hashes, Matched};
validate(#merkle_node{leaf = true}, <<1:1, Rest/bitstring>>, [H|T], Matched) ->
	{H, Rest, T, [H|Matched]};
validate(#merkle_node{leaf = true}, <<0:1, Rest/bitstring>>, [H|T], Matched) ->
	{H, Rest, T, Matched};
validate(#merkle_node{right = undefined}=Node, <<1:1, Rest/bitstring>>, Hashes, Matched) ->
	{H, R2, Hashes2, Matched2} = validate(Node#merkle_node.left, Rest, Hashes, Matched),
	{hash(<<H/binary, H/binary>>), R2, Hashes2, Matched2};

validate(Node, <<1:1, Rest/bitstring>>, Hashes, Matched) ->
	{H, R2, Hashes2, Matched2} = validate(Node#merkle_node.left, Rest, Hashes, Matched),
	{H2, R3, Hashes3, Matched3} = validate(Node#merkle_node.right, R2, Hashes2, Matched2),
	true = (H =/= H2), %% Left and Right hashes are never equal
	{hash(<<H/binary, H2/binary>>), R3, Hashes3, Matched3}.

validate(block, MBlock, Block) ->
	true = (MBlock#merkle_block.version =:= Block#bbdef.version),
	true = (MBlock#merkle_block.prev_block =:= Block#bbdef.previoushash),
	true = (MBlock#merkle_block.merkle_root =:= Block#bbdef.merkleroot),
	true = (MBlock#merkle_block.bits =:= Block#bbdef.difficulty),	
	{ok, Root} = lib_merkle:build(Block),
	validate(Root, MBlock#merkle_block.flags, MBlock#merkle_block.hashes);


validate(MerkleRoot, BitArray, Hashes) ->
	%% Truncate the BitArray to the number of Hashes available
	true = ((erlang:bit_size(BitArray) rem 8) =:= 0),
	{Hash, BitArray2, Hashes2, Matched} = validate(MerkleRoot, BitArray, Hashes, []),
	Hash = MerkleRoot#merkle_node.hash,
	[] = Hashes2,
	true = (erlang:bit_size(BitArray2) < 8),
	true = allzeroes(BitArray2),
	{valid, Matched}.

allzeroes(<<>>) -> true;
allzeroes(<<1:1, _/bitstring>>) -> false;
allzeroes(<<0:1, R/bitstring>>) -> allzeroes(R).

%% Utility
%%
pad_to(Width, Binary) ->
	case (Width - erlang:bit_size(Binary) rem Width) rem Width
       of 0 -> Binary
        ; N -> <<0:N, Binary/bitstring>>
     end.

rev (Bits) -> rev(Bits, <<>>).
rev (<<>>, Acc) -> Acc;
rev (<<H:1/bitstring, Rest/bitstring>>, Acc) ->
    rev(Rest, <<H/bitstring, Acc/bitstring>>).
