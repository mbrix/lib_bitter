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
-author('mbranton@emberfinancial.com').

%% Merkle trees for Bitcoin blocks and SPV

-export([build/1,
		 hash/1,
		 match/2]).

-record(merkle_node, {hash, left, right, leaf, tx, match}).

build(Block) when is_record(Block, bbdef) ->
	build(Block#bbdef.txdata);
build(Objs) when is_list(Objs) ->
	build(Objs, []);
build(_) -> error.

build([], []) -> error;
build([], [M]) -> {ok, M};
build([], Nodes) -> build(lists:reverse(Nodes), []);
build([H,H2|T], Nodes) ->
	build(T, [#merkle_node{hash  = hash(<<(hash(H))/binary, (hash(H2))/binary>>),
						   left  = mnode(H),
						   right = mnode(H2),
						   leaf  = false,
						   tx    = undefined,
						   match = 0}|Nodes]);
build([H|T], Nodes) ->
	build([H,H|T], Nodes).

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

%% Match TX based on fun, and recursively modify match attribute

match(#merkle_node{leaf = true} = Node, MatchFun) ->
	Node#merkle_node{match = MatchFun(Node#merkle_node.tx)};
match(Node, MatchFun) ->
	Left = match(Node#merkle_node.left, MatchFun),
	Right = match(Node#merkle_node.right, MatchFun),
	Node#merkle_node{left = Left,
					 right = Right,
					 match = Left#merkle_node.match or Right#merkle_node.match}.
