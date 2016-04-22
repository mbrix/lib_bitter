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

-module(fakeutxo).
-author('mbranton@emberfinancial.com').

-export([start/0,
         import/0,
         import/1,
         block_tx/1,
	     stop/0,
	     foldl/2,
	     all_unspents/0,
	     lookup_tx/2]).

% Fake Utxo creates a dictionary utxo for loading
% test data.

-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(fakeutxo, {hash_index, unspent}).

start() ->
	ets:new(fakeutxo, [named_table, set, {keypos, 2}, {read_concurrency, true}]).

stop() ->
	ets:delete(fakeutxo).

import() -> import("color_tests1.bin").
import(FileName) ->
	Block = lib_test:fake_block(FileName),
	block_tx(Block).

block_tx(Block) when is_record(Block, bbdef) ->
	import_tx(Block#bbdef.txdata).

import_tx([]) -> ok;
import_tx(TxList) ->
	[H|T] = TxList,
	remove_inputs(H#btxdef.txinputs),
	add_outputs(H#btxdef.txhash, H#btxdef.txoutputs),
	import_tx(T).

remove_inputs([]) -> ok;
remove_inputs(Inputs) ->
	[I|T] = Inputs, 
	ets:delete(fakeutxo, {I#btxin.txhash, I#btxin.txindex}),
	remove_inputs(T).

add_outputs(_, []) -> ok;
add_outputs(TxHash, Outputs) ->
	[O|T] = Outputs, 
	I = bblock:index(O),
	ets:insert(fakeutxo, #fakeutxo{hash_index = {TxHash,I},
	                               unspent = lib_test:output_to_unspent(O, TxHash, bblock:index(O), 100, false)}),
	add_outputs(TxHash, T).

lookup_tx(TxHash, TxIndex) ->
	case ets:lookup(fakeutxo, {TxHash, TxIndex}) of
		[N] -> {ok, N#fakeutxo.unspent};
		_ -> notx
	end.

foldl(Fun, Acc) ->
	ets:foldl(fun(Unspent, Acc2) ->  Fun(Unspent#fakeutxo.unspent, Acc2) end, Acc, fakeutxo).


all_unspents() -> lists:map(fun(E) -> E#fakeutxo.unspent end, ets:tab2list(fakeutxo)).

