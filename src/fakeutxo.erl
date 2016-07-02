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
    ets:new(ordered_fakeutxo, [named_table, ordered_set, {keypos, 1}, {read_concurrency, true}]),
	ets:new(fakeutxo, [named_table, set, {keypos, 2}, {read_concurrency, true}]).

stop() ->
    ets:delete(ordered_fakeutxo),
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
	true = ets:delete(fakeutxo, <<(I#btxin.txhash):32/binary, (I#btxin.txindex):32>>),
	remove_inputs(T).

add_outputs(_, []) -> ok;
add_outputs(TxHash, Outputs) ->
	[O|T] = Outputs, 
	I = bblock:index(O),
	Unspent = lib_test:output_to_unspent(O, TxHash, bblock:index(O), 100, false),
	FakeUtxoEntry = #fakeutxo{hash_index = <<TxHash:32/binary, I:32>>,
	                          unspent = Unspent},
	true = ets:insert(ordered_fakeutxo, {get_next(), FakeUtxoEntry}), 
	true = ets:insert(fakeutxo, FakeUtxoEntry),
	%?debugFmt("ADDING OUTPUT: ~p ~p ~n",[lib_color:color(Unspent), lib_color:quantity(Unspent)]),
	add_outputs(TxHash, T).

lookup_tx(TxHash, TxIndex) ->
    case ets:lookup(fakeutxo, <<TxHash:32/binary, TxIndex:32>>) of
		[N] -> {ok, N#fakeutxo.unspent};
		_ -> notx
	end.

get_next() -> proplists:get_value(size, ets:info(ordered_fakeutxo)).

foldl(Fun, Acc) ->
	ets:foldl(fun({_, Unspent}, Acc2) -> 
                      case ets:lookup(fakeutxo, Unspent#fakeutxo.hash_index) of
                          [RealUnspent] -> 
                              %?debugFmt("INDEX: ~p~n", [Index]),
                              Fun(RealUnspent#fakeutxo.unspent, Acc2);
                      [] -> Acc2
                      end
                      end, Acc, ordered_fakeutxo).


all_unspents() ->
    lists:filtermap(fun({_, Unspent}) ->
                      case ets:lookup(fakeutxo, Unspent#fakeutxo.hash_index) of
                          [RealUnspent] -> {true, RealUnspent#fakeutxo.unspent};
                          [] -> false
                      end
                      end,
              ets:tab2list(ordered_fakeutxo)).

