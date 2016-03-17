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

-module(unspentset).
-author('mbranton@gmail.com').

-export([new/0,
         destroy/1,
         status/1,
         is_empty/1,
         add/2,
         add/5,
         remove/3,
         fold/3,
         lookup/3,
         count/1,
         serialize/1,
         deserialize/2]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NEW_STATE, 0).
-define(SPENT_STATE, 2).

new() -> #unspentset{mapping = ets:new(us, [set, private, {keypos, 2}])}.
destroy(#unspentset{mapping = M}) -> ets:delete(M).

add(UnspentOutput, #unspentset{mapping = M}) ->
    true = ets:insert(M, UnspentOutput).

add(Hash, Outputs, Height, Coinbase, #unspentset{mapping = M}=U) ->
    true = ets:insert(M, generate_insert(Hash, 0, Outputs, Height, Coinbase, [])),
    U.

generate_insert(_Hash, _Index, [], _Height, _Coinbase, Acc) -> Acc;
generate_insert(Hash, Index, [H|T], Height, Coinbase, Acc) ->
    Meta = bblock:serialize_meta(H),
    generate_insert(Hash, Index+1, T, Height, Coinbase,
                    [#us{hash_index  = <<Hash/binary, Index:32>>, 
                         status = ?NEW_STATE,
                         height_coinbase_output =  <<Height:32,
                                                   (coinbase_serialize(Coinbase)):8,
                                                   (bblock:compress_output(H))/binary,
                                                   (size(Meta)):32,
                                                   Meta/binary>>}|Acc]).

remove(Hash, Index, #unspentset{mapping=M}=U) ->
    HI = <<Hash/binary, Index:32>>,
    case ets:lookup(M, HI) of
        [_] -> true = ets:delete(M, HI),
               {ok, U};
        [] ->
            %% Insert a proxy unspent that we can
            %% later cull from underlying storage
            true = ets:insert(M, #us{hash_index = HI, status = ?SPENT_STATE, height_coinbase_output = <<>>}),
            missing
    end.

count(#unspentset{mapping=M}) ->
    {size, Size} = proplists:lookup(size, ets:info(M)),
    Size.

coinbase_serialize(true) -> 1;
coinbase_serialize(false) -> 0.

fold(Fun, AccStart, #unspentset{mapping=M}) ->
    ets:foldl(fun(UnspentTx, Acc) ->
                      Fun(UnspentTx#us.hash_index, UnspentTx, Acc)
                      end, AccStart, M).

status(#us{status = ?NEW_STATE}) -> new;
status(#us{status = ?SPENT_STATE}) -> spent.

is_empty(#unspentset{mapping=M}) -> 
    case ets:first(M) of
        '$end_of_table' -> true;
        _ -> false
    end.

lookup(Hash, Index, #unspentset{mapping=M}) ->
    %?debugFmt("YYY: ~p~n", [ets:tab2list(M)]),
    %?debugFmt("XXX: ~p ~p~n", [Hash, Index]),
    case ets:lookup(M, <<Hash/binary, Index:32>>) of
        [#us{status = ?SPENT_STATE}] -> spent;
        [UnspentOutput] -> {ok, UnspentOutput};
        [] -> not_found
    end.

serialize(#us{height_coinbase_output = O}) -> O.

deserialize(HashIndex, Bin) ->
    #us{hash_index = HashIndex,
        height_coinbase_output = Bin}.
