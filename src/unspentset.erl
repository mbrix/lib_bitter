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
         new/1,
         destroy/1,
         status/1,
         status/2,
         is_empty/1,
         add/2,
         add/5,
         remove/4,
         fold/3,
         lookup/3,
         count/1,
         serialize/1,
         serialize2/1,
         address/1,
         create_unspent/5,
         keys/1,
         deserialize/2,
         deserialize2/2,
         output/1]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NEW_STATE, 0).
-define(SPENT_STATE, 2).

new() -> #unspentset{type = ets,
                     mapping = ets:new(us, [set, private, {keypos, 2}])}.

new(ets) -> new();
new(dict) -> #unspentset{type = dict,
                         mapping = dict:new()}.

destroy(#unspentset{type = ets, mapping = M}) -> ets:delete(M);
destroy(#unspentset{type = dict}) -> ok.

add(UnspentOutput, #unspentset{type = ets,
                               mapping = M}=U) ->
    true = ets:insert(M, UnspentOutput),
    U;

add(UnspentOutput, #unspentset{type = dict,
                               mapping = M}=U) ->
    U#unspentset{mapping = dict:store(UnspentOutput#us.hash_index, UnspentOutput, M)}.

add(Hash, Outputs, Height, Coinbase, #unspentset{type = ets, mapping = M}=U) ->
    true = ets:insert(M, generate_insert(Hash, 0, Outputs, Height, Coinbase, [])),
    U;

add(Hash, Outputs, Height, Coinbase, #unspentset{type = dict, mapping =M}=U) ->
    U#unspentset{mapping = store_dict(generate_insert(Hash, 0, Outputs, Height, Coinbase, []), M)}.


store_dict([], Dict) -> Dict;
store_dict([H|T], Dict) -> store_dict(T, dict:store(H#us.hash_index, H, Dict)).

generate_insert(_Hash, _Index, [], _Height, _Coinbase, Acc) -> Acc;
generate_insert(Hash, Index, [Output|T], Height, Coinbase, Acc) ->
    generate_insert(Hash, Index+1, T, Height, Coinbase,
                    [create_unspent(Output, Hash, bblock:index(Output), Height, Coinbase)|Acc]).

create_unspent(Output, Hash, Index, Height, Coinbase) ->
    #us{hash_index  = tohash(Hash, Index), 
                         status = ?NEW_STATE,
                         height_coinbase_output =  <<Height:32,
                                                   (coinbase_serialize(Coinbase)):8,
                                                   (bblock:compress_output(Output))/binary>>}.

keys(#unspentset{type = dict, mapping = M}) -> dict:fetch_keys(M).

tohash(Hash, Index) -> <<Hash/binary, Index:32>>.

remove(Hash, Index, U, Output) when is_record(Output, boutput) ->
    remove(Hash, Index, U, create_unspent(Output, Hash, Index, 0, false));

remove(Hash, Index, U, <<>>) -> remove(Hash, Index, U, #us{hash_index = tohash(Hash, Index),
                                                           height_coinbase_output = <<>>});

remove(Hash, Index, #unspentset{type = ets, mapping=M}=U, UnspentOutput) ->
    HI = tohash(Hash, Index),
    case ets:lookup(M, HI) of
        [_] -> true = ets:delete(M, HI),
               {ok, U};
        [] ->
            %% Insert a proxy unspent that we can
            %% later cull from underlying storage
            true = ets:insert(M, UnspentOutput#us{status = ?SPENT_STATE}),
            {ok, U}
    end;

remove(Hash, Index, #unspentset{type = dict, mapping=M}=U, UnspentOutput) ->
    HI = tohash(Hash, Index),
    case dict:find(HI, M) of
        {ok, _Unspent} -> {ok, U#unspentset{mapping = dict:erase(HI, M)}};
        error -> 
            {ok, U#unspentset{mapping = dict:store(HI, UnspentOutput#us{status = ?SPENT_STATE})}}
    end.


count(#unspentset{type = ets, mapping=M}) ->
    {size, Size} = proplists:lookup(size, ets:info(M)),
    Size;

count(#unspentset{type = dict, mapping=M}) -> dict:size(M).

coinbase_serialize(true) -> 1;
coinbase_serialize(false) -> 0.

fold(Fun, AccStart, #unspentset{type = ets, mapping=M}) ->
    ets:foldl(fun(UnspentTx, Acc) ->
                      Fun(UnspentTx#us.hash_index, UnspentTx, Acc)
                      end, AccStart, M);

fold(Fun, AccStart, #unspentset{type = dict, mapping=M}) -> dict:fold(Fun, AccStart, M).

status(#us{status = ?NEW_STATE}) -> new;
status(#us{status = ?SPENT_STATE}) -> spent.

status(spent, U) -> U#us{status = ?SPENT_STATE};
status(new, U) -> U#us{status = ?NEW_STATE}.

is_empty(#unspentset{type = ets, mapping=M}) -> 
    case ets:first(M) of
        '$end_of_table' -> true;
        _ -> false
    end;

is_empty(#unspentset{type = dict, mapping=M}) -> dict:is_empty(M).

lookup(Hash, Index, #unspentset{type = ets, mapping=M}) ->
    case ets:lookup(M, tohash(Hash, Index)) of
        [#us{status = ?SPENT_STATE}=U] -> {spent, U};
        [UnspentOutput] -> {ok, UnspentOutput};
        [] -> not_found
    end;

lookup(Hash, Index, #unspentset{type = dict, mapping = M}) ->
    case dict:find(tohash(Hash, Index), M) of
        {ok, #us{status = ?SPENT_STATE}=U} -> {spent, U};
        {ok, UnspentOutput} -> {ok, UnspentOutput};
        error -> not_found
    end.

serialize(#us{height_coinbase_output = O}) -> O.

deserialize(HashIndex, Bin) ->
    #us{hash_index = HashIndex,
        status = ?NEW_STATE,
        height_coinbase_output = Bin}.

serialize2(#us{status = Status, height_coinbase_output = O}) -> <<Status:8, O/binary>>.

deserialize2(HashIndex, Bin) ->
    <<Status:8, Output/binary>> = Bin,
    #us{hash_index = HashIndex,
        status = Status,
        height_coinbase_output = Output}.


output(#us{height_coinbase_output = Bin}) ->
    <<_Height:32, _Coinbase:8, Rest/binary>> = Bin,
    bblock:decompress_output(Rest).

address(Unspent) -> bblock:address(output(Unspent)).
