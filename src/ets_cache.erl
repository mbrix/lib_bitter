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

% ETS front end with insert notification / table events
% specifically for unspent outputs

-module(ets_cache).
-author('mbranton@emberfinancial.com').

-export([new/1,
         lookup/2,
         spend/3,
         template/3,
         delete/2,
         register_hash/2,
         register_lock/1,
         shutdown/1]).

new(TableRef) -> {ok, spawn(fun() -> msgloop(TableRef) end)}.

msgloop(TableRef) ->
        receive
            {register_lock, Pid} -> Pid ! {self(), done};
            {register, Hash} ->  put(Hash, true);
            {spend, Pid, Hash, Index} ->
                case get(Hash) of
                    undefined ->
                        Element = ets:lookup_element(TableRef, Hash, 2),
                        Pid ! {self(), status(Element, Index)},
                        case Element of
                            {1, _} -> ets:delete(TableRef, Hash);
                            {N, Bits} -> true = ets:insert(TableRef, {Hash, {N-1, set_bit(Bits, Index+1)}})
                        end;
                    true ->
                        case ets:lookup(TableRef, Hash) of
                            [] -> add_to_proc(Hash, Pid); 
                            [{_, {N, Bits}}] -> Pid ! {self(), status({N, Bits}, Index)},
                                              case N of
                                                  1 -> ets:delete(TableRef, Hash);
                                                  _ -> true = ets:insert(TableRef, {Hash, {N-1, set_bit(Bits, Index+1)}})
                                              end
                        end
                end;

            {lookup, Pid, Hash, _Index} ->
                case get(Hash) of
                    %% Hash isn't registered as pending
                    %% from current set
                    undefined ->
                        Pid ! {self(), ets:lookup_element(TableRef, Hash, 2)};
                    true ->
                        case ets:lookup(TableRef, Hash) of
                            %% Cache miss forces sender to wait on receive
                            [] ->  add_to_proc(Hash, Pid);
                            [V] -> Pid ! {self(), V}
                        end
                end;
            {template, Pid, Hash, OutputCount} ->
                true = ets:insert(TableRef, {Hash, {OutputCount, to_bits(OutputCount)}}),
                Pid ! {self(), ok},
                case get({a, Hash}) of
                    undefined -> ok;
                    PidList -> send_pids(PidList, {self(), unspent})
                end;
            shutdown -> exit(normal)
        end,
        msgloop(TableRef).

send_pids([], _) -> ok;
send_pids([P|T], Msg) ->
    P ! Msg,
    send_pids(T, Msg).


add_to_proc(Hash, Pid) -> add_to_proc(Hash, Pid, get({a, Hash})).

add_to_proc(Hash, Pid, undefined) -> put({a, Hash}, [Pid]);
add_to_proc(Hash, Pid, PList) ->  put({a, Hash}, [Pid|PList]).


status([], _) -> [];
status({_, Bits}, Index) when is_integer(Bits) -> status(get_bit(Bits, Index+1));
status({_, Bits}, Index) -> status(bitset:contains(Index+1, Bits)).

status(false) -> unspent;
status(true) -> spent.

lookup(Pid, Hash) -> 
    Pid ! {lookup, self(), Hash},
    receive {Pid, Val} -> Val
    end.

spend(MemSet, Hash, Index) -> 
    Pid = get_memset_pid(MemSet, Hash),
    Pid ! {spend, self(), Hash, Index},
                           receive {Pid, Val} -> Val end.

template(MemSet, Hash, OutputCount) ->
    Pid = get_memset_pid(MemSet, Hash),
    Pid ! {template, self(), Hash, OutputCount},
    receive {Pid, ok} -> ok end.

shutdown(Pid) -> Pid ! shutdown.

delete(Pid, Hash) -> Pid ! {delete, Hash}.

register_hash(MemSet, Hash) -> get_memset_pid(MemSet, Hash) ! {register, Hash}.

register_lock(Pid) -> Pid ! {register_lock, self()},
                      receive {Pid, Val} -> Val end.

get_memset_pid(MemSet, <<N:8, _/binary>>) ->
    get_memset_pid(MemSet, N rem 8);

get_memset_pid([A|_], 0) -> A;
get_memset_pid([_,B|_], 1) -> B;
get_memset_pid([_,_,C|_], 2) -> C;
get_memset_pid([_,_,_,D|_], 3) -> D;
get_memset_pid([_,_,_,_,E|_], 4) -> E;
get_memset_pid([_,_,_,_,_,F|_], 5) -> F;
get_memset_pid([_,_,_,_,_,_,G,_], 6) -> G;
get_memset_pid([_,_,_,_,_,_,_,H], 7) -> H;
get_memset_pid(Other, OtherBin) ->
    lager:info("WHAT? ~p ~p~n", [Other, OtherBin]).

to_bits(C) when C < 128 -> 0;
to_bits(_) -> bitset:new().

%% Utility
%%
get_bit(N, B) ->
    N band bit_to_int(B) > 0.
set_bit(N, B) when is_integer(N) -> 
    N bor bit_to_int(B);
set_bit(N, B) -> bitset:insert(B, N).
bit_to_int(B) -> 1 bsl B.

%print_size(C) when C > 10000 -> io:format("Z"); 
%print_size(C) when C > 1000 -> io:format("X"); 
%print_size(C) when C > 100 -> io:format("a");
%print_size(_) -> ok.



%% Group spawn and collect
%%
do_spawn(Max, Fun, WorkList) -> do_spawn(0, Max, Fun, WorkList, self(), make_ref(), []).

do_spawn(0, _Max, _Fun, [], _Self, _Ref, Res) -> Res;

do_spawn(Count, Max, Fun, WorkList, Self, Ref, Res) when Count >= Max ->
    do_spawn(Count-1, Max, Fun, WorkList, Self, Ref, [receive {Ref, R} -> R end|Res]);

do_spawn(Count, Max, Fun, [H|WorkList], Self, Ref, Res) when Count < Max ->
    spawn(fun() -> Self ! {Ref, Fun(H)} end),
    do_spawn(Count+1, Max, Fun, WorkList, Self, Ref, Res);

do_spawn(Count, Max, Fun, [], Self, Ref, Res) ->
    do_spawn(Count-1, Max, Fun, [], Self, Ref, [receive {Ref, R} -> R end|Res]).

