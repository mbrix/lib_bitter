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

-module(lib_kd).
-author('mbranton@emberfinancial.com').

% Key Dictionary helper functions
% Used to move/create Unspent Outputs (utxop) into dicts
% for key signing

-export([add/1, add/2, add/3, 
	     get/2, get/3,
	     merge/2,
	     oldest/1,
	     newest/1,
	     height/2,
	     height/3,
	     remove/2]).

-include_lib("bitter.hrl").

% Convert input into unspent
%add(I) when is_record(I, btxin) ->
%	add(#utxop{hash_index = {I#btxin.txhash, I#btxin.txindex},
%			script = I#btxin.txindex});

add(U) when is_record(U, utxop) ->
	dict:store(U#utxop.hash_index, U, dict:new());
add(L) when is_list(L) ->
	lists:foldl(fun(U,Acc) ->
				merge(add(U), Acc)
		end, dict:new(), L).

add(Hash, O) when is_record(O, btxout) ->
	add(lib_test:output_to_unspent(Hash, O));

add(Any, Dict) ->
	merge(add(Any), Dict).

add(Hash, O, Dict) when is_record(O, btxout) ->
	merge(add(Hash, O), Dict).

merge(D, D2) ->
	dict:merge(fun(k, v1, v2) -> v2 end, D, D2).

get(I, KD) when is_record(I, btxin) ->
	get(I#btxin.txhash, I#btxin.txindex, KD);

get(U, KD) when is_record(U, utxop) ->
	{H,I} = U#utxop.hash_index,
	get(H,I,KD).

get(Hash, O, KD) when is_record(O, btxout) ->
	get(lib_test:output_to_unspent(Hash, O), KD);

get(Hash, Index, KD) ->
	case dict:find({Hash,Index},KD) of
		{ok, V} -> V;
		_ -> error
	end.

remove(Dict, RDict) ->
    dict:filter(fun(K,_V) ->
                case dict:find(K, RDict) of
                    {ok, _} -> false;
                    error -> true
                end end, Dict).

height(Dict, Height) ->
    oldest(height(dict, Dict, Height)).

height(dict, Dict, Height) ->
    dict:filter(fun(_K, V) -> lib_unspent:height(V) =< Height end, Dict).

oldest(List) when is_list(List) ->
	pop_unconfirmed(lists:sort(fun(A, B) ->
	                                   AHeight = lib_unspent:height(A),
	                                   BHeight = lib_unspent:height(B),
									   if  AHeight =< BHeight -> true;
									   	   true -> false
									   end end, List));

oldest(Dict) -> oldest(lists:map(fun({_,V}) -> V end, dict:to_list(Dict))).


pop_unconfirmed(L) -> pop_unconfirmed(L, []).
pop_unconfirmed([], Acc) -> lists:reverse(Acc);
pop_unconfirmed(L, Acc) ->
    [H|T] = L,
    case lib_unspent:height(H) of
        -1 -> pop_unconfirmed(T, [H|Acc]);
        _  -> L ++ lists:reverse(Acc)
    end.

newest(Dict) ->
	lists:reverse(oldest(Dict)).
