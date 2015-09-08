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
%

%% Miscelaneous compound functions useful for testing

-module(lib_utils).
-author('mbranton@emberfinancial.com').

-export([stat/0,
		 block/1,
		 metablock/1,
		 info/1]).

-include_lib("bitter.hrl").


block(Height) when is_integer(Height) ->
	block(element(2, bitter_chaind:blockheight(near, Height)));

block(Hash) when is_binary(Hash) ->
	{A, _Meta} = metablock(Hash),
	A.

metablock(Height) when is_integer(Height) ->
	metablock(element(2, bitter_chaind:blockheight(near, Height)));
metablock(Hash) when is_binary(Hash) ->
	{ok, A, Meta} = bitter_blockd_app:query(near, colorhash, Hash),
	{A, Meta}.

info(B) when is_record(B, bbdef) ->
	#{addresses => lib_block:addresses(B),
	  colors => lib_block:colors(B),
	  txcount => length(B#bbdef.txdata)};
info(Criteria) -> info(block(Criteria)).

	
%% System status
stat() ->
	merge_maps([{utxo, bitter_utxo:info()},
				{mempool, bitter_mempool:info()},
				{chain, bitter_chaind:info(near)},
				{disk, bitter_diskloader:status()}]).

merge_maps(Maps) -> merge_maps(Maps, #{}).
merge_maps([], Maps) -> Maps;
merge_maps([{System, H}|T], Maps) -> merge_maps(T, maps:put(System, convert_hashes(H), Maps)).

convert_hashes(M) when is_map(M) -> maps:map(fun(_K,V) -> convert_hashes(V) end, M);
convert_hashes(M) when is_tuple(M) -> convert_hashes(tuple_to_list(M));
convert_hashes(M) when is_list(M) -> lists:map(fun(E) -> convert_hashes(E) end, M);
convert_hashes(M) when is_binary(M), size(M) =:= 32 -> lib_tx:readable_txhash(M);
convert_hashes(M) -> M.
