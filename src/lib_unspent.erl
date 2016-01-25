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

-module(lib_unspent).
-author('mbranton@emberfinancial.com').

-export([to_json/3,
		 to_map/3,
		 is_coinbase/1,
         filter_by_confirmations/4,
         filter_by_value/2,
         filter_by_quantity/2,
         filter_by_color/2,
         readable/1]).

-include_lib("lib_bitter/include/bitter.hrl").

readable(Unspent) -> 
	{H,I} = Unspent#utxop.hash_index,
	io_lib:format("~p ~p", [lib_tx:readable_txhash(H), I]).

to_json(NetworkParams, UnspentList, Height) -> jiffy:encode(to_map(NetworkParams, UnspentList, Height)).

to_map(NetworkParams, UnspentList, Height) when is_list(UnspentList) ->
    lists:map(fun(E) ->
                {Hash, Index} = E#utxop.hash_index,
                AddressRecord = lib_address:new(E#utxop.script),
                TxHash = iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(Hash))),
                Address = lib_address:readable(binary, NetworkParams, AddressRecord),
                ScriptPubKey = iolist_to_binary(hex:bin_to_hexstr(E#utxop.script)),
                Color = to_readable(lib_color:readable(binary, NetworkParams, E#utxop.color)),
                Confirmations = Height - E#utxop.height,
                #{txid => TxHash,
                  vout => Index,
                  address => Address,
                  scriptPubKey => ScriptPubKey,
                  amount => lib_transact:satoshi_to_btc(E#utxop.value),
                  confirmations => Confirmations,
                  height => Height,
                  color => Color,
                  quantity => E#utxop.quantity}
        end, UnspentList).

to_readable(A) when is_atom(A) -> A;
to_readable(A) when is_list(A) -> iolist_to_binary(A);
to_readable(A) when is_binary(A) -> A.

%% Coinbase
is_coinbase(Unspent) ->
	Unspent#utxop.coinbase.

%% Filtering

filter_by_confirmations(UnspentList, Height, MinConfirms, MaxConfirms) ->
	%% Remap unconfirms to height+1
	Remapped = lists:map(fun(E) when E#utxop.height =:= -1 ->
								 E#utxop{height = Height+1};
							(E) -> E
						 end, UnspentList),
	%% Remap Coinbase unspents to height + 100 Age
	Remapped2 = lists:map(fun(E) when E#utxop.coinbase =:= true ->
								 E#utxop{height = Height+100};
							(E) -> E
						 end, Remapped),
	%% Filter unspents
    lists:filter(fun(E) ->
                MinH = (Height+1) - MinConfirms,
                MaxH = (Height+1) - MaxConfirms,
                (E#utxop.height =< MinH) and (E#utxop.height >= MaxH)
        end, Remapped2).

filter_by_value(UnspentList, any) -> UnspentList;
filter_by_value(UnspentList, Value) ->
    lists:filter(fun(E) ->
                E#utxop.value >= Value
        end, UnspentList).

filter_by_color(UnspentList, any) -> UnspentList;
filter_by_color(UnspentList, Color) ->
    C = lib_color:new(Color),
    ColorBin = lib_color:hash160(C),
    lists:filter(fun(E) ->
                E#utxop.color =:= ColorBin
        end, UnspentList).

filter_by_quantity(UnspentList, Quantity) ->
    lists:filter(fun(E) ->
                E#utxop.quantity >= Quantity
        end, UnspentList).
