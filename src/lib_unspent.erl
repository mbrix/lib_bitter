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
         create_input/1,
         readable/1]).

-export([readable_hash/1,
         readable_hash/2,
         get_attribute/3,
	     set_attribute/3,
	     del_attribute/2]).

-export([hash/1,
         index/1,
         script/1,
         height/1,
         value/1,
         set_height/2]).


-include_lib("lib_bitter/include/bitter.hrl").


create_input(I) when is_record(I, btxin) -> I;

create_input(U) ->
    Hash = hash(U),
    Index = index(U),
    Script = script(U),
	SeqNum = 4294967295, % 0xFFFFFFFF
    #binput{data = <<Hash:32/binary,
                     Index:32/little,
                     (lib_parse:int_to_varint(size(Script)))/binary,
                     Script/binary,
                     SeqNum:32/little>>,
            meta = #{}}.


hash(U) when is_record(U, utxop) -> 
    {H, _} = U#utxop.hash_index,
    H;
hash(U) when is_record(U, us) -> unspentset:hash(U).

index(U) when is_record(U, utxop) -> 
    {_, I} = U#utxop.hash_index,
    I;
index(U) when is_record(U, us) -> unspentset:index(U).

script(U) when is_record(U, utxop) -> U#utxop.script;
script(U) when is_record(U, us) -> unspentset:script(U).

height(U) when is_record(U, utxop) -> U#utxop.height;
height(U) when is_record(U, us) -> unspentset:height(U).

value(U) when is_record(U, utxop) -> U#utxop.value;
value(U) when is_record(U, us) -> unspentset:value(U).

set_height(U, Height) when is_record(U, utxop) -> U#utxop{height = Height};
set_height(U, Height) when is_record(U, us) -> unspentset:set_height(U, Height).

readable(Unspent) -> 
    H = hash(Unspent),
    I = index(Unspent),
	io_lib:format("~p ~p", [readable_hash(H), I]).

to_json(NetworkParams, UnspentList, Height) -> jiffy:encode(to_map(NetworkParams, UnspentList, Height)).

to_map(NetworkParams, UnspentList, Height) when is_list(UnspentList) ->
    lists:map(fun(E) ->
                Hash = hash(E),
                Index = index(E),
                Script = script(E),
                AddressRecord = lib_address:new(Script),
                TxHash = iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(Hash))),
                Address = lib_address:readable(binary, NetworkParams, AddressRecord),
                ScriptPubKey = iolist_to_binary(hex:bin_to_hexstr(Script)),
				%% #{} = E#utxop.attributes,
                Color = to_readable(lib_color:readable(binary, NetworkParams, get_attribute(color, E, ?Uncolored))),
                Confirmations = Height - height(E),
                #{txid => TxHash,
                  vout => Index,
                  address => Address,
                  scriptPubKey => ScriptPubKey,
                  amount => lib_transact:satoshi_to_btc(value(E)),
                  confirmations => Confirmations,
                  height => Height,
                  attributes => #{color => Color,
                  				  quantity => get_attribute(quantity, E, 0)}
				 }
        end, UnspentList).

to_readable(A) when is_atom(A) -> A;
to_readable(A) when is_list(A) -> iolist_to_binary(A);
to_readable(A) when is_binary(A) -> A.

%% Coinbase
is_coinbase(U) when is_record(U, utxop) -> U#utxop.coinbase;
is_coinbase(U) when is_record(U, us) -> unspentset:coinbase(U).

%% Filtering

filter_by_confirmations(UnspentList, Height, MinConfirms, MaxConfirms) ->
	%% Remap unconfirms to height+1
	Remapped = lists:map(fun(E) ->
	                             case height(E) of -1 -> set_height(E, Height+1);
	                                               _ -> E
                                 end
                         end, UnspentList),
	%% Remap Coinbase unspents to height + 100 Age
	Remapped2 = lists:map(fun(E) ->
	                                case is_coinbase(E) of
	                                    true -> set_height(E, Height+100);
	                                    false -> E
                                    end
                          end, Remapped),
	%% Filter unspents
    lists:filter(fun(E) ->
                MinH = (Height+1) - MinConfirms,
                MaxH = (Height+1) - MaxConfirms,
                H = height(E),
                (H =< MinH) and (H >= MaxH)
        end, Remapped2).

filter_by_value(UnspentList, any) -> UnspentList;
filter_by_value(UnspentList, Value) ->
    lists:filter(fun(E) -> value(E) >= Value end, UnspentList).

filter_by_color(UnspentList, any) -> UnspentList;
filter_by_color(UnspentList, Color) ->
    C = lib_color:new(Color),
    ColorBin = lib_color:hash160(C),
    lists:filter(fun(E) ->
                         get_attribute(color, E, ?Uncolored) =:= ColorBin
                 end, UnspentList).

filter_by_quantity(UnspentList, Quantity) ->
    lists:filter(fun(E) ->
                         get_attribute(quantity, E, 0) >= Quantity
        end, UnspentList).


readable_hash(binary, Hash) ->
	iolist_to_binary(readable_hash(Hash)).

readable_hash(Hash) when is_binary(Hash) ->
	hex:bin_to_hexstr(hex:bin_reverse(Hash)).


%% Attributes

get_attribute(Attr, #us{}=U, Default) ->
    get_attribute(Attr, unspentset:output(U), Default);

get_attribute(Attr, #boutput{}=B, Default) ->
	case bblock:getattr(Attr, B) of
		error -> Default;
		{ok, Attribute} -> Attribute
	end;

get_attribute(Attr, #btxout{attributes = A}, Default) ->
	case maps:find(Attr, A) of
		error -> Default;
		{ok, Attribute} -> Attribute
	end;

get_attribute(Attr, #utxop{attributes = A}, Default) ->
	case maps:find(Attr, A) of
		error -> Default;
		{ok, Attribute} -> Attribute
	end.

set_attribute(Attr, Value, #us{}=U) -> unspentset:set_output(U, set_attribute(Attr, Value, unspentset:output(U)));

set_attribute(Attr, Value, #binput{}=B) -> {ok, Out} = bblock:setattr(Attr, Value, B),
                                          Out;

set_attribute(Attr, Value, #boutput{}=B) -> {ok, Out} = bblock:setattr(Attr, Value, B),
											Out;
set_attribute(Attr, Value, #btx{}=B) -> {ok, Out} = bblock:setattr(Attr, Value, B),
										Out;
set_attribute(Attr, Value, #btxout{attributes = A}=Out) ->
	Out#btxout{attributes = maps:put(Attr, Value, A)};

set_attribute(Attr, Value, #utxop{attributes = A}=Unspent) ->
	Unspent#utxop{attributes = maps:put(Attr, Value, A)}.

del_attribute(Attr, #btxout{attributes = A}=Out) -> Out#btxout{attributes = maps:remove(Attr, A)};
del_attribute(Attr, #btx{}=B) -> {ok, Out} = bblock:delattr(Attr, B),
									 Out;
del_attribute(Attr, #us{}=U) -> unspentset:set_output(U, del_attribute(Attr, unspentset:output(U)));
del_attribute(Attr, #boutput{}=B) -> {ok, Out} = bblock:delattr(Attr, B),
									 Out;
del_attribute(Attr, #binput{}=B) -> {ok, Out} = bblock:delattr(Attr, B),
									 Out;
del_attribute(Attr, #utxop{attributes = A}=Unspent) -> Unspent#utxop{attributes = maps:remove(Attr, A)}.

