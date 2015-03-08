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

% bip-0045 HD Multisig deterministic key generation

% composite structures for building P2SH

-module(lib_hdpm).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").

-export([new/1,
		 purpose/1,
		 lock/1,
		 unlock/1,
		 add/2,
		 remove/2,
		 derive/4,
		 bip45_path/3,
		 bip45_path_rel/3,
		 public/1,
		 pub/1,
		 private/1,
		 priv/1]).

%% Testing
-export([sort_pub_keys/1,
	     co_index/1]).

new(Bip32Init) when is_binary(Bip32Init) ->
	PrivKey = lib_hd:new(Bip32Init),
	#bip45_key{private = PrivKey,
			   cosigner_keys = [purpose(PrivKey)],
			   locked = false};

new(PrivKey) when is_record(PrivKey, bip32_priv_key) ->
	#bip45_key{private = PrivKey,
			   cosigner_keys = [purpose(PrivKey)],
			   locked = false}.


%% Bip-0045 specified 45 as the purpose key
purpose(Key) when is_record(Key, bip45_key) ->
	purpose(Key#bip45_key.private);
purpose(PrivKey) when is_record(PrivKey, bip32_priv_key) ->
	lib_hd:pub(lib_hd:derive(path, PrivKey, <<"m/45'">>)).

lock(Key) when is_record(Key, bip45_key) ->
	Key#bip45_key{locked = true}.

unlock(Key) when is_record(Key, bip45_key) ->
	Key#bip45_key{locked = false}.

add(#bip45_key{locked = false} = K, Cosigner) ->
	add_cosigner(K, Cosigner);
add(_K, _Cosigner) -> error.

exists_cosigner(Key, KeyList) ->
	lists:keyfind(lib_hd:public(Key), 2, KeyList).

add_cosigner(K, XPubKey) when is_record(XPubKey, bip32_pub_key) -> 
	case exists_cosigner(XPubKey, K#bip45_key.cosigner_keys) of
		false ->
			Pubkeys = [XPubKey|K#bip45_key.cosigner_keys],
			K#bip45_key{cosigner_keys = sort_pub_keys(Pubkeys)};
		_ -> K
	end;
add_cosigner(_K, error) -> error.

remove(#bip45_key{locked = false} = K, Cosigner) ->
	remove_cosigner(K, Cosigner);
remove(_K, _Cosigner) -> error.


remove_cosigner(K, XPubKey) when is_record(XPubKey, bip32_pub_key) ->
	Pubkeys = K#bip45_key.cosigner_keys,
	K#bip45_key{cosigner_keys = lists:filter(fun(E) ->
									lib_hd:pub(E) /= lib_hd:pub(XPubKey)
											 end, Pubkeys)};
remove_cosigner(_K, error) -> error.

%% Pubkey sorting
sort_pub_keys(Pubkeys) ->
	lists:sort(fun(X, Y) ->
					   hex:bin_to_hexstr(lib_hd:public(X)) < hex:bin_to_hexstr(lib_hd:public(Y))
			   end, Pubkeys).

%% Bip-0045 derivation
%% Returns {Privkey Derivation, Composite Address}
derive(index, #bip45_key{private = Priv, cosigner_keys = C, locked = true} = K, Change, Index) ->
	Pubkeylist = lists:map(fun({CoIndex, XPubKey}) ->
					  lib_hd:public(lib_hd:derive(path, XPubKey, bip45_path_rel(CoIndex, Change, Index)))
			  end, lists:zip(lists:seq(0, length(C)-1), C)),
	{lib_hd:derive(path, Priv, bip45_path(co_index(K), Change, Index)),
	 lib_address:new(key, Pubkeylist),
	 Pubkeylist};
derive(_, _, _, _) -> error.

co_index(#bip45_key{cosigner_keys = C} = Key) ->
	PubKey = lib_hd:public(purpose(Key)), 
	L = lists:zip(lists:seq(0, length(C)-1),
				  lists:map(fun(E) -> lib_hd:public(E) end, C)),
	{CoIndex, _Rest} = lists:keyfind(PubKey, 2, L),
	CoIndex.

bip45_path(CoIndex, Change, AddressIndex) ->
erlang:iolist_to_binary([<<"m/45'/">>,
						 erlang:integer_to_binary(CoIndex),
						 <<"/">>,
						 erlang:integer_to_binary(Change),
						 <<"/">>,
						 erlang:integer_to_binary(AddressIndex)]).

bip45_path_rel(CoIndex, Change, AddressIndex) ->
erlang:iolist_to_binary([<<"m/">>,
						 erlang:integer_to_binary(CoIndex),
						 <<"/">>,
						 erlang:integer_to_binary(Change),
						 <<"/">>,
						 erlang:integer_to_binary(AddressIndex)]).


public(#bip45_key{private = P}) -> lib_hd:public(P).
pub(#bip45_key{private = P}) -> lib_hd:pub(P).

private(#bip45_key{private = P}) -> lib_hd:private(P).
priv(#bip45_key{private = P}) -> lib_hd:priv(P).

