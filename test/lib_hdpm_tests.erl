
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

-module(lib_hdpm_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	ok.

stop(_) ->
	ok.

bip45_init() ->
	Master = hex:hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
	lib_hdpm:new(Master),
	ok.

paths() ->
	?assertEqual(<<"m/45'/0/0/0">>, lib_hdpm:bip45_path(0, 0, 0)),
	?assertEqual(<<"m/45'/100/20/1">>, lib_hdpm:bip45_path(100, 20, 1)),
	?assertEqual(<<"m/45'/200/1/23">>, lib_hdpm:bip45_path(200, 1, 23)),
	?assertEqual(<<"m/45'/3/1/4">>, lib_hdpm:bip45_path(3, 1, 4)),
	?assertEqual(<<"m/0/0/0">>, lib_hdpm:bip45_path_rel(0, 0, 0)),
	?assertEqual(<<"m/100/20/1">>, lib_hdpm:bip45_path_rel(100, 20, 1)),
	?assertEqual(<<"m/200/1/23">>, lib_hdpm:bip45_path_rel(200, 1, 23)),
	?assertEqual(<<"m/3/1/4">>, lib_hdpm:bip45_path_rel(3, 1, 4)).

purpose() ->
	Master = hex:hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
	M = lib_hdpm:new(Master),
	?assertEqual(lib_hd:pub(lib_hd:derive(path, lib_hd:new(Master), <<"m/45'">>)),
				 lib_hdpm:purpose(M)).


%% Util
get_pub_keys() ->
	A = "03a473275a750a20b7b71ebeadfec83130c014da4b53f1c4743fcf342af6589a38",
	B = "039863fb5f07b667d9b1ca68773c6e6cdbcac0088ffba9af46f6f6acd153d44463",
	C = "03f76588e06c0d688617ef365d1e58a7f1aa84daa3801380b1e7f12acc9a69cd13",
	D = "0393a617abb9657c2e2de47d256de9df52eab3c0aa155403f13d2a33a14fd86856",
	[A,B,C,D].

pub_to_bip32(L) ->
	lists:map(fun(E) -> #bip32_pub_key{key = hex:hexstr_to_bin(E)} end, L).


lexigraphic_order() ->
	[A, B, C, D] = pub_to_bip32(get_pub_keys()),
	Correct = [D, B, A, C],
	Wrong = [A, B, D, C],
	?assertEqual(Correct, lib_hdpm:sort_pub_keys(Wrong)).


add() ->
	M = lib_hd:new(crypto:rand_bytes(32)),
	M2 = lib_hd:pub(lib_hd:new(crypto:rand_bytes(32))),
	M3 = lib_hd:pub(lib_hd:new(crypto:rand_bytes(32))),
	MPM = lib_hdpm:new(M),
	MPM2 = lib_hdpm:add(MPM, M2),
	?assertNotEqual(error, MPM2),
	MPM3 = lib_hdpm:add(MPM2, M3),
	?assertEqual(3, length(MPM3#bip45_key.cosigner_keys)).

add_same() ->
	MPM = lib_hdpm:new(crypto:rand_bytes(32)),
	M2 = lib_hd:pub(lib_hd:new(crypto:rand_bytes(32))),
	MPM2 = lib_hdpm:add(MPM, M2),
	MPM3 = lib_hdpm:add(MPM2, M2),
	?assertEqual(2, length(MPM3#bip45_key.cosigner_keys)).

co_index() ->
	Master = hex:hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
	M = lib_hdpm:new(Master),
	?assertEqual(0, lib_hdpm:co_index(M)),
	Cosigners = M#bip45_key.cosigner_keys,
	[_|R] = pub_to_bip32(get_pub_keys()),
	M2 = M#bip45_key{cosigner_keys = lib_hdpm:sort_pub_keys(R ++ Cosigners)},
	?assertEqual(0, lib_hdpm:co_index(M2)).

remove() ->
	M = lib_hd:new(crypto:rand_bytes(32)),
	M2 = lib_hd:pub(lib_hd:new(crypto:rand_bytes(32))),
	M3 = lib_hd:pub(lib_hd:new(crypto:rand_bytes(32))),
	MPM = lib_hdpm:new(M),
	MPM2 = lib_hdpm:add(MPM, M2),
	MPM3 = lib_hdpm:add(MPM2, M3),
	MPM4 = lib_hdpm:remove(MPM3, M2),
	MPM5 = lib_hdpm:remove(MPM4, M3),
	?assertEqual(1, length(MPM5#bip45_key.cosigner_keys)).

locked() ->
	M = lib_hd:new(crypto:rand_bytes(32)),
	M2 = lib_hd:pub(lib_hd:new(crypto:rand_bytes(32))),
	MPM = lib_hdpm:new(M),
	MPM2 = lib_hdpm:lock(MPM),
	?assertEqual(error, lib_hdpm:add(MPM2, M2)),
	MPM3 = lib_hdpm:unlock(MPM2),
	?assertNotEqual(error, lib_hdpm:add(MPM3, M2)).

derivation() ->
	M = lib_hd:new(crypto:rand_bytes(32)),
	M2 = lib_hd:new(crypto:rand_bytes(32)),
	M3 = lib_hd:new(crypto:rand_bytes(32)),
	MPM = lib_hdpm:new(M),
	M2PM = lib_hdpm:new(M2),
	M3PM = lib_hdpm:new(M3),
	MPM2 = lib_hdpm:add(MPM, lib_hdpm:purpose(M2PM)),
	MPM3 = lib_hdpm:add(MPM2, lib_hdpm:purpose(M3PM)),
	?assertMatch(error, lib_hdpm:derive(index, MPM3, 0, 0)),
	MPM4 = lib_hdpm:lock(MPM3),
	?assertMatch({_, _, _}, lib_hdpm:derive(index, MPM4, 0, 0)),
	{A, B, _C} = lib_hdpm:derive(index, MPM4, 0, 0),
	?assertEqual(true, is_priv_key(A)),
	?assertEqual(true, is_address(B)).

single() ->
	MPM = lib_hdpm:new(crypto:rand_bytes(32)),
	MPM2 = lib_hdpm:lock(MPM),
	?assertMatch({_NewKey, _Addr, _Publist}, lib_hdpm:derive(index, MPM2, 0, 0)).

path_check() ->
	MPM = lib_hdpm:new(crypto:rand_bytes(32)),
	P = lib_hd:pub(lib_hdpm:purpose(MPM)),
	?assertEqual(lib_hd:public(lib_hdpm:purpose(MPM)), lib_hd:public(P)),
	?assertEqual(lib_hd:public(lib_hd:derive(path, MPM#bip45_key.private,
											 lib_hdpm:bip45_path(0,0,0))),
				 lib_hd:public(lib_hd:derive(path, P, lib_hdpm:bip45_path_rel(0,0,0)))),
	MPM2 = lib_hdpm:lock(MPM),
	?assertMatch(true, MPM2#bip45_key.locked),
	{NewKey, _Addr, Publist} = lib_hdpm:derive(index, MPM2, 0, 0),
	[Pk|_] = Publist,
	?assertEqual(lib_hd:public(NewKey), Pk).


is_address(A) when is_record(A, addr) -> true;
is_address(_) -> false.

is_priv_key(A) when is_record(A, bip32_priv_key) -> true;
is_priv_key(_) -> false.

hdpm_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
   	    {"bip45 init", fun bip45_init/0},
		{"bip45 paths", fun paths/0},
		{"Purpose", fun purpose/0},
		{"Add cosigners", fun add/0},
		{"Add duplicate cosigners", fun add_same/0},
		{"Lexigraphic", fun lexigraphic_order/0},
		{"Remove cosigners", fun remove/0},
		{"Compute index", fun co_index/0},
		{"Lock unlock", fun locked/0},
		{"Derivation", fun derivation/0},
		{"single 1of1 derivation", fun single/0},
		{"path check", fun path_check/0}
   ]
  }.
