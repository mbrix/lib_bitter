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

-module(lib_script_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	ok.

stop(_) ->
	ok.


%% Scipting language tests

match() ->
	Script = [op_dup, op_hash160,
			  {hex, "0c1b83d01d0ffb2bccae606963376cca3863a7ce"},
			  op_equalverify, op_checksig],
	A = lib_script:build(Script),
	B = iolist_to_binary(A),
	C = hex:hexstr_to_bin("76a9140c1b83d01d0ffb2bccae606963376cca3863a7ce88ac"),
	D = lib_script:ops(hex, B),
	?assertEqual(B, C),
	?assertEqual(Script, D).

simple_eval() ->
	Script = iolist_to_binary([?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6]),
	?assertEqual([6,5,4,3,2], lib_script:eval(Script)).

simple_if() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF, ?OP_2, ?OP_ENDIF]),
	Stack = lib_script:eval(Script),
	?assertEqual([2], Stack).

simple_ifelse() ->
	Script = iolist_to_binary([?OP_0, ?OP_IF, ?OP_2, ?OP_ELSE, ?OP_3, ?OP_ENDIF]),
	Stack = lib_script:eval(Script),
	?assertEqual([3], Stack).

multiple_else() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF, ?OP_2, ?OP_ELSE, ?OP_3, ?OP_ELSE, ?OP_4,
							   ?OP_ELSE, ?OP_5, ?OP_ELSE, ?OP_6, ?OP_ENDIF]),
	Stack = lib_script:eval(Script),
	?assertEqual([6,4,2], Stack).

nested_ifs() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF, ?OP_1, ?OP_IF, ?OP_2, ?OP_ELSE, ?OP_3, ?OP_ELSE, ?OP_4,
			  ?OP_ELSE, ?OP_5, ?OP_ELSE, ?OP_6, ?OP_ENDIF, ?OP_ENDIF]),
	Stack = lib_script:eval(Script),
	?assertEqual([6,4,2], Stack).

dangling_endif() ->
	Script = iolist_to_binary([?OP_1, ?OP_2, ?OP_ENDIF]),
	?assertEqual(false, lib_script:eval(Script)).

dangling_else()->
	Script = iolist_to_binary([?OP_2, ?OP_ELSE]),
	?assertEqual(false, lib_script:eval(Script)).

extra_if() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF]),
	?assertEqual(false, lib_script:eval(Script)).

op_notif() ->
	Script = ib([?OP_0, ?OP_NOTIF, ?OP_2, ?OP_ENDIF]),
	?assertEqual([2], lib_script:eval(Script)).

op_verif() ->
	Script = ib([?OP_VERIF]),
	?assertEqual(false, lib_script:eval(Script)).

op_notverif() ->
	Script = ib([?OP_VERNOTIF]),
	?assertEqual(false, lib_script:eval(Script)).

op_verify() ->
	?assertEqual([1], lib_script:eval(ib([?OP_1, ?OP_VERIFY]))),
	?assertEqual(false, lib_script:eval(ib([?OP_VERIFY]))),
	?assertEqual(false, lib_script:eval(ib([?OP_0, ?OP_VERIFY]))).

op_return() ->
	?assertEqual(false, lib_script:eval(ib([?OP_RETURN]))).

op_altstack() ->
	?assertEqual(false, lib_script:eval(ib([?OP_FROMALTSTACK]))),
	?assertEqual([2], lib_script:eval(ib([?OP_2, ?OP_TOALTSTACK, ?OP_FROMALTSTACK]))).

op_2drop() ->
	?assertEqual([2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_2DROP]))).

op_2dup() -> 
	?assertEqual([4,3,4,3,2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_2DUP]))).

op_3dup() ->
	?assertEqual([4,3,2,4,3,2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_3DUP]))).

op_2over() ->
	?assertEqual([2,1,4,3,2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_2OVER]))).

op_2rot() ->
	?assertEqual([2,1,6,5,4,3], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_2ROT]))).

op_2swap() ->
	?assertEqual([4,3,6,5,2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_2SWAP]))).

op_ifdup() ->
	?assertEqual([4,4], lib_script:eval(ib([?OP_4, ?OP_IFDUP]))),
	?assertEqual([0], lib_script:eval(ib([?OP_0, ?OP_IFDUP]))).

op_depth() ->
	?assertEqual([0], lib_script:eval(ib([?OP_DEPTH]))),
	?assertEqual([6,6,5,4,3,2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_DEPTH]))).

op_drop() ->
	?assertEqual([1], lib_script:eval(ib([?OP_1, ?OP_0, ?OP_DROP]))).

op_dup() ->
	?assertEqual([0,0,1], lib_script:eval(ib([?OP_1, ?OP_0, ?OP_DUP]))).

op_nip() ->
	?assertEqual([0], lib_script:eval(ib([?OP_1, ?OP_0, ?OP_NIP]))).

op_over() ->
	?assertEqual([1,2,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_OVER]))).


op_pick() ->
	?assertEqual([2,6,5,4,3,2,1],
				 lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_4, ?OP_PICK]))).

op_roll() ->
	?assertEqual([2,6,5,4,3,1],
				 lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_4, ?OP_ROLL]))).

op_rot() ->
	?assertEqual([2,4,3,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_ROT]))).

op_swap() -> ?assertEqual([2,3,1], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_3, ?OP_SWAP]))).
op_tuck() -> ?assertEqual([2,1,2], lib_script:eval(ib([?OP_1, ?OP_2, ?OP_TUCK]))).

op_cat() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_CAT]))).
op_substr() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_SUBSTR]))).
op_left() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_LEFT]))).
op_right() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_RIGHT]))).
op_size() -> ?assertEqual([5, <<1:(8*5)>>], lib_script:eval(ib([5, <<1:(8*5)>>, ?OP_SIZE]))).
op_invert() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_INVERT]))).
op_and() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_AND]))).
op_or() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_OR]))).
op_xor() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_XOR]))).
op_equal() -> 
	?assertEqual([1, 2, 2], lib_script:eval(ib([?OP_2, ?OP_2, ?OP_EQUAL]))),
	?assertEqual([0, 2, 3], lib_script:eval(ib([?OP_3, ?OP_2, ?OP_EQUAL]))).

op_equalverify() ->
	?assertEqual([], lib_script:eval(ib([?OP_2, ?OP_2, ?OP_EQUALVERIFY]))),
	?assertEqual(false, lib_script:eval(ib([?OP_2, ?OP_3, ?OP_EQUALVERIFY]))).

op_reserved() ->
	%% Unexecuted IF
	?assertEqual([2], lib_script:eval(ib([?OP_0, ?OP_IF, ?OP_RESERVED, ?OP_ELSE, ?OP_2, ?OP_ENDIF]))), 
	?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_RESERVED]))).

op_reserved1() ->
	?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_RESERVED1]))).
op_reserved2() ->
	?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_RESERVED2]))).

op_1add() ->
	?assertEqual([2], lib_script:eval(ib([?OP_1, ?OP_1ADD]))).

op_1sub() ->
	?assertEqual([1], lib_script:eval(ib([?OP_2, ?OP_1SUB]))).

op_2mul() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_2MUL]))).
op_2div() -> ?assertEqual(false, lib_script:eval(ib([?OP_1, ?OP_2DIV]))).

op_negate() -> ?assertEqual([-2], lib_script:eval(ib([?OP_2, ?OP_NEGATE]))).
op_abs() -> ?assertEqual([2], lib_script:eval(ib([?OP_2, ?OP_NEGATE, ?OP_ABS]))).
op_not() ->
	?assertEqual([0], lib_script:eval(ib([?OP_1, ?OP_NOT]))),
	?assertEqual([1], lib_script:eval(ib([?OP_0, ?OP_NOT]))),
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_NOT]))).

op_0notequal() -> 
	?assertEqual([0], lib_script:eval(ib([?OP_0, ?OP_0NOTEQUAL]))),
	?assertEqual([1], lib_script:eval(ib([?OP_1, ?OP_0NOTEQUAL]))),
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_0NOTEQUAL]))).

op_add() -> 
	?assertEqual([5], lib_script:eval(ib([?OP_2, ?OP_3, ?OP_ADD]))).

op_sub() ->
	?assertEqual([-1], lib_script:eval(ib([?OP_2, ?OP_3, ?OP_SUB]))).

op_mul() -> ?assertEqual(false, lib_script:eval(ib([?OP_2, ?OP_3, ?OP_MUL]))).
op_div() -> ?assertEqual(false, lib_script:eval(ib([?OP_2, ?OP_3, ?OP_DIV]))).
op_mod() -> ?assertEqual(false, lib_script:eval(ib([?OP_2, ?OP_3, ?OP_MOD]))).
op_lshift() -> ?assertEqual(false, lib_script:eval(ib([?OP_2, ?OP_3, ?OP_LSHIFT]))).
op_rshift() -> ?assertEqual(false, lib_script:eval(ib([?OP_2, ?OP_3, ?OP_RSHIFT]))).
op_booland() ->
	?assertEqual([1], lib_script:eval(ib([?OP_1, ?OP_1, ?OP_BOOLAND]))),
	?assertEqual([0], lib_script:eval(ib([?OP_0, ?OP_1, ?OP_BOOLAND]))),
	?assertEqual([0], lib_script:eval(ib([?OP_1, ?OP_0, ?OP_BOOLAND]))).

op_boolor() ->
	?assertEqual([0], lib_script:eval(ib([?OP_0, ?OP_0, ?OP_BOOLOR]))),
	?assertEqual([1], lib_script:eval(ib([?OP_0, ?OP_1, ?OP_BOOLOR]))),
	?assertEqual([1], lib_script:eval(ib([?OP_1, ?OP_0, ?OP_BOOLOR]))),
	?assertEqual([1], lib_script:eval(ib([?OP_1, ?OP_1, ?OP_BOOLOR]))).

op_numequal() ->
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_NUMEQUAL]))),
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_NUMEQUAL]))).

op_numequalverify() ->
	?assertEqual([], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_NUMEQUALVERIFY]))),
	?assertEqual(false, lib_script:eval(ib([?OP_5, ?OP_6, ?OP_NUMEQUALVERIFY]))).

op_numnotequal() ->
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_NUMNOTEQUAL]))),
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_NUMNOTEQUAL]))).

op_lessthan() ->
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_LESSTHAN]))),
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_LESSTHAN]))),
	?assertEqual([1], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_LESSTHAN]))).

op_greaterthan() ->
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_GREATERTHAN]))),
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_GREATERTHAN]))),
	?assertEqual([0], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_GREATERTHAN]))).

op_lessthanorequal() ->
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_LESSTHANOREQUAL]))),
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_LESSTHANOREQUAL]))),
	?assertEqual([1], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_LESSTHANOREQUAL]))).

op_greaterthanorequal() ->
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_GREATERTHANOREQUAL]))),
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_GREATERTHANOREQUAL]))),
	?assertEqual([0], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_GREATERTHANOREQUAL]))).

op_min() ->
	?assertEqual([5], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_MIN]))),
	?assertEqual([5], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_MIN]))),
	?assertEqual([5], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_MIN]))).

op_max() ->
	?assertEqual([5], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_MAX]))),
	?assertEqual([6], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_MAX]))),
	?assertEqual([6], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_MAX]))).

op_within() ->
	?assertEqual([1], lib_script:eval(ib([?OP_5, ?OP_5, ?OP_10, ?OP_WITHIN]))),
	?assertEqual([1], lib_script:eval(ib([?OP_6, ?OP_5, ?OP_10, ?OP_WITHIN]))),
	?assertEqual([0], lib_script:eval(ib([?OP_5, ?OP_6, ?OP_10, ?OP_WITHIN]))).

op_ripemd160() ->
	?assertEqual([<<92,0,189,74,202,4,169,5,124,9,178,11,5,247,35,242,226,
  61,235,101>>], lib_script:eval(ib([20, <<0:(20*8)>>, ?OP_RIPEMD160]))).

op_sha1() ->
	?assertEqual([<<103,104,3,62,33,100,104,36,123,208,49,160,162,217,135,
  109,121,129,143,143>>], lib_script:eval(ib([20, <<0:(20*8)>>, ?OP_SHA1]))).

op_sha256() ->
	?assertEqual([<<222,71,201,178,126,184,211,0,219,181,242,195,83,230,50,195,147,38,44,240,99,
  64,196,250,127,27,64,196,203,211,111,144>>], lib_script:eval(ib([20, <<0:(20*8)>>, ?OP_SHA256]))).

op_hash160() ->
	?assertEqual([<<148,79,153,124,85,83,166,243,225,2,142,112,124,113,181,
  250,13,211,175,167>>], lib_script:eval(ib([20, <<0:(20*8)>>, ?OP_HASH160]))).

op_hash256() -> 
	?assertEqual([<<246,234,183,185,26,66,52,38,208,109,168,68,52,116,114,153,74,115,140,198,177,
  5,197,250,105,95,116,131,40,24,209,115>>], lib_script:eval(ib([20, <<0:(20*8)>>, ?OP_HASH256]))).

op_codeseparator() -> 
	?assertEqual([], lib_script:eval(ib([?OP_CODESEPARATOR]))).
	
op_checknops() ->
	?assertEqual([], lib_script:eval(ib([?OP_NOP1, ?OP_NOP5, ?OP_NOP10]))).

get_ops() ->
	Script = <<?OP_0>>,
	?assertEqual([op_0], lib_script:ops(Script)).

get_p2pkh() ->
	Script = <<?OP_DUP:8, ?OP_HASH160:8, 16#14:8, 2:160, ?OP_EQUALVERIFY:8, ?OP_CHECKSIG:8>>,
	?assertEqual([op_dup, op_hash160, <<2:160>>, op_equalverify, op_checksig], lib_script:ops(Script)).

input() ->
	Hex = "48304502206ddbbb8642be5296457374b517a1b3249b86ed76f33c40cbbce2cc2b3c443ec8022100a636b8e9260e513c57b34654d69e45980ec1b47837fd907173607f9a2c8ec85b01210271e2e20a8ebc7423ef5cd4a88f57fc03a36b7f22342a7275309e713dd1528ce1",
	[A,B] = lib_script:ops(hex:hexstr_to_bin(Hex)),
	?assertEqual(72, size(A)),
	?assertEqual(33, size(B)).

coinbase_tx() ->
	Bin = hex:hexstr_to_bin("410496b538e853519c726a2c91e61ec11600ae1390813a627c66fb8be7947be63c52da7589379515d4e0a604f8141781e62294721166bf621e73a82cbf2342c858eeac"),
	[_A,B] = lib_script:ops(Bin),
	?assertEqual(op_checksig, B).

validate_p2pkh() ->
	Tx = lib_parse:parse_tx(hex:hexstr_to_bin("0100000001802f6b5f7515e9b11575926b25c340b69964924aaf792a00627edcaab9d3d2bd000000006c493046022100ac7e4e27f2b11cb86fb5aa872ab9d32cdc083380733e3e9847ff77a069cddfab022100c04c3e6ffe88a15bc507b8e571aa35928acfe15a4a23201b08fe3c7b3c97c88f0121024005c945d86ac6b01fb04258345abea7a845bd25689edb723d5ad4068ddd3036ffffffff026073f040000000001976a914406d7324bacc8f7ee02091dedd707770fc3ff38288ac80969800000000001976a91406f1b67078fc400a63d54c313cd6bb817e4760f088ac00000000")),
	TxIndex = 0,
	[I|_] = Tx#btxdef.txinputs,
	ScriptSig = I#btxin.script,
	?assertEqual(ScriptSig, hex:hexstr_to_bin("493046022100ac7e4e27f2b11cb86fb5aa872ab9d32cdc083380733e3e9847ff77a069cddfab022100c04c3e6ffe88a15bc507b8e571aa35928acfe15a4a23201b08fe3c7b3c97c88f0121024005c945d86ac6b01fb04258345abea7a845bd25689edb723d5ad4068ddd3036")),
	Pubkey = hex:hexstr_to_bin("0c1b83d01d0ffb2bccae606963376cca3863a7ce"),
	?debugFmt("Pubkey: ~p~n", [Pubkey]),
	ScriptPubKey = hex:hexstr_to_bin("76a9140c1b83d01d0ffb2bccae606963376cca3863a7ce88ac"),
	?debugFmt("ScriptPubKey ~p~n", [ScriptPubKey]),
	?debugFmt("OPS: ~p~n", [lib_script:ops(<<ScriptSig/binary, ScriptPubKey/binary>>)]),
	?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, TxIndex, Tx)).

script_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
	%{"build and match", fun match/0},
	%{"Simple eval", fun simple_eval/0},
	{"Simple if", fun simple_if/0},
	{"Simple ifelse", fun simple_ifelse/0},
	{"Multiple else", fun multiple_else/0},
	{"Nested ifs", fun nested_ifs/0},
	{"Dangling endif", fun dangling_endif/0},
	{"Dangling else", fun dangling_else/0},
	{"Extra if", fun extra_if/0},
	{"op_notif", fun op_notif/0},
	{"op_verif", fun op_verif/0},
	{"op_notverif", fun op_notverif/0},
	{"op_verify", fun op_verify/0},
	{"op_return", fun op_return/0},
	{"op_altstack", fun op_altstack/0},
	{"op_2drop", fun op_2drop/0},
	{"op_2dup", fun op_2dup/0},
	{"op_3dup", fun op_3dup/0},
	{"op_2over", fun op_2over/0},
	{"op_2rot", fun op_2rot/0},
	{"op_2swap", fun op_2swap/0},
	{"op_ifdup", fun op_ifdup/0},
	{"op_depth", fun op_depth/0},
	{"op_drop", fun op_drop/0},
	{"op_dup", fun op_dup/0},
	{"op_nip", fun op_nip/0},
	{"op_over", fun op_over/0},
	{"op_pick", fun op_pick/0},
	{"op_roll", fun op_roll/0},
	{"op_rot", fun op_rot/0},
	{"op_swap", fun op_swap/0},
	{"op_tuck", fun op_tuck/0},
	{"op_cat", fun op_cat/0},
	{"op_substr", fun op_substr/0},
	{"op_left", fun op_left/0},
	{"op_right", fun op_right/0},
	{"op_size", fun op_size/0},
	{"op_invert", fun op_invert/0},
	{"op_and", fun op_and/0},
	{"op_or", fun op_or/0},
	{"or_xor", fun op_xor/0},
	{"op_equal", fun op_equal/0},
	{"op_equalverify", fun op_equalverify/0},
	{"op_reserved", fun op_reserved/0},
	{"op_reserved1", fun op_reserved1/0},
	{"op_reserved2", fun op_reserved2/0},
	{"op_1add", fun op_1add/0},
	{"op_1sub", fun op_1sub/0},
	{"op_2mul", fun op_2mul/0},
	{"op_2div", fun op_2div/0},
	{"op_negate", fun op_negate/0},
	{"op_abs", fun op_abs/0},
	{"op_not", fun op_not/0},
	{"op_0notequal", fun op_0notequal/0},
	{"op_add", fun op_add/0},
	{"op_sub", fun op_sub/0},
	{"op_mul", fun op_mul/0},
	{"op_div", fun op_div/0},
	{"op_mod", fun op_mod/0},
	{"op_lshift", fun op_lshift/0},
	{"op_rshift", fun op_rshift/0},
	{"op_booland", fun op_booland/0},
	{"op_boolor", fun op_boolor/0},
	{"op_numequal", fun op_numequal/0},
	{"op_numequalverify", fun op_numequalverify/0},
	{"op_numnotequal", fun op_numnotequal/0},
	{"op_lessthan", fun op_lessthan/0},
	{"op_greaterthan", fun op_greaterthan/0},
	{"op_lessthanorequal", fun op_lessthanorequal/0},
	{"op_greaterthanorequal", fun op_greaterthanorequal/0},
	{"op_min", fun op_min/0},
	{"op_max", fun op_max/0},
	{"op_within", fun op_within/0},
	{"op_ripemd160", fun op_ripemd160/0},
	{"op_sha1", fun op_sha1/0},
	{"op_sha256", fun op_sha256/0},
	{"op_hash160", fun op_hash160/0},
	{"op_hash256", fun op_hash256/0},
	{"op_codeseparator", fun op_codeseparator/0},
	{"op_check nops range", fun op_checknops/0}
		%{"Get ops", fun get_ops/0},
		%{"Get p2pkh", fun get_p2pkh/0},
	    %{"Input", fun input/0},
		%{"Coinbase tx", fun coinbase_tx/0},
	%{"validate p2pkh", fun validate_p2pkh/0} 
   ]
  }.


ib(X) -> iolist_to_binary(X). 
