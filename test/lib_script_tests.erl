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
	?assertEqual([6,5,4,3,2], e(Script)).

simple_if() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF, ?OP_2, ?OP_ENDIF]),
	Stack = e(Script),
	?assertEqual([2], Stack).

simple_ifelse() ->
	Script = iolist_to_binary([?OP_0, ?OP_IF, ?OP_2, ?OP_ELSE, ?OP_3, ?OP_ENDIF]),
	Stack = e(Script),
	?assertEqual([3], Stack).

multiple_else() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF, ?OP_2, ?OP_ELSE, ?OP_3, ?OP_ELSE, ?OP_4,
							   ?OP_ELSE, ?OP_5, ?OP_ELSE, ?OP_6, ?OP_ENDIF]),
	Stack = e(Script),
	?assertEqual([6,4,2], Stack).

nested_ifs() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF, ?OP_1, ?OP_IF, ?OP_2, ?OP_ELSE, ?OP_3, ?OP_ELSE, ?OP_4,
			  ?OP_ELSE, ?OP_5, ?OP_ELSE, ?OP_6, ?OP_ENDIF, ?OP_ENDIF]),
	Stack = e(Script),
	?assertEqual([6,4,2], Stack).

dangling_endif() ->
	Script = iolist_to_binary([?OP_1, ?OP_2, ?OP_ENDIF]),
	?assertEqual(false, e(Script)).

dangling_else()->
	Script = iolist_to_binary([?OP_2, ?OP_ELSE]),
	?assertEqual(false, e(Script)).

extra_if() ->
	Script = iolist_to_binary([?OP_1, ?OP_IF]),
	?assertEqual(false, e(Script)).

op_notif() ->
	Script = ib([?OP_0, ?OP_NOTIF, ?OP_2, ?OP_ENDIF]),
	?assertEqual([2], e(Script)).

op_verif() ->
	Script = ib([?OP_VERIF]),
	?assertEqual(false, e(Script)).

op_notverif() ->
	Script = ib([?OP_VERNOTIF]),
	?assertEqual(false, e(Script)).

op_verify() ->
	?assertEqual([1], e(ib([?OP_1, ?OP_VERIFY]))),
	?assertEqual(false, e(ib([?OP_VERIFY]))),
	?assertEqual(false, e(ib([?OP_0, ?OP_VERIFY]))).

op_return() ->
	?assertEqual(false, e(ib([?OP_RETURN]))).

op_altstack() ->
	?assertEqual(false, e(ib([?OP_FROMALTSTACK]))),
	?assertEqual([2], e(ib([?OP_2, ?OP_TOALTSTACK, ?OP_FROMALTSTACK]))).

op_2drop() ->
	?assertEqual([2,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_2DROP]))).

op_2dup() -> 
	?assertEqual([4,3,4,3,2,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_2DUP]))).

op_3dup() ->
	?assertEqual([4,3,2,4,3,2,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_3DUP]))).

op_2over() ->
	?assertEqual([2,1,4,3,2,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_2OVER]))).

op_2rot() ->
	?assertEqual([2,1,6,5,4,3], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_2ROT]))).

op_2swap() ->
	?assertEqual([4,3,6,5,2,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_2SWAP]))).

op_ifdup() ->
	?assertEqual([4,4], e(ib([?OP_4, ?OP_IFDUP]))),
	?assertEqual([0], e(ib([?OP_0, ?OP_IFDUP]))).

op_depth() ->
	?assertEqual([0], e(ib([?OP_DEPTH]))),
	?assertEqual([6,6,5,4,3,2,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_DEPTH]))).

op_drop() ->
	?assertEqual([1], e(ib([?OP_1, ?OP_0, ?OP_DROP]))).

op_dup() ->
	?assertEqual([0,0,1], e(ib([?OP_1, ?OP_0, ?OP_DUP]))).

op_nip() ->
	?assertEqual([0], e(ib([?OP_1, ?OP_0, ?OP_NIP]))).

op_over() ->
	?assertEqual([1,2,1], e(ib([?OP_1, ?OP_2, ?OP_OVER]))).


op_pick() ->
	?assertEqual([2,6,5,4,3,2,1],
				 e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_4, ?OP_PICK]))).

op_roll() ->
	?assertEqual([2,6,5,4,3,1],
				 e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_5, ?OP_6, ?OP_4, ?OP_ROLL]))).

op_rot() ->
	?assertEqual([2,4,3,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_4, ?OP_ROT]))).

op_swap() -> ?assertEqual([2,3,1], e(ib([?OP_1, ?OP_2, ?OP_3, ?OP_SWAP]))).
op_tuck() -> ?assertEqual([2,1,2], e(ib([?OP_1, ?OP_2, ?OP_TUCK]))).

op_cat() -> ?assertEqual(false, e(ib([?OP_1, ?OP_CAT]))).
op_substr() -> ?assertEqual(false, e(ib([?OP_1, ?OP_SUBSTR]))).
op_left() -> ?assertEqual(false, e(ib([?OP_1, ?OP_LEFT]))).
op_right() -> ?assertEqual(false, e(ib([?OP_1, ?OP_RIGHT]))).
op_size() -> ?assertEqual([5, <<1:(8*5)>>], e(ib([5, <<1:(8*5)>>, ?OP_SIZE]))).
op_invert() -> ?assertEqual(false, e(ib([?OP_1, ?OP_INVERT]))).
op_and() -> ?assertEqual(false, e(ib([?OP_1, ?OP_AND]))).
op_or() -> ?assertEqual(false, e(ib([?OP_1, ?OP_OR]))).
op_xor() -> ?assertEqual(false, e(ib([?OP_1, ?OP_XOR]))).
op_equal() -> 
	?assertEqual([1, 2, 2], e(ib([?OP_2, ?OP_2, ?OP_EQUAL]))),
	?assertEqual([0, 2, 3], e(ib([?OP_3, ?OP_2, ?OP_EQUAL]))).

op_equalverify() ->
	?assertEqual([], e(ib([?OP_2, ?OP_2, ?OP_EQUALVERIFY]))),
	?assertEqual(false, e(ib([?OP_2, ?OP_3, ?OP_EQUALVERIFY]))).

op_reserved() ->
	%% Unexecuted IF
	?assertEqual([2], e(ib([?OP_0, ?OP_IF, ?OP_RESERVED, ?OP_ELSE, ?OP_2, ?OP_ENDIF]))), 
	?assertEqual(false, e(ib([?OP_1, ?OP_RESERVED]))).

op_reserved1() ->
	?assertEqual(false, e(ib([?OP_1, ?OP_RESERVED1]))).
op_reserved2() ->
	?assertEqual(false, e(ib([?OP_1, ?OP_RESERVED2]))).

op_1add() ->
	?assertEqual([2], e(ib([?OP_1, ?OP_1ADD]))).

op_1sub() ->
	?assertEqual([1], e(ib([?OP_2, ?OP_1SUB]))).

op_2mul() -> ?assertEqual(false, e(ib([?OP_1, ?OP_2MUL]))).
op_2div() -> ?assertEqual(false, e(ib([?OP_1, ?OP_2DIV]))).

op_negate() -> ?assertEqual([-2], e(ib([?OP_2, ?OP_NEGATE]))).
op_abs() -> ?assertEqual([2], e(ib([?OP_2, ?OP_NEGATE, ?OP_ABS]))).
op_not() ->
	?assertEqual([0], e(ib([?OP_1, ?OP_NOT]))),
	?assertEqual([1], e(ib([?OP_0, ?OP_NOT]))),
	?assertEqual([0], e(ib([?OP_5, ?OP_NOT]))).

op_0notequal() -> 
	?assertEqual([0], e(ib([?OP_0, ?OP_0NOTEQUAL]))),
	?assertEqual([1], e(ib([?OP_1, ?OP_0NOTEQUAL]))),
	?assertEqual([1], e(ib([?OP_5, ?OP_0NOTEQUAL]))).

op_add() -> 
	?assertEqual([5], e(ib([?OP_2, ?OP_3, ?OP_ADD]))).

op_sub() ->
	?assertEqual([-1], e(ib([?OP_2, ?OP_3, ?OP_SUB]))).

op_mul() -> ?assertEqual(false, e(ib([?OP_2, ?OP_3, ?OP_MUL]))).
op_div() -> ?assertEqual(false, e(ib([?OP_2, ?OP_3, ?OP_DIV]))).
op_mod() -> ?assertEqual(false, e(ib([?OP_2, ?OP_3, ?OP_MOD]))).
op_lshift() -> ?assertEqual(false, e(ib([?OP_2, ?OP_3, ?OP_LSHIFT]))).
op_rshift() -> ?assertEqual(false, e(ib([?OP_2, ?OP_3, ?OP_RSHIFT]))).
op_booland() ->
	?assertEqual([1], e(ib([?OP_1, ?OP_1, ?OP_BOOLAND]))),
	?assertEqual([0], e(ib([?OP_0, ?OP_1, ?OP_BOOLAND]))),
	?assertEqual([0], e(ib([?OP_1, ?OP_0, ?OP_BOOLAND]))).

op_boolor() ->
	?assertEqual([0], e(ib([?OP_0, ?OP_0, ?OP_BOOLOR]))),
	?assertEqual([1], e(ib([?OP_0, ?OP_1, ?OP_BOOLOR]))),
	?assertEqual([1], e(ib([?OP_1, ?OP_0, ?OP_BOOLOR]))),
	?assertEqual([1], e(ib([?OP_1, ?OP_1, ?OP_BOOLOR]))).

op_numequal() ->
	?assertEqual([1], e(ib([?OP_5, ?OP_5, ?OP_NUMEQUAL]))),
	?assertEqual([0], e(ib([?OP_5, ?OP_6, ?OP_NUMEQUAL]))).

op_numequalverify() ->
	?assertEqual([], e(ib([?OP_5, ?OP_5, ?OP_NUMEQUALVERIFY]))),
	?assertEqual(false, e(ib([?OP_5, ?OP_6, ?OP_NUMEQUALVERIFY]))).

op_numnotequal() ->
	?assertEqual([0], e(ib([?OP_5, ?OP_5, ?OP_NUMNOTEQUAL]))),
	?assertEqual([1], e(ib([?OP_5, ?OP_6, ?OP_NUMNOTEQUAL]))).

op_lessthan() ->
	?assertEqual([0], e(ib([?OP_5, ?OP_5, ?OP_LESSTHAN]))),
	?assertEqual([0], e(ib([?OP_5, ?OP_6, ?OP_LESSTHAN]))),
	?assertEqual([1], e(ib([?OP_6, ?OP_5, ?OP_LESSTHAN]))).

op_greaterthan() ->
	?assertEqual([0], e(ib([?OP_5, ?OP_5, ?OP_GREATERTHAN]))),
	?assertEqual([1], e(ib([?OP_5, ?OP_6, ?OP_GREATERTHAN]))),
	?assertEqual([0], e(ib([?OP_6, ?OP_5, ?OP_GREATERTHAN]))).

op_lessthanorequal() ->
	?assertEqual([1], e(ib([?OP_5, ?OP_5, ?OP_LESSTHANOREQUAL]))),
	?assertEqual([0], e(ib([?OP_5, ?OP_6, ?OP_LESSTHANOREQUAL]))),
	?assertEqual([1], e(ib([?OP_6, ?OP_5, ?OP_LESSTHANOREQUAL]))).

op_greaterthanorequal() ->
	?assertEqual([1], e(ib([?OP_5, ?OP_5, ?OP_GREATERTHANOREQUAL]))),
	?assertEqual([1], e(ib([?OP_5, ?OP_6, ?OP_GREATERTHANOREQUAL]))),
	?assertEqual([0], e(ib([?OP_6, ?OP_5, ?OP_GREATERTHANOREQUAL]))).

op_min() ->
	?assertEqual([5], e(ib([?OP_5, ?OP_5, ?OP_MIN]))),
	?assertEqual([5], e(ib([?OP_5, ?OP_6, ?OP_MIN]))),
	?assertEqual([5], e(ib([?OP_6, ?OP_5, ?OP_MIN]))).

op_max() ->
	?assertEqual([5], e(ib([?OP_5, ?OP_5, ?OP_MAX]))),
	?assertEqual([6], e(ib([?OP_5, ?OP_6, ?OP_MAX]))),
	?assertEqual([6], e(ib([?OP_6, ?OP_5, ?OP_MAX]))).

op_within() ->
	?assertEqual([1], e(ib([?OP_5, ?OP_5, ?OP_10, ?OP_WITHIN]))),
	?assertEqual([1], e(ib([?OP_6, ?OP_5, ?OP_10, ?OP_WITHIN]))),
	?assertEqual([0], e(ib([?OP_5, ?OP_6, ?OP_10, ?OP_WITHIN]))).

op_ripemd160() ->
	?assertEqual([<<92,0,189,74,202,4,169,5,124,9,178,11,5,247,35,242,226,
  61,235,101>>], e(ib([20, <<0:(20*8)>>, ?OP_RIPEMD160]))).

op_sha1() ->
	?assertEqual([<<103,104,3,62,33,100,104,36,123,208,49,160,162,217,135,
  109,121,129,143,143>>], e(ib([20, <<0:(20*8)>>, ?OP_SHA1]))).

op_sha256() ->
	?assertEqual([<<222,71,201,178,126,184,211,0,219,181,242,195,83,230,50,195,147,38,44,240,99,
  64,196,250,127,27,64,196,203,211,111,144>>], e(ib([20, <<0:(20*8)>>, ?OP_SHA256]))).

op_hash160() ->
	?assertEqual([<<148,79,153,124,85,83,166,243,225,2,142,112,124,113,181,
  250,13,211,175,167>>], e(ib([20, <<0:(20*8)>>, ?OP_HASH160]))).

op_hash256() -> 
	?assertEqual([<<246,234,183,185,26,66,52,38,208,109,168,68,52,116,114,153,74,115,140,198,177,
  5,197,250,105,95,116,131,40,24,209,115>>], e(ib([20, <<0:(20*8)>>, ?OP_HASH256]))).

op_codeseparator() -> 
	?assertEqual([], e(ib([?OP_CODESEPARATOR]))).
	
op_checknops() ->
	?assertEqual([], e(ib([?OP_NOP1, ?OP_NOP5, ?OP_NOP10]))).

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
	ScriptPubKey = hex:hexstr_to_bin("76a9140c1b83d01d0ffb2bccae606963376cca3863a7ce88ac"),
	?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, TxIndex, Tx)).

scripts1() ->
	%% Multiple Inputs (Index off by one error)
	ScriptPubKey = <<118,169,20,192,250,89,158,131,18,148,15,33,169,66,196,223,193,255,
             166,24,72,167,192,136,172>>,
    ScriptSig = <<71,48,68,2,32,32,191,138,54,200,105,44,162,228,175,61,36,187,157,79,
           205,95,54,169,117,143,50,173,87,102,169,38,183,173,17,251,155,2,32,
           10,56,50,174,86,160,58,220,159,120,46,37,59,37,35,134,114,118,70,
           162,30,98,139,69,97,239,201,65,37,127,237,33,1,65,4,238,18,95,206,
           151,120,137,14,140,214,24,192,70,210,107,182,83,220,182,162,152,75,
           114,79,167,234,9,130,212,210,33,138,202,115,222,187,165,178,89,128,
           6,218,241,169,102,6,129,229,99,2,105,189,15,161,104,202,46,169,210,
           72,180,7,108,225>>,
    Tx = lib_parse:parse_tx(<<1,0,0,0,2,173,227,140,223,127,85,144,158,90,22,84,31,169,226,56,164,56,
       234,13,217,216,81,81,85,6,44,169,155,73,24,18,14,0,0,0,0,138,71,48,68,2,
       32,32,191,138,54,200,105,44,162,228,175,61,36,187,157,79,205,95,54,169,
       117,143,50,173,87,102,169,38,183,173,17,251,155,2,32,10,56,50,174,86,
       160,58,220,159,120,46,37,59,37,35,134,114,118,70,162,30,98,139,69,97,
       239,201,65,37,127,237,33,1,65,4,238,18,95,206,151,120,137,14,140,214,24,
       192,70,210,107,182,83,220,182,162,152,75,114,79,167,234,9,130,212,210,
       33,138,202,115,222,187,165,178,89,128,6,218,241,169,102,6,129,229,99,2,
       105,189,15,161,104,202,46,169,210,72,180,7,108,225,255,255,255,255,173,
       227,140,223,127,85,144,158,90,22,84,31,169,226,56,164,56,234,13,217,216,
       81,81,85,6,44,169,155,73,24,18,14,1,0,0,0,139,72,48,69,2,33,0,135,200,
       10,101,177,119,125,198,42,187,78,199,121,60,215,121,105,118,52,130,194,
       223,222,123,198,122,39,25,84,45,125,61,2,32,105,158,226,83,12,220,169,
       38,27,250,13,164,43,45,30,198,96,219,230,208,34,59,84,195,39,147,64,143,
       199,199,4,24,1,65,4,8,159,227,18,174,191,226,195,197,119,214,126,195,
       215,15,36,192,87,40,223,141,81,233,151,169,235,149,206,131,0,13,172,167,
       80,214,52,36,26,59,20,169,47,126,243,155,183,86,80,211,74,40,53,231,84,
       161,99,235,149,212,136,117,191,9,179,255,255,255,255,2,64,210,223,3,0,0,
       0,0,25,118,169,20,124,220,108,179,40,251,82,30,193,52,209,118,56,239,76,
       182,84,143,206,20,136,172,128,86,248,59,4,0,0,0,25,118,169,20,217,159,
       250,243,224,20,237,121,209,148,187,128,243,100,159,109,220,30,0,89,136,
       172,0,0,0,0>>),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 0, Tx)). 

scripts2() ->
	%% Include OP_CODESEPARATOR in SCRIPTSIG PUSH
	ScriptPubKey = <<118,169,20,238,215,163,26,45,236,185,182,124,171,72,34,49,106,73,99,
           128,232,156,2,136,172>>, 
    ScriptSig = <<70,48,67,2,31,1,204,226,152,176,61,236,9,181,122,37,172,16,21,174,
             194,113,146,239,81,142,233,189,36,89,45,94,175,186,64,179,2,32,44,
             137,44,70,211,194,130,141,170,240,112,144,198,135,96,14,234,6,206,
             61,244,253,55,110,133,24,92,126,44,191,51,163,1,65,4,91,184,21,36,
             239,213,194,83,251,111,25,71,14,227,118,64,243,132,68,241,97,82,
             179,97,202,46,165,71,145,41,79,22,19,223,155,183,160,31,222,35,49,
             222,36,215,84,122,188,58,231,164,150,125,132,242,158,203,128,179,
             237,49,115,157,52,148>>,
    Tx = lib_parse:parse_tx(<<1,0,0,0,2,30,73,37,79,101,33,5,128,182,254,131,109,11,179,90,84,201,97,
       11,243,51,151,21,109,140,92,137,213,151,155,206,126,0,0,0,0,139,72,48,
       69,2,33,0,165,219,70,105,230,61,46,124,123,12,109,126,15,44,183,102,125,
       95,34,125,83,59,224,204,22,213,81,229,74,85,80,235,2,32,39,14,189,126,
       111,6,231,201,4,175,255,194,247,153,225,173,28,255,68,113,1,49,244,93,
       83,122,126,164,103,52,169,19,1,65,4,128,159,55,185,126,235,4,222,122,27,
       113,217,190,20,32,141,125,233,44,167,164,247,44,35,135,169,181,213,95,
       99,14,242,212,199,81,130,88,109,238,14,190,149,88,101,10,128,172,229,90,
       105,224,151,204,230,157,30,145,210,76,15,53,121,201,10,255,255,255,255,
       30,73,37,79,101,33,5,128,182,254,131,109,11,179,90,84,201,97,11,243,51,
       151,21,109,140,92,137,213,151,155,206,126,1,0,0,0,137,70,48,67,2,31,1,
       204,226,152,176,61,236,9,181,122,37,172,16,21,174,194,113,146,239,81,
       142,233,189,36,89,45,94,175,186,64,179,2,32,44,137,44,70,211,194,130,
       141,170,240,112,144,198,135,96,14,234,6,206,61,244,253,55,110,133,24,92,
       126,44,191,51,163,1,65,4,91,184,21,36,239,213,194,83,251,111,25,71,14,
       227,118,64,243,132,68,241,97,82,179,97,202,46,165,71,145,41,79,22,19,
       223,155,183,160,31,222,35,49,222,36,215,84,122,188,58,231,164,150,125,
       132,242,158,203,128,179,237,49,115,157,52,148,255,255,255,255,2,128,95,
       17,72,3,0,0,0,25,118,169,20,229,244,183,127,168,71,165,188,118,1,216,55,
       169,157,3,174,144,123,136,173,136,172,0,132,4,73,1,0,0,0,25,118,169,20,
       5,105,83,25,40,199,199,4,130,235,121,132,47,102,171,98,46,64,72,139,136,
       172,0,0,0,0>>),
    %?debugFmt("YYY ~p~n", [Tx]),
    %?debugFmt("XXX ~p~n", [lib_script:ops(SubScriptBin)]),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 1, Tx)). 


scripts3() ->
	%% OP_CHECKMULTISIG + CODESEPARATOR in Input Script
	ScriptPubKey = <<20,42,155,197,68,125,102,76,29,1,65,57,42,132,45,35,219,164,92,79,
           19,177,117>>,
    ScriptSig = <<0,71,48,68,2,32,39,109,109,173,61,239,163,123,95,129,173,211,153,
             45,81,13,47,68,163,23,253,133,224,79,147,161,226,218,234,100,102,
             2,2,32,15,134,42,13,166,132,36,147,34,206,184,237,132,47,184,200,
             89,192,203,148,200,30,28,83,8,180,134,129,87,164,40,238,1,171,81,
             33,2,50,171,220,137,62,127,6,49,54,77,127,208,28,179,61,36,218,69,
             50,154,0,53,123,58,120,134,33,26,180,20,213,90,81,174>>,
    Tx = lib_parse:parse_tx(<<1,0,0,0,2,77,232,176,196,194,88,45,185,95,166,179,86,122,152,155,102,68,
       132,199,173,102,114,200,90,61,164,19,119,62,99,253,184,0,0,0,0,107,72,
       48,69,2,32,91,40,47,188,155,6,79,59,200,35,162,62,220,192,4,140,187,23,
       71,84,231,170,116,46,60,159,72,62,190,2,145,28,2,33,0,228,176,179,161,
       23,211,108,171,90,103,64,77,221,191,67,219,123,234,60,21,48,224,254,18,
       142,188,21,98,27,214,154,59,1,33,3,90,169,141,95,119,205,154,45,136,113,
       14,111,198,98,18,175,248,32,2,111,13,173,143,50,209,247,206,135,69,125,
       222,80,255,255,255,255,77,232,176,196,194,88,45,185,95,166,179,86,122,
       152,155,102,68,132,199,173,102,114,200,90,61,164,19,119,62,99,253,184,1,
       0,0,0,111,0,71,48,68,2,32,39,109,109,173,61,239,163,123,95,129,173,211,
       153,45,81,13,47,68,163,23,253,133,224,79,147,161,226,218,234,100,102,2,
       2,32,15,134,42,13,166,132,36,147,34,206,184,237,132,47,184,200,89,192,
       203,148,200,30,28,83,8,180,134,129,87,164,40,238,1,171,81,33,2,50,171,
       220,137,62,127,6,49,54,77,127,208,28,179,61,36,218,69,50,154,0,53,123,
       58,120,134,33,26,180,20,213,90,81,174,255,255,255,255,2,224,253,28,0,0,
       0,0,0,25,118,169,20,56,12,179,197,148,222,78,126,155,142,24,219,24,41,
       135,190,187,90,79,112,136,172,192,198,45,0,0,0,0,0,23,20,42,155,197,68,
       125,102,76,29,1,65,57,42,132,45,35,219,164,92,79,19,177,117,0,0,0,0>>),
    %?debugFmt("YYY ~p~n", [lib_script:ops(ScriptSig)]),
    %?debugFmt("XXX ~p~n", [lib_script:ops(SubScriptBin)]),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 1, Tx)). 

sighash0() ->
	%TX SIGHASHTYPE 0 (SIGHASH OLD)
	ScriptSig = <<73,48,70,2,33,0,210,52,89,208,62,215,233,81,26,71,209,50,146,211,
             67,10,4,98,125,230,35,91,110,81,164,15,156,211,134,242,171,227,2,
             33,0,231,210,91,8,15,11,184,216,213,248,120,187,167,213,74,210,
             253,166,80,234,141,21,138,51,238,60,189,17,118,129,145,253,0,65,4,
             176,226,200,121,228,218,247,185,171,104,53,2,40,193,89,118,102,
             118,161,79,88,21,8,75,161,102,67,42,171,70,25,141,76,202,152,250,
             62,153,129,208,169,11,46,255,197,20,183,98,121,71,101,80,186,54,
             99,253,202,255,148,195,132,32,233,213>>,
    ScriptPubKey = <<118,169,20,220,68,177,22,65,136,6,124,58,50,212,120,15,89,150,250,
           20,164,242,217,136,172>>,
    Tx = lib_parse:parse_tx(<<1,0,0,0,1,2,118,183,107,7,244,147,92,112,172,245,79,191,31,67,138,76,57,
       122,159,183,230,51,135,60,77,211,188,6,43,107,64,0,0,0,0,140,73,48,70,2,
       33,0,210,52,89,208,62,215,233,81,26,71,209,50,146,211,67,10,4,98,125,
       230,35,91,110,81,164,15,156,211,134,242,171,227,2,33,0,231,210,91,8,15,
       11,184,216,213,248,120,187,167,213,74,210,253,166,80,234,141,21,138,51,
       238,60,189,17,118,129,145,253,0,65,4,176,226,200,121,228,218,247,185,
       171,104,53,2,40,193,89,118,102,118,161,79,88,21,8,75,161,102,67,42,171,
       70,25,141,76,202,152,250,62,153,129,208,169,11,46,255,197,20,183,98,121,
       71,101,80,186,54,99,253,202,255,148,195,132,32,233,213,0,0,0,0,1,0,9,61,
       0,0,0,0,0,25,118,169,20,154,123,15,59,128,198,186,174,237,206,10,8,66,
       85,56,0,248,50,186,31,136,172,0,0,0,0>>),
    %?debugFmt("YYY ~p~n", [lib_script:ops(ScriptSig)]),
    %%?debugFmt("XXX ~p~n", [lib_script:ops(SubScriptBin)]),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 0, Tx)). 

scripts4() ->
	%% Weird push data semantics.
	ScriptSig = <<76,123,48,69,2,33,0,153,214,245,137,126,236,111,44,74,235,60,203,
             67,220,25,244,95,74,67,55,47,214,138,158,131,91,236,70,49,89,230,
             98,2,32,54,93,85,77,135,214,86,144,122,246,169,201,135,104,144,12,
             126,140,251,211,53,45,16,140,72,211,131,205,107,8,246,160,42,42,
             42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,
             42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,
             42,42,42,42,42,1,65,4,191,229,150,103,231,198,202,230,221,68,216,
             237,100,108,73,163,129,134,24,156,244,168,76,187,107,86,63,169,20,
             209,84,155,77,28,173,1,109,186,101,176,110,21,78,30,100,112,175,
             150,212,97,147,20,48,53,95,108,6,50,172,141,165,213,240,38>>,
	ScriptPubKey = <<118,169,20,48,148,166,75,251,214,180,212,176,6,22,232,221,234,67,87,
 				  206,240,70,232,136,172>>,
 	Tx = lib_parse:parse_tx(<<1,0,0,0,1,115,63,204,61,61,147,237,88,5,115,128,167,11,254,163,42,198,
       157,255,71,124,156,18,120,135,17,170,76,254,10,65,147,1,0,0,0,191,76,
       123,48,69,2,33,0,153,214,245,137,126,236,111,44,74,235,60,203,67,220,25,
       244,95,74,67,55,47,214,138,158,131,91,236,70,49,89,230,98,2,32,54,93,85,
       77,135,214,86,144,122,246,169,201,135,104,144,12,126,140,251,211,53,45,
       16,140,72,211,131,205,107,8,246,160,42,42,42,42,42,42,42,42,42,42,42,42,
       42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,
       42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,1,65,4,191,229,150,103,231,
       198,202,230,221,68,216,237,100,108,73,163,129,134,24,156,244,168,76,187,
       107,86,63,169,20,209,84,155,77,28,173,1,109,186,101,176,110,21,78,30,
       100,112,175,150,212,97,147,20,48,53,95,108,6,50,172,141,165,213,240,38,
       255,255,255,255,2,128,195,201,1,0,0,0,0,25,118,169,20,244,11,131,234,97,
       1,121,85,148,225,133,50,53,76,213,127,130,75,176,131,136,172,128,68,229,
       45,0,0,0,0,25,118,169,20,245,190,69,163,180,152,140,71,34,164,164,8,107,
       68,180,54,214,64,86,35,136,172,0,0,0,0>>),
    %?debugFmt("YYY ~p~n", [lib_script:ops(ScriptSig)]),
    %?debugFmt("~p~n", [lib_tx:readable_txhash(binary, Tx)]),
    %?debugFmt("YYY ~p~n", [hex:bin_to_hexstr(ScriptSig)]),
    %?debugFmt("XXX ~p~n", [lib_script:ops(ScriptSig)]),
    %?debugFmt("XXX ~p~n", [Stack]),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 0, Tx)). 

sighash_none() ->
	%TX SIGHASHTYPE 2
	%ALso, strange DER encoding for R/S ECDSA values
	%http://bitcoin.stackexchange.com/questions/37469/blockchain-checksig-fail
	ScriptSig = <<71,48,68,2,32,187,79,188,73,90,162,59,171,178,194,190,78,63,180,
             165,223,254,254,32,200,239,245,148,15,19,86,73,195,234,150,68,74,
             2,32,4,175,205,169,102,200,7,187,151,98,45,62,239,234,130,143,98,
             58,243,6,239,43,117,103,130,238,111,138,34,169,89,162,2,65,4,241,
             147,154,230,176,30,132,155,240,93,14,213,31,213,185,43,121,160,
             227,19,227,243,137,199,38,241,31,163,225,68,217,34,123,7,232,168,
             124,14,227,99,114,233,103,224,144,209,27,119,119,7,170,115,239,
             172,171,255,255,162,133,192,11,54,34,214>>,
    ScriptPubKey = <<118,169,20,33,196,60,228,0,144,19,18,166,3,228,32,122,173,253,116,
           43,232,231,218,136,172>>,
    Tx = lib_parse:parse_tx(<<1,0,0,0,1,95,56,108,138,56,66,201,169,220,250,155,120,190,120,90,64,167,
       189,160,139,100,100,107,227,101,67,1,234,204,252,141,94,1,0,0,0,138,71,
       48,68,2,32,187,79,188,73,90,162,59,171,178,194,190,78,63,180,165,223,
       254,254,32,200,239,245,148,15,19,86,73,195,234,150,68,74,2,32,4,175,205,
       169,102,200,7,187,151,98,45,62,239,234,130,143,98,58,243,6,239,43,117,
       103,130,238,111,138,34,169,89,162,2,65,4,241,147,154,230,176,30,132,155,
       240,93,14,213,31,213,185,43,121,160,227,19,227,243,137,199,38,241,31,
       163,225,68,217,34,123,7,232,168,124,14,227,99,114,233,103,224,144,209,
       27,119,119,7,170,115,239,172,171,255,255,162,133,192,11,54,34,214,255,
       255,255,255,2,64,66,15,0,0,0,0,0,25,118,169,20,102,13,78,243,167,67,227,
       230,150,173,153,3,100,229,85,194,113,173,80,75,136,172,32,114,200,1,0,0,
       0,0,25,118,169,20,33,196,60,228,0,144,19,18,166,3,228,32,122,173,253,
       116,43,232,231,218,136,172,0,0,0,0>>),
    %?debugFmt("XXX ~p~n", [lib_script:ops(ScriptPubKey)]),
    %?debugFmt("YYY ~p~n", [hex:bin_to_hexstr(ScriptPubKey)]),
    %?debugFmt("XXX ~p~n", [Stack]),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 0, Tx)). 

scripts5() ->
	%NEW TX SIGHASHTYPE 129
	%Should fall through to default signing
	%and then SIGHASH_ANYONECANPAY
	ScriptSig = <<72,48,69,2,32,88,83,199,241,57,87,133,191,171,176,60,87,233,98,
             235,7,111,242,77,142,78,87,59,4,219,19,180,94,211,237,110,226,2,
             33,0,157,200,42,228,59,233,212,177,254,40,71,117,78,29,54,218,212,
             139,168,1,129,125,72,93,197,41,175,197,22,194,221,180,129,33,3,5,
             88,73,128,54,123,50,31,173,127,28,31,77,93,114,61,10,200,12,29,
             128,200,186,18,52,57,101,180,131,100,83,122>>,
    ScriptPubKey = <<118,169,20,133,81,228,138,83,222,205,28,252,99,7,154,69,129,188,204,
           250,209,169,60,136,172>>,
 Tx = lib_parse:parse_tx(<<1,0,0,0,2,246,4,76,10,212,133,246,51,180,31,151,208,215,147,235,40,55,
       174,64,247,56,255,109,95,80,253,253,16,82,140,29,118,1,0,0,0,107,72,48,
       69,2,32,88,83,199,241,57,87,133,191,171,176,60,87,233,98,235,7,111,242,
       77,142,78,87,59,4,219,19,180,94,211,237,110,226,2,33,0,157,200,42,228,
       59,233,212,177,254,40,71,117,78,29,54,218,212,139,168,1,129,125,72,93,
       197,41,175,197,22,194,221,180,129,33,3,5,88,73,128,54,123,50,31,173,127,
       28,31,77,93,114,61,10,200,12,29,128,200,186,18,52,57,101,180,131,100,83,
       122,255,255,255,255,156,106,240,223,102,105,188,222,209,158,49,126,37,
       190,188,140,120,228,141,248,174,31,224,42,127,3,8,24,231,30,205,64,1,0,
       0,0,108,73,48,70,2,33,0,130,105,201,215,186,10,126,115,13,209,111,64,
       130,210,158,54,132,251,116,99,186,6,79,208,147,175,193,112,173,110,3,
       136,2,33,0,188,109,118,55,57,22,163,255,110,228,27,44,117,32,1,253,163,
       201,224,72,188,255,13,129,208,91,57,255,15,66,23,178,129,33,3,170,227,3,
       216,37,66,21,69,197,188,124,205,90,200,125,213,173,211,188,195,164,50,
       186,122,162,242,102,22,153,249,246,89,255,255,255,255,1,224,147,4,0,0,0,
       0,0,25,118,169,20,92,17,249,23,136,59,146,126,239,119,220,87,112,122,
       235,133,63,109,56,148,136,172,0,0,0,0>> ),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 0, Tx)). 

sighash_single() ->
	%% SIGHASH_SINGLE
	ScriptSig = <<71,48,68,2,32,54,13,32,186,255,56,32,89,4,11,169,190,152,148,127,
             214,120,251,8,170,178,187,12,23,46,250,153,111,216,236,233,183,2,
             32,27,79,176,222,103,240,21,201,14,122,200,161,147,174,171,72,106,
             31,88,126,15,84,208,251,149,82,239,127,92,230,202,236,3,33,3,87,
             156,162,230,209,7,82,47,1,44,208,11,82,185,166,95,180,111,12,87,
             185,184,182,227,119,196,143,82,106,68,116,26>>,
 ScriptPubKey = <<118,169,20,220,247,44,79,208,47,90,152,124,249,176,47,47,171,252,
           172,51,65,168,125,136,172>>,
 Tx = lib_parse:parse_tx(<<1,0,0,0,3,112,172,10,26,229,136,170,242,132,195,8,214,124,169,44,105,
       163,158,45,184,19,55,229,99,191,64,197,157,160,165,207,99,0,0,0,0,106,
       71,48,68,2,32,54,13,32,186,255,56,32,89,4,11,169,190,152,148,127,214,
       120,251,8,170,178,187,12,23,46,250,153,111,216,236,233,183,2,32,27,79,
       176,222,103,240,21,201,14,122,200,161,147,174,171,72,106,31,88,126,15,
       84,208,251,149,82,239,127,92,230,202,236,3,33,3,87,156,162,230,209,7,82,
       47,1,44,208,11,82,185,166,95,180,111,12,87,185,184,182,227,119,196,143,
       82,106,68,116,26,255,255,255,255,125,129,91,100,71,227,95,190,160,151,
       224,14,2,143,183,223,186,212,243,240,152,123,71,52,103,108,132,243,252,
       208,232,4,1,0,0,0,107,72,48,69,2,33,0,199,20,49,11,225,227,169,255,28,
       95,124,172,198,92,45,142,120,31,195,168,140,235,6,60,97,83,191,149,6,80,
       128,33,2,32,11,45,9,121,199,110,18,187,72,13,166,53,241,146,204,141,198,
       249,5,56,13,212,172,31,243,90,79,104,244,98,255,253,3,33,3,87,156,162,
       230,209,7,82,47,1,44,208,11,82,185,166,95,180,111,12,87,185,184,182,227,
       119,196,143,82,106,68,116,26,255,255,255,255,63,31,9,115,51,228,212,109,
       81,245,231,123,83,38,77,184,247,245,210,225,130,23,225,9,153,87,208,245,
       175,119,19,238,1,0,0,0,108,73,48,70,2,33,0,182,99,73,158,247,50,115,163,
       120,141,234,52,39,23,194,100,10,196,60,90,28,248,98,201,224,155,32,111,
       203,63,107,184,2,33,0,176,153,114,231,89,114,217,20,143,43,221,70,46,92,
       182,155,87,193,33,75,136,252,85,202,99,134,118,192,124,252,16,216,3,33,
       3,87,156,162,230,209,7,82,47,1,44,208,11,82,185,166,95,180,111,12,87,
       185,184,182,227,119,196,143,82,106,68,116,26,255,255,255,255,3,128,132,
       30,0,0,0,0,0,25,118,169,20,191,178,130,199,12,65,145,244,91,90,102,101,
       202,209,104,47,44,156,253,251,136,172,128,132,30,0,0,0,0,0,25,118,169,
       20,152,87,204,7,190,211,58,92,241,43,156,94,5,0,182,117,213,0,200,17,
       136,172,224,253,28,0,0,0,0,0,25,118,169,20,67,197,40,80,96,108,135,36,3,
       192,96,30,105,250,52,178,111,98,219,74,136,172,0,0,0,0>>),
    ?assertEqual(true, lib_script:eval(ScriptSig, ScriptPubKey, 0, Tx)). 

script_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
	{"build and match", fun match/0},
	{"Simple eval", fun simple_eval/0},
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
	{"op_check nops range", fun op_checknops/0},
	{"Get ops", fun get_ops/0},
	{"Get p2pkh", fun get_p2pkh/0},
	{"Input", fun input/0},
	{"Coinbase tx", fun coinbase_tx/0},
	{"validate p2pkh", fun validate_p2pkh/0},
	{"scripts 1", fun scripts1/0},
	{"scripts 2", fun scripts2/0},
	{"scripts 3", fun scripts3/0},
	{"sighash 0", fun sighash0/0},
	{"scripts 4", fun scripts4/0},
	{"sighash none", fun sighash_none/0},
	{"scripts 5", fun scripts5/0},
	{"Sighash single", fun sighash_single/0}
   ]
  }.


ib(X) -> iolist_to_binary(X). 
e(X) ->
	%?debugFmt("Eval: ~p~n", [lib_script:eval(X)]),
	{Stack, _, _} = lib_script:eval(X),
	Stack.
