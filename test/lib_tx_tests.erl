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

-module(lib_tx_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	ok.

stop(_) ->
	ok.

parse_serialize() ->
	RawTX = hex:hexstr_to_bin("0100000001bc59d78f9621bc76be283ce621c2681286358d64cd15e3bef31704f4f7ef4ea5010000006946304302204262dd6fd358019bdb4de673da589ddffa834573f864f56e46ba064166e4a413021f61b0b5ced3f0f09d125a8a15c42d6d175f8df3b633bb97f73fe3a82c1ad58e012103411fb18f52f89e20fa43562c63a0b57cc1eaa7768e1e461f451d7134550dfb6affffffff0224db930d000000001976a914086a78a1b5b7b3982684157cb9cf0dc4191482a388ac40900200000000001976a914fd5b5159ca320055a683d33b7e40b537ce962b0288ac00000000"),
	[T|_] = lib_parse:getTransactions(1, RawTX),
	[Tx|_] = T,
	SerializedTx = lib_tx:serialize_btxdef(Tx),
	?assertEqual(RawTX, SerializedTx).

parse_reconstruct() ->
	RawTX = hex:hexstr_to_bin("0100000001bc59d78f9621bc76be283ce621c2681286358d64cd15e3bef31704f4f7ef4ea5010000006946304302204262dd6fd358019bdb4de673da589ddffa834573f864f56e46ba064166e4a413021f61b0b5ced3f0f09d125a8a15c42d6d175f8df3b633bb97f73fe3a82c1ad58e012103411fb18f52f89e20fa43562c63a0b57cc1eaa7768e1e461f451d7134550dfb6affffffff0224db930d000000001976a914086a78a1b5b7b3982684157cb9cf0dc4191482a388ac40900200000000001976a914fd5b5159ca320055a683d33b7e40b537ce962b0288ac00000000"),
	[T|_] = lib_parse:getTransactions(1, RawTX),
	[Tx|_] = T,
	N = lib_tx:create_tx(),
	N2 = insert_inputs(Tx, N),
	N3 = insert_outputs(Tx, N2),
	SerializedTx = lib_tx:serialize_btxdef(N3),
	?assertEqual(RawTX, SerializedTx).

sign_tx() ->
	Unspent = {utxop,
       {<<247,176,86,46,208,66,141,115,70,76,185,244,40,183,169,22,119,170,63,
          58,215,253,40,121,243,215,190,217,28,235,57,32>>,
        1},
       168000,
       <<118,169,20,253,91,81,89,202,50,0,85,166,131,211,59,126,64,181,55,206,
         150,43,2,136,172>>,
       <<253,91,81,89,202,50,0,85,166,131,211,59,126,64,181,55,206,150,43,2>>,
       p2pkh,?Uncolored,0,312763,?Unspent_Nostate},
    UnspentDict = lib_kd:add(Unspent),
    {Hash160Public, Public, Private} = {<<253,91,81,89,202,50,0,85,166,131,211,59,126,64,181,55,206,150,43,2>>,
 <<3,62,157,4,220,25,221,86,158,186,154,170,210,195,229,183,83,94,211,43,217,5,
    176,122,214,122,237,55,160,226,199,244,199>>,
     <<147,142,170,118,88,118,82,176,21,252,227,188,224,125,76,241,149,117,225,
        161,57,12,202,90,254,181,117,107,166,177,74,120>>}, 
	KeypairDict = dict:store(Hash160Public, {Public, Private}, dict:new()),
    N = lib_tx:create_tx(),
    I = lib_tx:create_input(Unspent),
    N2 = lib_tx:add_input(N, I), 
    O = lib_tx:create_output(p2pkh,
    		                  ?Uncolored,
    		                  168000-?DEFAULTFEE,
			lib_address:address_to_hash160("1DNzrK2AgStNgRcqqreGbFZbBR6CUAuE2M")),
    N3 = lib_tx:add_output(N2, O),
	SignedTx = lib_tx:serialize_btxdef(lib_tx:sign_tx(?SIGHASH_ALL, N3, KeypairDict, dict:new(), UnspentDict)),
	?assertEqual(SignedTx, SignedTx).

sign_tx_again() ->
	PrevTx = hex:hexstr_to_bin("010000000126c07ece0bce7cda0ccd14d99e205f118cde27e83dd75da7b141fe487b5528fb000000008b48304502202b7e37831273d74c8b5b1956c23e79acd660635a8d1063d413c50b218eb6bc8a022100a10a3a7b5aaa0f07827207daf81f718f51eeac96695cf1ef9f2020f21a0de02f01410452684bce6797a0a50d028e9632be0c2a7e5031b710972c2a3285520fb29fcd4ecfb5fc2bf86a1e7578e4f8a305eeb341d1c6fc0173e5837e2d3c7b178aade078ffffffff02b06c191e010000001976a9143564a74f9ddb4372301c49154605573d7d1a88fe88ac00e1f505000000001976a914010966776006953d5567439e5e39f86a0d273bee88ac00000000"),
	UnsignedTx = hex:hexstr_to_bin("0100000001eccf7e3034189b851985d871f91384b8ee357cd47c3024736e5676eb2debb3f2010000001976a914010966776006953d5567439e5e39f86a0d273bee88acffffffff01605af405000000001976a914097072524438d003d23a2f23edb65aae1bb3e46988ac0000000001000000"),
	[T|_] = lib_parse:getTransactions(1, PrevTx),
	[Tx|_] = T,
	[_, Input] = Tx#btxdef.txoutputs,
	Unspent = #utxop{hash_index={Tx#btxdef.txhash, 1},
			          script=Input#btxout.script},
	UnspentDict = lib_kd:add(Unspent),
    N = lib_tx:create_tx(),
    I = lib_tx:create_input(Unspent),
    N2 = lib_tx:add_input(N, I), 
    O = lib_tx:create_output(p2pkh,
    		                  ?Uncolored,
    		                  99900000,
			lib_address:address_to_hash160("1runeksijzfVxyrpiyCY2LCBvYsSiFsCm")),
    N3 = lib_tx:add_output(N2, O),
	ITX = lib_tx:intermediate_tx(?SIGHASH_ALL, lib_tx:clear_input_scripts(lib_tx:replace_input_script(N3, I, Input#btxout.script), I)),
	?assertEqual(UnsignedTx, ITX),
	Private = hex:hexstr_to_bin("18E14A7B6A307F426A94F8114701E7C8E774E7F9A47E2C2035DB29A206321725"),
	Public = hex:hexstr_to_bin("0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"),
	Hash160Public = lib_address:key_to_hash160(Public),
	?assertEqual(lib_address:address_to_hash160("16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"),
		Hash160Public),
	?assertEqual(hex:hexstr_to_bin("9302bda273a887cb40c13e02a50b4071a31fd3aae3ae04021b0b843dd61ad18e"),
		         lib_tx:hash_tx(ITX)),
	KeypairDict = dict:store(Hash160Public, {Public, Private}, dict:new()),
	SignedTx = lib_tx:serialize_btxdef(lib_tx:sign_tx(?SIGHASH_ALL, N3, KeypairDict, dict:new(), UnspentDict)),
	?assertEqual(SignedTx, SignedTx).


create_p2sh_to_p2pkh_transaction() ->
	% Sign and spend a P2SH output to a P2pkh output
	%
	% Unspent Uncolored Multisig
	Unspent = {utxop,
       {<<107,70,3,70,28,119,76,215,46,212,77,232,74,220,88,47,136,181,209,41,
          104,33,251,223,64,202,250,113,95,104,136,152>>,
        0},
       1000000,
       <<169,20,26,40,10,174,157,152,180,191,245,201,71,139,39,108,33,92,170,
         41,62,230,135>>,
       <<26,40,10,174,157,152,180,191,245,201,71,139,39,108,33,92,170,41,62,230>>,
       p2sh,?Uncolored,0,313879,?Unspent_Nostate},
    UnspentDict = lib_kd:add(Unspent),
	% The Hash160 Address maps to a tuple list of Public / Private keypairs
	% or proposals necessary to construct the composite script sig
	KeypairDict = dict:store(<<26,40,10,174,157,152,180,191,245,201,71,139,39,108,33,92,170,41,62,230>>,  [{<<4,102,100,185,158,175,153,55,0,126,207,72,60,123,220,151,3,36,33,106,23,
     164,236,160,216,55,74,168,195,214,191,250,8,20,81,27,81,47,210,216,3,125,
     32,139,46,248,190,103,91,83,21,210,250,125,121,86,86,94,149,6,134,220,105,
     127,216>>,
   <<58,3,77,221,19,150,142,7,82,208,102,32,36,196,74,120,201,106,53,76,190,
     252,105,143,179,34,96,253,167,222,59,236>>},
  {<<4,196,134,241,131,121,212,11,57,108,226,176,246,231,154,58,43,23,175,54,
     232,56,1,123,221,51,14,130,28,96,48,120,141,98,66,244,237,222,219,245,206,
     203,89,52,5,199,97,239,163,109,216,236,41,215,188,52,65,111,94,236,73,148,
     27,207,69>>,
   <<213,63,71,73,196,155,124,174,126,51,154,41,190,179,21,252,8,162,146,
     153,125,125,22,211,66,42,36,32,108,35,110,21>>}], dict:new()),
	KeyList = dict:fetch(<<26,40,10,174,157,152,180,191,245,201,71,139,39,108,33,92,170,41,62,230>>, KeypairDict),
	{PublicKeyList, _} = lists:unzip(KeyList),
%	lists:foreach(fun(X) ->
%			?debugFmt("~n~p~n", [hex:bin_to_hexstr(lib_address:compress_key(X))])
%		end, PublicKeyList),
	N = lib_tx:create_tx(),
	I = lib_tx:create_input(Unspent),
	?assertMatch({p2sh, _}, lib_tx:unspent_type(Unspent)),

	CorrectRedeemScript = hex:hexstr_to_bin("5221026664b99eaf9937007ecf483c7bdc970324216a17a4eca0d8374aa8c3d6bffa082103c486f18379d40b396ce2b0f6e79a3a2b17af36e838017bdd330e821c6030788d52ae"),
	?assertEqual(CorrectRedeemScript, lib_address:p2sh_redeemscript(PublicKeyList)),
	?assertEqual("345KTgMSLhvtugSV7jxY3LpFt5KPj6eK99",
		lib_address:p2sh_script_to_address(erlang:iolist_to_binary([lib_address:p2sh_redeemscript(PublicKeyList)]))),

	?assertEqual(hex:hexstr_to_bin("1a280aae9d98b4bff5c9478b276c215caa293ee6"), lib_address:address_to_hash160("345KTgMSLhvtugSV7jxY3LpFt5KPj6eK99")),
	?assertEqual("345KTgMSLhvtugSV7jxY3LpFt5KPj6eK99", lib_address:p2sh_script_to_address(CorrectRedeemScript)),
	?assertEqual(lib_address:address_to_hash160("345KTgMSLhvtugSV7jxY3LpFt5KPj6eK99"), lib_address:script_to_hash160(lib_address:p2sh_redeemscript(PublicKeyList))),
    O = lib_tx:create_output(p2pkh,
    		                  ?Uncolored,
    		                  1000000-?DEFAULTFEE,
			lib_address:address_to_hash160("1DNzrK2AgStNgRcqqreGbFZbBR6CUAuE2M")),
	N2 = lib_tx:add_input(N, I),
	N3 = lib_tx:add_output(N2, O),
	_SignedTx = lib_tx:serialize_btxdef(lib_tx:sign_tx(?SIGHASH_ALL, N3, KeypairDict, dict:new(), UnspentDict)).
	%?debugFmt("~n~p~n", [hex:bin_to_hexstr(SignedTx)]).

change_input_script() ->
	TestScript = <<$T, $E, $S, $T>>,
	N = lib_tx:create_tx(),
	I = lib_test:create_random_input(),
	N2 = lib_tx:add_input(N, lib_test:create_random_input()),
	N3 = lib_tx:add_input(N2, lib_test:create_random_input()),
	N4 = lib_tx:add_input(N3, I),
	N5 = lib_tx:add_input(N4, lib_test:create_random_input()),
	O = lib_test:create_random_output(),
	N6 = lib_tx:add_output(N5, O),
	N7 = lib_tx:replace_input_script(N6, I, TestScript),
	I2 = lib_tx:find_input(N7, I),
	?assertEqual(TestScript, I2#btxin.script).

readable_serial() ->
	RawHexString = "0100000001c887d7fce8e0a526905604b03b6a3cb3b827ee662363afc06ba2416cb9740f66000000006c493046022100c452abef5152a354852bc1484bfb5fe7be88580bc9a45a581181e2f7885954f6022100b73a73ab43bf77bb8b7c36d6777bad6f6c9853fb3cbf9538605232bc5e1e3e9e0121023ff4b9bb4172700d60d048f6fb67252b1c76e28425136f8a6a6959303ba0cabcffffffff02983a0000000000001976a9144dbc4a6961fc5f647343d8a5dfde8a7248765f8188ac084c0100000000001976a914011a7b389f342df6b4d9093d9111bf3697f7c99c88ac00000000",
	A = lib_tx:from_hex(RawHexString),
	?assertEqual(RawHexString, lib_tx:readable(A)).

redeemscript_parse() ->
	Script = <<0,71,48,68,2,32,38,36,252,71,174,1,47,229,205,218,129,178,168,166,101,40,146, 36,172,156,66,202,211,63,229,135,69,217,142,38,185,77,2,32,3,24,42,119,7,112, 39,47,220,106,201,181,147,193,117,219,3,82,64,174,76,161,153,237,113,74,81,
  164,164,121,21,99,1,72,48,69,2,33,0,247,217,25,226,129,24,173,68,160,169,221,
  202,221,47,167,218,212,112,126,81,226,149,182,177,89,184,125,20,156,154,92,
  105,2,32,68,239,16,103,67,61,72,115,21,152,115,178,227,176,210,247,195,104,8,
  163,174,138,249,118,181,123,145,110,225,202,186,199,1,71,82,33,3,57,228,105,
  255,19,78,97,181,196,195,221,145,196,167,34,185,189,183,15,23,117,191,51,85,
  182,133,140,231,21,94,197,110,33,2,43,94,36,98,235,130,233,89,86,70,21,162,
  117,21,183,79,105,12,114,203,192,68,111,9,74,246,174,249,245,45,17,82,82,174>>,
	{{RedeemInfo, _RawScript}, Sigs} = lib_tx:sigs(Script),
	{{M,N}, _} = RedeemInfo,
	?assertEqual(2, length(Sigs)),
	?assertEqual(2, N),
	?assertEqual(2, M).

signed() ->
	{Pub, _} = lib_address:generate_keypair(),
	Addr = lib_address:new(key, Pub),
	{Pub2, _} = lib_address:generate_keypair(),
	Addr2 = lib_address:new(key, Pub2),
	{Pub3, _} = lib_address:generate_keypair(),
	Addr3 = lib_address:new(key, Pub3),
	N = lib_tx:create_tx(),
	{_,I1} = lib_test:create_input(Addr),
	{_,I2} = lib_test:create_input(Addr2),
	{_,I3} = lib_test:create_input(Addr3),
	N2 = lib_tx:add_input(N, I1#btxin{signed=true}),
	N3 = lib_tx:add_input(N2, I2#btxin{signed=false}),
	N4 = lib_tx:add_input(N3, I3#btxin{signed=true}),
	N5 = lib_tx:add_output(N4, lib_test:create_random_output()),
	N6 = lib_tx:add_output(N5, lib_test:create_random_output()),
	N7 = lib_tx:add_output(N6, lib_test:create_random_output()),
	?assertEqual(false, lib_tx:is_signed(N7)).

pushdata_vs_varint() ->
    A = lib_tx:int_to_pushdata(10),
    ?assertMatch([10,_], lib_tx:pushdata_to_int(A)),
    B = lib_tx:int_to_pushdata(76),
    ?assertMatch([76,_], lib_tx:pushdata_to_int(B)),
    C = lib_tx:int_to_pushdata(253),
    ?assertMatch([253,_], lib_tx:pushdata_to_int(C)),
    D = lib_tx:int_to_pushdata(65535),
    ?assertMatch([65535,_], lib_tx:pushdata_to_int(D)),
    E = lib_tx:int_to_pushdata(4294967294),
    ?assertMatch([4294967294,_], lib_tx:pushdata_to_int(E)),
    F = lib_tx:int_to_pushdata(4294967296),
    ?assertMatch(error, lib_tx:pushdata_to_int(F)).

json_serialization() ->
	RawTX = hex:hexstr_to_bin("0100000001bc59d78f9621bc76be283ce621c2681286358d64cd15e3bef31704f4f7ef4ea5010000006946304302204262dd6fd358019bdb4de673da589ddffa834573f864f56e46ba064166e4a413021f61b0b5ced3f0f09d125a8a15c42d6d175f8df3b633bb97f73fe3a82c1ad58e012103411fb18f52f89e20fa43562c63a0b57cc1eaa7768e1e461f451d7134550dfb6affffffff0224db930d000000001976a914086a78a1b5b7b3982684157cb9cf0dc4191482a388ac40900200000000001976a914fd5b5159ca320055a683d33b7e40b537ce962b0288ac00000000"),
	Tx = lib_parse:parse_tx(RawTX),
	_JsonTx = lib_tx:to_json(Tx),
	_JsonTx2 = lib_tx:to_json(RawTX).


tx_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"Parse and Serialize", fun parse_serialize/0},
		{"Parse then Reconstruct", fun parse_reconstruct/0},
		{"Sign TX", fun sign_tx/0},
	    {"Sign TX again", fun sign_tx_again/0},
	    {"P2sh to P2pkh", fun create_p2sh_to_p2pkh_transaction/0},
	    {"Change input script", fun change_input_script/0},
	    {"Human/Hex conversion", fun readable_serial/0},
	    {"Redeemscript parse", fun redeemscript_parse/0},
	    {"Signed test", fun signed/0},
	    {"Pushdata vs varint", fun pushdata_vs_varint/0},
	    {"Json serialization", fun json_serialization/0}
   ]
  }.

%% TX construction helpers

insert_inputs(OldTx, NewTx) ->
	lists:foldl(fun(I, Acc) ->
				lib_tx:add_input(Acc, I)
				end, NewTx, OldTx#btxdef.txinputs).

insert_outputs(OldTx, NewTx) ->
	lists:foldl(fun(O, Acc) ->
				NewOutput = lib_tx:create_output(p2pkh,
					                 ?Uncolored,
					                 O#btxout.value,
					                 O#btxout.address),
				lib_tx:add_output(Acc, NewOutput)
		        end, NewTx, OldTx#btxdef.txoutputs).
