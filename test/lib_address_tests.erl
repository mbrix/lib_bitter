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

-module(lib_address_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	btr_net_params:init(main),
	ok.

stop(_) ->
	ok.

private_wif() ->
	Private = hex:hexstr_to_bin("0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D"),
	WifKey = lib_address:private_to_wif(uncompressed, Private),
	?assertEqual("5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ", WifKey).

wif_private() ->
	Private = lib_address:wif_to_private("5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ"),
	?assertEqual(hex:hexstr_to_bin("0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D"), Private).

public_to_address() ->
	Public = hex:hexstr_to_bin("0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"),
	% Uncompressed Public key is compressed before turning into address.
	CorrectAddress = "1PMycacnJaSqwwJqjawXBErnLsZ7RkXUAs",
	Address = lib_address:public_to_address(btr_net_params:params(),
											Public),
	?assertEqual(CorrectAddress, Address).

address_type() ->
	?assertEqual(p2sh, lib_address:address_type("33PfEm7Bo2KhJVUZxEN3v7S6SPcrhvzJKq")),
	?assertEqual(p2pkh, lib_address:address_type("1Q6dMQQ8paDqDjiEywV25kSrcwj6W4dia2")).

p2sh_addressing() ->
	{Address, Keylist} = {<<26,40,10,174,157,152,180,191,245,201,71,139,39,108,33,92,170,41,62,230>>,  [{<<4,102,100,185,158,175,153,55,0,126,207,72,60,123,220,151,3,36,33,106,23,
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
						153,125,125,22,211,66,42,36,32,108,35,110,21>>}]},
	{PublicKeys, _} = lists:unzip(Keylist),
	?assertEqual(Address, lib_address:p2sh_redeemscript_hash(PublicKeys)).

% New Style addressing
new_addresses() ->
	A_readable = "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
	A2_readable = "3KDN4ovVr4xA98ZrKZBPrbTok8hkPadG6K",
	A = lib_address:new(A_readable),
	A2 = lib_address:new(A2_readable),
	?assertEqual(p2pkh, A#addr.type),
	?assertEqual(p2sh, A2#addr.type),
	?assertEqual(A_readable, lib_address:readable(btr_net_params:params(), A)),
	?assertEqual(A2_readable, lib_address:readable(btr_net_params:params(), A2)).

verify_keypair() ->
	{Pub, Priv} = lib_address:generate_keypair(),
	?assertEqual(true, lib_address:verify_keypair(Pub, Priv)).

verify_keypair_compressed() ->
	{Pub, Priv} = lib_address:generate_keypair(),
	CPub = lib_address:compress_key(Pub),
	?assertEqual(true, lib_address:verify_keypair(CPub, Priv)).

redeemscript() ->
	{_Addr, [{Pub1, _}, {Pub2, _}]} = lib_address:generate_p2sh_address(p2sh_2of2),
	RScript = lib_address:p2sh_redeemscript([Pub1, Pub2]),
	RInfo = lib_parse:parse_script(RScript),
	{multisig, {{M,N}, _KeyList}} = RInfo,
	?assertEqual(2, M),
	?assertEqual(2, N).

openasset_address() ->
    Address = "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM",
    OpenAssetsAddress = "akB4NBW9UuCmHuepksob6yfZs6naHtRCPNy",
    A = lib_address:new(Address),
    ?assertEqual(OpenAssetsAddress, lib_address:openassets(btr_net_params:params(),
    													   A)).

openasset_new() ->
    Address = lib_address:new("akB4NBW9UuCmHuepksob6yfZs6naHtRCPNy"),
    ?assertEqual("16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM", lib_address:readable(btr_net_params:params(),
    																	   Address)).

failed_checksum() ->
    Address = "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvm",
    ?assertException(throw, address_error, lib_address:new(Address)).

failed_checksum2() ->
    Address = "33PfEm7Bo2KhJVuZxEN3v7S6SPcrhvzJKq",
    ?assertException(throw, address_error, lib_address:new(Address)).

color_identifier() ->
    ColorAddress = "ALn3aK1fSuG27N96UGYB1kUYUpGKRhBuBC",
    ?assertEqual(true, lib_address:checksum(23, ColorAddress)). 

is_openassets() ->
    ColorAddress = "akB4NBW9UuCmHuepksob6yfZs6naHtRCPNy",
    ?assertEqual(true,  lib_address:is_openassets(btr_net_params:params(),
    											  ColorAddress)),
    ?assertEqual(false, lib_address:is_openassets(btr_net_params:params(),
    											  "blah")),
    ?assertEqual(false, lib_address:is_openassets(btr_net_params:params(),
    											  "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM")).

testnet_addresses() ->
	Address = "mwHu1wJDHAnb8ULya6UG75NQ8LyL5KLdV1",
	OpenAssetAddress = "akSjpy42ZwdF2Dc2iu9c4w24ebxYoAFNp2C",
	A = lib_address:new(Address),
	B = lib_address:new(OpenAssetAddress),
	?assertEqual(A, B).

address_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
            {"Private to WIF", fun private_wif/0},
            {"WIF to Private", fun wif_private/0},
            {"Public to address", fun public_to_address/0},
            {"Address types", fun address_type/0},
            {"P2sh addressing", fun p2sh_addressing/0},
            {"New style addressing", fun new_addresses/0},
            {"Verify keypair", fun verify_keypair/0},
            {"Verify keypair compressed", fun verify_keypair_compressed/0},
            {"Redeem script", fun redeemscript/0},
            {"Open assets address", fun openasset_address/0},
		    {"New open assets address", fun openasset_new/0},
		    {"Failed checksum", fun failed_checksum/0},
		    {"Failed checksum p2sh", fun failed_checksum2/0},
		    {"Color id checksum", fun color_identifier/0},
			{"Is openassets target", fun is_openassets/0},
			{"Testnet addresses", fun testnet_addresses/0}
   ]
  }.


