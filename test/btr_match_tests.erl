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

-module(btr_match_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

start() -> ok.
stop(_) -> ok.

true() ->
	F = btr_match:true_fun(),
	?assertEqual(true, F(anything)).

false() ->
	F = btr_match:false_fun(),
	?assertEqual(false, F(anything)).

bloom() ->
	{ok, A} = bitter_bloom:new(100, 0.001),
	{ok, B} = bitter_bloom:insert(A, <<"testdata">>),
	F = btr_match:bloom_fun(B),
	Tx = get_tx(),
	?assertEqual(false, F(Tx)),
	Address = lib_address:new("3Jv3Kot9CdgGJCtUkvRAwmXN7SbKeNjhV3"),
	{ok, B2} = bitter_bloom:insert(A, lib_address:hash160(Address)),
	F2 = btr_match:bloom_fun(B2),
	?assertEqual(true, F2(Tx)),
	F3 = btr_match:match_helper({bloom, B2}),
	?assertEqual(true, F3(Tx)).

run_fundefs() ->
	{ok, A} = bitter_bloom:new(10000, 0.001),
	{ok, A2} = bitter_bloom:insert(A, lib_address:hash160(lib_address:new("3Jv3Kot9CdgGJCtUkvRAwmXN7SbKeNjhV3"))),
	F3 = btr_match:match_helper({bloom, A2}),
	F4 = fun btr_match:false_fun/1,
	Tx = get_tx(),
	?assertEqual(true, F3(Tx)),
	?assertEqual(false, F4(Tx)),
	?assertEqual(true, btr_match:run_fundefs([F3], Tx)),
	?assertEqual(true, btr_match:run_fundefs([fun btr_match:false_fun/1,
											  fun btr_match:false_fun/1,
											  F3,
											  fun btr_match:false_fun/1], Tx)).

match_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"true", fun true/0},
		{"false", fun false/0},
		{"bloom filter functions", fun bloom/0},
		{"Run fundefs", fun run_fundefs/0}
   ]
  }.



get_tx() ->
	lib_parse:parse_tx(hex:hexstr_to_bin("0100000001b25928781a2f7928ea5c7b40aef4e3e39656fcc0a573f3ba699b75fd8df1438700000000fc00473044022064aacc34ba447c6da1822b0e8bc772ecaccabbbdf95703fbcc658d340e9e9f0d02204457f4655345b73be77b93f0d2d52b7389504169d5609ee833b81ceb50affcad01473044022029fab43907297c55f313707b5e6a22ae367a1114e4adafd5861d7eb6258df78302202d2d16193390cb46deccb3b3a80c30aaf0218a9cf387854e2c9f234e314222cd014c69522103e9c2919bb0d4dfa4755545a9c1bc258310d58650cf8ff961efe3ef64cddab5eb2103b2013482c3c097d3c7da8fbb0c7b61e0fcef08fef6c83855eaaf81348abf22122103286c6150d97b0ab242de8e8e8f706d4ba2b7ee7ccfcd47066cd34c0a5ab0faa753aeffffffff0247b2e91f0000000017a914bcf0b726624011de86c634bf38ad1ccb904059d687b5b301000000000017a914abfeccd572cda3e1f0adfbb17e6a568296569e2a8700000000")).


