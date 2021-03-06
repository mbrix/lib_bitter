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

-module(lib_transact_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
	fakeutxo:start(),
	fake_colored_block(),
	btr_net_params:init(main),
	ok.

stop(_) ->
	fakeutxo:stop(),
	ok.

fake_colored_block() ->
	fakeutxo:import().

input_select() ->
	Unspents = get_unspents(), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                          "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                          ?Uncolored,
                          100000),
	{U2, Selected, Total} = lib_transact:get_inputs(list, Unspents, P, 0),
	?assert(Total >= 100000),
	?assert(length(U2) < length(Unspents)),
	?assert(length(Selected) > 0),
	lists:foreach(fun(X) ->
				?assertEqual(?Uncolored, lib_unspent:get_attribute(color, X, ?Uncolored))
		end, Selected),
	{_U3, _Selected3, Total3} = lib_transact:get_inputs(list, Unspents, P#payee{value=2000000000000}, 0),
	?assert(Total3 < 2000000000000), lib_transact:get_inputs(list, Unspents, P#payee{value=2000000000000}, 0),
	{_U4, Selected4, Total4} = lib_transact:get_inputs(list, Unspents, P#payee{value=23405, color=chartreuse}, 0),
	?assertEqual(1, length(Selected4)),
	?assertEqual(23405, Total4),
	{_U5, Selected5, Total5} = lib_transact:get_inputs(list, [], P#payee{value=23405, color=chartreuse}, 200),
	?assertEqual(200, Total5),
	?assertEqual([], Selected5).

outputs_uncolored() ->
	Unspents = get_unspents(), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           100000),
	{U, S, Total, _O} = lib_transact:make_outputs(Unspents, P, lib_transact:payment(), false),
	?assert(length(U) < length(Unspents)),
	?assert(length(S) > 0),
	?assert(Total >= 100000).

outputs_overflow() ->
	Unspents = get_unspents(),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           100000),
	{U, S, Remaining, O} = lib_transact:make_outputs(Unspents, P, lib_transact:payment(), true),
	?assert(length(U) < length(Unspents)),
	?assert(length(S) > 0),
	?assert(Remaining =:= 0),
	?assert(O#btxout.value =:= 2513989998).


outputs_colored() ->
	Unspents = get_unspents(), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{U, S, Remaining, O} = lib_transact:make_outputs(Unspents, P, lib_transact:payment(), false),
	?assert(length(U) < length(Unspents)),
	?assert(length(S) > 0),
	?assert(Remaining =:= 405),
	?assert(lib_unspent:get_attribute(quantity, O, 0) =:= 23000).


simple_pay() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = get_unspents(), 
	UnspentsDict = unspents_to_dict(Unspents), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	[O|_] = Payment#payment.outputs,
	?assert(lib_unspent:get_attribute(quantity, O, 0) =:= 23000),
	?assert(Payment#payment.r_value =:= 405),
	?assert(Payment#payment.r_color =:= chartreuse),
	% Finalize payment obj then recolor outputs
	{ok, _, _, Tx} = lib_transact:finalize(Payment, Remaining, Change),
	?assertEqual(4, length(Tx#btxdef.txoutputs)),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, UnspentsDict),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs).

multiple_pay() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = get_unspents(), 
	UnspentsDict = unspents_to_dict(Unspents), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	P2 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           100),
	P3 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           50),
	P4 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           50),
	P5 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           green,
                           400),
	{R, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, [P, P2, P3, P4, P5]),
	O = Payment#payment.outputs,
	?assert(length(O) =:= 6),
	?assert(length(R) < length(Unspents)),
	{ok, _, _, Tx} = lib_transact:finalize(Payment, R, Change),
	?assertEqual(9, length(Tx#btxdef.txoutputs)),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txinputs, Tx#btxdef.txoutputs]),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, UnspentsDict),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txoutputs, RecoloredOutputs]),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs).


simple_encode() ->
	Unspents = get_unspents(), 
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),

	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	{ok, _, _, _Tx} = lib_transact:finalize(Payment, Remaining, Change).

payment_error() ->
	Unspents = get_unspents(), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           230000000),
	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).

simple_issue() ->
	Unspents = get_unspents(), 
	UnspentsDict = unspents_to_dict(Unspents), 
	C = #utxop{hash_index = {<<5,88,152,203,2,63,74,167,62,207,11,66,59,31,127,88,176,81,5,
                  229,240,148,98,68,171,67,72,165,96,176,194,34>>, 1},
               value = 997018000,
               script = <<118,169,20,166,113,172,35,25,185,238,85,99,183,67,
                           179,1,221,121,40,240,203,226,216,136,172>>},
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           100000000),
	P2 = lib_transact:issue(lib_transact:payment(), C, P),
	{ok, _, _, Tx} = lib_transact:finalize(P2, Unspents, Change),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, UnspentsDict),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs),
	P2.



multi_issue() ->
	Unspents = get_unspents(), 
	UnspentsDict = unspents_to_dict(Unspents), 
	% Need to manually specify an uncolored Issue address
	C = #utxop{hash_index = {<<5,88,152,203,2,63,74,167,62,207,11,66,59,31,127,88,176,81,5,
                  229,240,148,98,68,171,67,72,165,96,176,194,34>>, 1},
               value = 997018000,
               script = <<118,169,20,166,113,172,35,25,185,238,85,99,183,67,
                           179,1,221,121,40,240,203,226,216,136,172>>},
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           100000000),
	P2 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           8000),
	P3 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           150000),
	Payment = lib_transact:issue(lib_transact:payment(), C, P),
	Payment2 = lib_transact:issue(Payment, P2),
	Payment3 = lib_transact:issue(Payment2, P3),
	{ok, _, _, Tx} = lib_transact:finalize(Payment3, Unspents, Change),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txinputs, Tx#btxdef.txoutputs]),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, UnspentsDict),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txoutputs, RecoloredOutputs]),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs),
	P2.


% Issue an asset, and then do a transfer payment.
issue_transfer() ->
	Payment = simple_issue(),
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = get_unspents(), 
	UnspentsDict = unspents_to_dict(Unspents), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{Remaining, Payment2} = lib_transact:pay(Payment, Unspents, P),
	?assert(length(Payment2#payment.outputs) =:= 2),
	{ok, _, _, Tx} = lib_transact:finalize(Payment2, Remaining, Change),
	?assert(length(Tx#btxdef.txoutputs) =:= 5),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txinputs, Tx#btxdef.txoutputs]),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, UnspentsDict),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txoutputs, RecoloredOutputs]),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs).

% Uncolored funds
insufficient_funds() ->
	Unspents = get_unspents(), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           1000000000000000000),
	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).


nonexistent_color() ->
	Unspents = get_unspents(), 
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           raspberry,
                           1),
	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).


no_fee() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = get_unspents(), 
	AvailableValue = lib_transact:available(?Uncolored, Unspents),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           AvailableValue),
	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	?assertThrow(insufficient_funds, lib_transact:finalize(Payment, Remaining, Change)).

change_under_dust() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = get_unspents(), 
	AvailableValue = lib_transact:available(?Uncolored, Unspents),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           AvailableValue-?DEFAULTFEE-300),

	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	?assertThrow(insufficient_funds, lib_transact:finalize(Payment, Remaining, Change)).

% Enough for fee but creates dust output
dust_fee() ->
	Unspents = get_unspents(), 
	UncoloredValue = lib_transact:value(?Uncolored, Unspents),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           UncoloredValue-20599),

	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).


satoshi_conversions() ->
    ?assertEqual(0, lib_transact:satoshi_to_btc(0)),
    ?assertEqual(0, lib_transact:satoshi_to_btc(0.0)),
    ?assertEqual(0, lib_transact:btc_to_satoshi(0)),
    ?assertEqual(100000000, lib_transact:btc_to_satoshi(1)),
    ?assertEqual(0.00001, lib_transact:satoshi_to_mbtc(1)),
    ?assertEqual(0.01, lib_transact:satoshi_to_ubtc(1)),
    ?assertEqual(100000000, lib_transact:btc_to_satoshi(1)),
    ?assertEqual(1000000, lib_transact:btc_to_ubtc(1)),
    ?assertEqual(1000, lib_transact:btc_to_mbtc(1)).

fee_computation() ->
    RawTx = hex:hexstr_to_bin("0100000001D57084EB371DA3130A83A6F5D15CA7A1F511242A90297A092F01C5C8CF9E80AC000000006A473044022030E29E6F35AACA596DEB088EDDF361BE46CF9723BDD1B5CE62DDE331B91EE9AA02206D9CF8A0491A4CEC4A204172ED87924A1A85453A41245AA35BA49DB6D3B60BE5012102B1C863488A2EDEEFCE5CD1EEDF59CC89CF9A0A1C39621D7E981FD530F089797DFFFFFFFF0354150000000000001976A914513EC2BAC7AD5B9A420DEFD2BFD3C83D2E199A6D88AC00000000000000000B6A094F41010001C0843D00BC640700000000001976A91490E04ABF858D553B8E59EB63C4BAFFDFE032F3C588AC00000000"),
    Tx = lib_parse:parse_tx(btxdef, RawTx),
    ?assertEqual(10000, lib_tx:calculate_fee(Tx)).

color_transfer_fee() ->
    RawTx = hex:hexstr_to_bin("0100000003E6DE34FDA87D9B37D05E109398F77E2795FDFAC1F4C268AC536EE1BCCDDC388C000000006C493046022100F370358A4D88377E211CF2855976EC4C85D39FA4C7AC2F3045656E92BE06B57C022100B40E2968AE308C8486115B1B9797D7862232F8015F31495A9234159F46555F830121033603213B740AC22E14B6B1D4CE20B450213617D09A38BE652350B162D51AB2F8FFFFFFFFC95B79B89924C169FEE635D0F9818E6C9C1BBF967CCE284B86FA75F4F5D7D919000000006B48304502206A1F75E48A0D01F994FFCA697AB7CE41EA4383CE02B468BE4845D6E23CCA1D3F0221009DE78528146BEE1ABD286881C2141BE31CE5580B18B993C4A9A0A628A01343BE0121033603213B740AC22E14B6B1D4CE20B450213617D09A38BE652350B162D51AB2F8FFFFFFFFCA58A5EB4FE4C841B0E8FABEB4AEEE1769C93C2C1AA6C57F8F847D991FA0E0B2000000006B48304502202A3C64E37FCD21B715AFAE564CF5E08031C52C291290BB1A065BB1EFC24DEE6D022100BED22378093D744EED34AA6539F7A2D17CACFF3BA3CD25D185BACDB001A6C4FA012102B1C863488A2EDEEFCE5CD1EEDF59CC89CF9A0A1C39621D7E981FD530F089797DFFFFFFFF0400000000000000000E6A0C4F41010002E0A03EA0E83B0022020000000000001976A9145C172970A5EF780D13D2D25C259DC12D53A46B3A88AC22020000000000001976A914041302758C429B95D1CABBAF1F9F0D91BB102A1C88AC22020000000000001976A914041302758C429B95D1CABBAF1F9F0D91BB102A1C88AC00000000"),
    Tx = lib_parse:parse_tx(btxdef, RawTx),
    ?assertEqual(10000, lib_tx:calculate_fee(Tx)).

% Try and spend the total amount - fee
minimal_spend() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspent = lib_test:random_unspent(utxop, 100),
	UnspentColored = lib_test:random_unspent(utxop, 99),
	Unspent2 = Unspent#utxop{value =  ?DEFAULTFEE + ?DUSTLIMIT},
	UnspentColored2 = lib_color:set_color(UnspentColored#utxop{value = ?DUSTLIMIT}, red, 2000),
	Unspents = [UnspentColored2, Unspent2],
	AvailableValue = Unspent2#utxop.value - ?DEFAULTFEE,
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           ?Uncolored,
                           AvailableValue),
	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
    ?assertEqual(1, length(Payment#payment.selected)),
    [A] = Payment#payment.selected,
    ?assertEqual(?Uncolored, lib_unspent:get_attribute(color, A, ?Uncolored)),
    ?assertMatch({ok, _, _, _}, lib_transact:finalize(Payment, Remaining, Change)).



%{ok, _, _, Tx} = lib_transact:finalize(Payment, Remaining, Change),
%	?debugFmt("~p~n", [Tx#btxdef.txoutputs]),
%	?assertEqual(2, length(Tx#btxdef.txoutputs)).
%

transact_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"Select Inputs from Dict", fun input_select/0},
		{"Generate outputs uncolored", fun outputs_uncolored/0},
		{"Outputs Overflow", fun outputs_overflow/0},
		{"Generate outputs colored", fun outputs_colored/0},
		{"Not enough of color", fun payment_error/0},
		{"Simple payment", fun simple_pay/0},
		{"Multiple ordered color", fun multiple_pay/0},
		{"Simple encode", fun simple_encode/0},
		{"Simple issue", fun simple_issue/0},
		{"Multiple issue",  fun multi_issue/0},
		{"Issue and Transfer", fun issue_transfer/0},
		{"Insufficent Colored Funds", fun insufficient_funds/0},
		{"Nonexistent color", fun nonexistent_color/0},
		{"No room for fee", fun no_fee/0},
		{"Change under dust limit.", fun change_under_dust/0},
		{"Try to trigger leftover dust", fun dust_fee/0},
		{"Satoshi conversions", fun satoshi_conversions/0},
		{"Fee computation", fun fee_computation/0},
		{"Color transfer fee", fun color_transfer_fee/0},
		{"Minimal uncolored spend", fun minimal_spend/0}
   ]
  }.



%% Helper functions
%%

unspents_to_dict(Utxo) ->
	lists:foldl(fun(O, Acc) ->
	                    Hash = lib_unspent:hash(O),
	                    Index = lib_unspent:index(O),
					dict:store({Hash, Index}, O, Acc)
				end, dict:new(), Utxo).


get_unspents() -> fakeutxo:all_unspents().
