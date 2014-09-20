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
	ok.

stop(_) ->
	fakeutxo:stop(),
	ok.

fake_colored_block() ->
	fakeutxo:import().

input_select() ->
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                          "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                          uncolored,
                          100000),
	{U2, Selected, Total} = lib_transact:get_inputs(list, Unspents, P, 0),
	?assert(Total >= 100000),
	?assert(length(U2) < length(Unspents)),
	?assert(length(Selected) > 0),
	lists:foreach(fun(X) ->
				?assertEqual(uncolored, X#utxop.color)
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
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           100000),
	{U, S, Total, _O} = lib_transact:make_outputs(Unspents, P, lib_transact:payment(), false),
	?assert(length(U) < length(Unspents)),
	?assert(length(S) > 0),
	?assert(Total >= 100000).

outputs_overflow() ->
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           100000),
	{U, S, Remaining, O} = lib_transact:make_outputs(Unspents, P, lib_transact:payment(), true),
	?assert(length(U) < length(Unspents)),
	?assert(length(S) > 0),
	?assert(Remaining =:= 0),
	?assert(O#btxout.value =:= 1321000000).


outputs_colored() ->
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{U, S, Remaining, O} = lib_transact:make_outputs(Unspents, P, lib_transact:payment(), false),
	?assert(length(U) < length(Unspents)),
	?assert(length(S) > 0),
	?assert(Remaining =:= 405),
	?assert(O#btxout.quantity =:= 23000).


simple_pay() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	[O|_] = Payment#payment.outputs,
	?assert(O#btxout.quantity =:= 23000),
	?assert(Payment#payment.r_value =:= 405),
	?assert(Payment#payment.r_color =:= chartreuse),
	% Finalize payment obj then recolor outputs
	{ok, _, _, Tx} = lib_transact:finalize(Payment, Remaining, Change),
	?assertEqual(4, length(Tx#btxdef.txoutputs)),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, fun fakeutxo:lookup_tx/2),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs).

multiple_pay() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = ets:tab2list(fakeutxo),
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
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, fun fakeutxo:lookup_tx/2),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txoutputs, RecoloredOutputs]),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs).


simple_encode() ->
	Unspents = ets:tab2list(fakeutxo),
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
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           230000000),
	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).

simple_issue() ->
	Unspents = ets:tab2list(fakeutxo),
	C = #utxop{hash_index = {<<5,88,152,203,2,63,74,167,62,207,11,66,59,31,127,88,176,81,5,
                  229,240,148,98,68,171,67,72,165,96,176,194,34>>, 1},
               color = uncolored,
               value = 997018000,
               script = <<118,169,20,166,113,172,35,25,185,238,85,99,183,67,
                           179,1,221,121,40,240,203,226,216,136,172>>},
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           100000000),
	P2 = lib_transact:issue(lib_transact:payment(), C, P),
	{ok, _, _, Tx} = lib_transact:finalize(P2, Unspents, Change),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, fun fakeutxo:lookup_tx/2),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs),
	P2.



multi_issue() ->
	Unspents = ets:tab2list(fakeutxo),
	% Need to manually specify an uncolored Issue address
	C = #utxop{hash_index = {<<5,88,152,203,2,63,74,167,62,207,11,66,59,31,127,88,176,81,5,
                  229,240,148,98,68,171,67,72,165,96,176,194,34>>, 1},
               color = uncolored,
               value = 997018000,
               script = <<118,169,20,166,113,172,35,25,185,238,85,99,183,67,
                           179,1,221,121,40,240,203,226,216,136,172>>},
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           100000000),
	P2 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           8000),
	P3 = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           150000),
	Payment = lib_transact:issue(lib_transact:payment(), C, P),
	Payment2 = lib_transact:issue(Payment, P2),
	Payment3 = lib_transact:issue(Payment2, P3),
	{ok, _, _, Tx} = lib_transact:finalize(Payment3, Unspents, Change),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txinputs, Tx#btxdef.txoutputs]),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, fun fakeutxo:lookup_tx/2),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txoutputs, RecoloredOutputs]),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs),
	P2.


% Issue an asset, and then do a transfer payment.
issue_transfer() ->
	Payment = simple_issue(),
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           chartreuse,
                           23000),
	{Remaining, Payment2} = lib_transact:pay(Payment, Unspents, P),
	?assert(length(Payment2#payment.outputs) =:= 2),
	{ok, _, _, Tx} = lib_transact:finalize(Payment2, Remaining, Change),
	?assert(length(Tx#btxdef.txoutputs) =:= 5),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txinputs, Tx#btxdef.txoutputs]),
	RecoloredOutputs = lib_color:color_outputs(Tx#btxdef.txinputs, Tx#btxdef.txoutputs, fun fakeutxo:lookup_tx/2),
	%?debugFmt("~p~n~p~n", [Tx#btxdef.txoutputs, RecoloredOutputs]),
	?assert(Tx#btxdef.txoutputs =:= RecoloredOutputs).

% Uncolored funds
insufficient_funds() ->
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           1000000000000000000),
	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).


nonexistent_color() ->
	Unspents = ets:tab2list(fakeutxo),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           raspberry,
                           1),
	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).


no_fee() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = ets:tab2list(fakeutxo),
	AvailableValue = lib_transact:available(uncolored, Unspents),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           AvailableValue),
	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	?assertThrow(insufficient_funds, lib_transact:finalize(Payment, Remaining, Change)).

change_under_dust() ->
	Change = lib_address:new("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB"),
	Unspents = ets:tab2list(fakeutxo),
	AvailableValue = lib_transact:available(uncolored, Unspents),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           AvailableValue-?DEFAULTFEE-300),

	{Remaining, Payment} = lib_transact:pay(lib_transact:payment(), Unspents, P),
	?assertThrow(insufficient_funds, lib_transact:finalize(Payment, Remaining, Change)).

% Enough for fee but creates dust output
dust_fee() ->
	Unspents = ets:tab2list(fakeutxo),
	UncoloredValue = lib_transact:value(uncolored, Unspents),
	P = lib_transact:payee("1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           "1ANGt72gYkAPts4pV5hY5E3QUuU2vEMfBB",
                           uncolored,
                           UncoloredValue-20599),

	?assertThrow(insufficient_funds, lib_transact:pay(lib_transact:payment(), Unspents, P)).




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
		{"Try to trigger leftover dust", fun dust_fee/0}
   ]
  }.
