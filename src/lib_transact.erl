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
%
-module(lib_transact).
-author('mbranton@emberfinancial.com').

% High Level Transaction Functions
% Initiate p2pkh and p2sh multi-color payments


-export([get_inputs/4,
	     get_colored_unspents/3,
	     get_quantity_unspents/3,
		 pay/3,
	     make_outputs/4,
	     payee/0,
	     payee/4,
	     payment/0,
	     payment/1,
	     finalize/3,
	     issue/3,
	     issue/2,
	     color_marker/1,
	     value/2,
	     available/2,
	     satoshi_to_mbtc/1,
	     satoshi_to_ubtc/1,
	     satoshi_to_btc/1,
	     mbtc_to_satoshi/1,
	     ubtc_to_satoshi/1,
	     btc_to_satoshi/1,
	     btc_to_mbtc/1,
	     btc_to_ubtc/1,
	     add_fee/1,
	     spent/2,
	     included/2,
	     fee/1,
	     metaurl/2,
	     partial_finish/3]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


% Utility functions for satoshi conversion

satoshi_to_mbtc(S) when is_number(S), S < 1 -> 0;
satoshi_to_mbtc(Satoshis) when is_number(Satoshis), Satoshis >= 1 ->
    Satoshis / 100000.

satoshi_to_ubtc(S) when is_number(S), S < 1 -> 0;
satoshi_to_ubtc(Satoshis) when is_number(Satoshis), Satoshis >= 1->
    Satoshis / 100.

satoshi_to_btc(S) when is_number(S), S < 1 -> 0;
satoshi_to_btc(Satoshis) when is_number(Satoshis), Satoshis >= 1 ->
    Satoshis / 100000000.

mbtc_to_satoshi(S) when is_number(S), S < 0.0001 -> 0;
mbtc_to_satoshi(Mbtc) when is_number(Mbtc), Mbtc >= 0.0001 ->
    Mbtc * 10000.

ubtc_to_satoshi(S) when is_number(S), S < 0.000001 -> 0;
ubtc_to_satoshi(Ubtc) when is_number(Ubtc), Ubtc >= 0.000001 ->
    Ubtc * 100.

btc_to_satoshi(S) when is_number(S), S < 0.00000001 -> 0;
btc_to_satoshi(Btc) when is_number(Btc), Btc >= 0.00000001 ->
    erlang:trunc(Btc * 100000000).

btc_to_mbtc(S) when is_number(S), S < 0.00000001 -> 0;
btc_to_mbtc(Btc) when is_number(Btc), Btc >= 0.00000001 ->
    erlang:trunc(satoshi_to_mbtc(btc_to_satoshi(Btc))).

btc_to_ubtc(S) when is_number(S), S < 0.00000001 -> 0;
btc_to_ubtc(Btc) when is_number(Btc), Btc >= 0.00000001 ->
    erlang:trunc(satoshi_to_ubtc(btc_to_satoshi(Btc))).

% Some functions for manipulating payees
%

payee() ->
	#payee{}.

payee(Address, Change, Color, Value) ->
	#payee{address=lib_address:new(Address),
		   change=lib_address:new(Change),
		   color=lib_color:hash160(Color),
		   value=Value}.

payment() ->
	#payment{selected=[],
		     outputs=[],
		     issuances=0,
		     metaurl = <<>>}.

payment(Color) ->
    #payment{selected=[],
             outputs=[],
             issuances=0,
             r_color = lib_color:hash160(Color),
             metaurl = <<>>}.

%% Prior to issue we may want to associate a Meta data url
metaurl(P, Url) when is_record(P, payment) ->
    P#payment{metaurl = lib_color:encode_metaurl(Url)}.

% Add an issuance to a payment
% BTC change will flow back on
% finalization
% Issue outputs are always first
% Color is not the color of the issuance output
% but the hash160 of the tx.

% Uncolored unspent

issue(Payment, Unspent = #utxop{color = ?Uncolored}, Payee) ->
	if Payment#payment.issuances =/= 0 ->
			throw(issue_error);
	   true ->
	        O = create_output(
						Payee#payee{color=lib_color:get_issue_color_unspents([Unspent])}),
	   		Payment#payment{selected=[Unspent|Payment#payment.selected],
	   		                issuances=1,
	   		                outputs = Payment#payment.outputs ++
	   		    			[O],
	   		    	        r_value = Unspent#utxop.value - O#btxout.value,
	   		    	        r_color = ?Uncolored}
	end;


% Let's simplify this and not use colored unspents
% for issuance. Colored unspents can only be used for
% transfer.
issue(_P,  _U, _P) ->
	throw(issue_error).

issue(Payment, Payee) ->
	if Payment#payment.issuances =:= 0 ->
			throw(issue_error);
	true ->
	        O = create_output(
				Payee#payee{
							color=lib_color:get_issue_color_unspents(Payment#payment.selected)}),
		Payment#payment{issuances = Payment#payment.issuances + 1,
			outputs = Payment#payment.outputs ++ [O],
			r_value = Payment#payment.r_value - O#btxout.value}
	end.			

pay(Payment, Unspents, []) ->
	{Unspents, Payment};

pay(Payment, Unspents, Payee) when is_list(Payee) ->
	pay(Payment, Unspents, lib_color:color_aggregate(Payee), sorted);

pay(Payment, Unspents, Payee) when
		is_record(Payment, payment), is_record(Payee, payee) ->
	if Payment#payment.r_color =/= Payee#payee.color ->
			% Boundary need to create a change output
			O = Payment#payment.outputs,
			add_payee(Payment#payment{outputs = create_change_output(Payment, O),
					            change=undefined,
					            r_color=?Uncolored,
					            r_value=0}, Unspents, Payee, false);
		true ->
			add_payee(Payment, Unspents, Payee, false)
	end.

pay(Payment, Unspents, Payee, sorted) when is_list(Payee) ->
	[P|R] = Payee,
	{Remaining, NewPayment} = pay(Payment, Unspents, P),
	pay(NewPayment, Remaining, R).


add_payee(Payment, Unspents, Payee, Overflow) ->
	{RemainingUnspents, Selected, Remainder, Output} =
		make_outputs(Unspents, Payee, Payment, Overflow),
	{RemainingUnspents, Payment#payment{outputs=[Output|Payment#payment.outputs],
		    selected=Payment#payment.selected ++ Selected,
		    r_color = Payee#payee.color,
		    r_value = Remainder,
			change = Payee#payee.change}}.

acquire_only(Payment, Unspents, Payee) ->
	{RemainingUnspents, Selected, Remainder} =
	    acquire(Unspents, Payee, Payment),
	{RemainingUnspents, Payment#payment{
		    selected=Payment#payment.selected ++ Selected,
		    r_color = Payee#payee.color,
		    r_value = Remainder,
			change = Payee#payee.change}}.


%% Finish the color
%% For use by partial payments
partial_finish(Payment, Unspents, Change) when is_record(Payment, payment) ->
	P2 = finish_color(Payment, Unspents), 
	{Remaining, P3} = add_change(P2, Unspents, Change),
	{ok, P3, Remaining}.

% Finish and validate a payments object
% Returns a TX ready for signing
finalize(Payment, Unspents, Change) when is_record(Payment, payment) ->
	P = add_dust(Payment),
	P2 = finish_color(P, Unspents), %If not uncolored, send back change to last payee
	P3 = add_fee(P2),
	{Remaining, P4} = add_change(P3, Unspents, Change),
	P5 = P4#payment{outputs = lists:reverse(P4#payment.outputs)},
	P6 = final_check(P5), % Run through payment validation
	P7 = color_marker(P6),
	{ok, Remaining, P7, tx(P7)}.

add_fee(P) ->
	P#payment{fee = lib_tx:calculate_fee(tx(P))}.

spent(P, Color) ->
    A = lib_color:new(Color),
    value(lib_color:hash160(A), P#payment.selected).

included(P, Color) ->
    A = lib_color:new(Color),
    value(lib_color:hash160(A), P#payment.outputs).

fee(P) ->
    P2 = add_fee(P),
    P2#payment.fee.

final_check(P) ->
	check_dust_verify(P),
	check_value(P),
	P.

check_dust_verify(P) ->
	lists:map(fun(O) ->
		if O#btxout.value < ?DUSTLIMIT ->
				throw(insufficient_funds);
			true -> true
	end end, P#payment.outputs).

check_value(P) ->
	BTCinputs = value(?Uncolored, P#payment.selected),
	BTCoutputs = value(?Uncolored, P#payment.outputs),
	BTCFee = BTCinputs - BTCoutputs,
	if 
	    BTCFee < ?DEFAULTFEE ->
            ?debugFmt("Insufficient Fee ~p.~n", [BTCFee]),
            throw(insufficient_funds);
	    BTCFee < 5*?DEFAULTFEE -> P;
	    true -> 
		    ?debugFmt("checkvalue: Fee ~p Inputs: ~p Outputs: ~p ~n", [BTCFee,
		                                                         BTCinputs,
		                                                         BTCoutputs]),
	   		throw(insufficient_funds)
	end.

add_dust(P)  when is_record(P, payment) ->
	P#payment{outputs = check_dust(P#payment.outputs)}.

tx(Payment) when is_record(Payment, payment) ->
	lib_tx:add_output(
		lib_tx:add_unspent(
			lib_tx:create_tx(),
	Payment#payment.selected), Payment#payment.outputs).


make_outputs(Unspents,
			P,  % payee
			Payment,
			Overflow) ->
	case get_inputs(list, Unspents, P, Payment#payment.r_value) of
		{RemainingUnspents, Selected, T} when T =:= P#payee.value ->
			% Requested Amount completely fullfilled
			{RemainingUnspents, Selected, 0, create_output(P)};
		{RemainingUnspents, Selected, T} when T > P#payee.value ->
			if Overflow =:= true ->
				{RemainingUnspents, Selected, 0, create_output(P#payee{value=T})};
			   true ->
				{RemainingUnspents, Selected, T-P#payee.value, create_output(P)}
			end;
		{_RemainingUnspents, _Selected, T} when T < P#payee.value ->
			% Request was only partially filled
			% More than available amount of color was needed
			throw(insufficient_funds)
	end.

acquire(Unspents,
			P,  % payee
			Payment) ->
	case get_inputs(list, Unspents, P, Payment#payment.r_value) of
		{RemainingUnspents, Selected, T} when T =:= P#payee.value ->
			% Requested Amount completely fullfilled
			{RemainingUnspents, Selected, 0};
		{RemainingUnspents, Selected, T} when T > P#payee.value ->
		    {RemainingUnspents, Selected, T-P#payee.value};
		{_RemainingUnspents, _Selected, T} when T < P#payee.value ->
			% Request was only partially filled
			% More than available amount of color was needed
			throw(insufficient_funds)
	end.


finish_color(#payment{r_color=?Uncolored}=P, _) ->
    P;
finish_color(P, Unspents) when is_record(P, payment) ->
    LastOutput = lists:last(Unspents),
	P#payment{outputs=create_change_output(P, P#payment.outputs),
		      change=undefined,
			  r_color=?Uncolored,
			  r_value=LastOutput#utxop.value}.

add_change(P, Unspents, Change) when is_record(P, payment),
		is_record(Change, addr) ->
	BTCinputs = value(?Uncolored, P#payment.selected),
	BTCoutputs = value(?Uncolored, P#payment.outputs),
	BTCneeded = BTCoutputs + P#payment.fee,
	if BTCinputs =:= BTCneeded ->
			{Unspents, P#payment{r_value=0}};
	   BTCinputs > BTCneeded ->
			% Need to return some of this to Change
			Diff = BTCinputs - BTCneeded,
            %?debugFmt("Spent greater than needed: ~p~n", [Diff]),
			{Unspents, P#payment{outputs = create_change_output(Change, Diff, P#payment.outputs),
								 r_value = 0}};
	   BTCinputs < BTCneeded ->
	   		Diff = BTCneeded - BTCinputs,
            %?debugFmt("Spent less than needed: ~p~n", [Diff]),
	   		% Not enough BTC to cover fees
	   		% Create a payment back to change addr
	   		% Overflow = true so total acquired BTC goes back to change
            {R, P2} = acquire_only(P, Unspents, payee(Change, Change, ?Uncolored, Diff)),
			add_change(P2, R, Change)
	end.

% Insert an open assets color marker
color_marker(P) when is_record(P, payment) ->
	case lib_color:is_colored(P#payment.outputs) of
		true ->
			lib_color:marker(P);
		false ->
			P
	end.

create_change_output(#payment{r_value = 0}, O) -> O;
create_change_output(#payment{r_color = ?Uncolored}, O) -> O;

create_change_output(P, O) when is_record(P, payment) ->
	[lib_tx:create_output(lib_address:type(P#payment.change),
		P#payment.r_color, P#payment.r_value,
			P#payment.change#addr.bin)|O].

create_change_output(P, Value, O) when is_record(P, addr) ->
	[lib_tx:create_output(lib_address:type(P),
		?Uncolored, Value,
		P#addr.bin)|O].

create_output(P) when is_record(P, payee) ->
	lib_tx:create_output(lib_address:type(P#payee.address),
	        P#payee.color, P#payee.value,
			P#payee.address#addr.bin).


get_value(O) when is_record(O, utxop) -> O#utxop.value;
get_value(O) when is_record(O, btxout) -> O#btxout.value.

%get_quant(O) when is_record(O, utxop) -> O#utxop.quantity;
%get_quant(O) when is_record(O, btxout) -> O#btxout.quantity.

value(?Uncolored, Outputs) when is_list(Outputs) ->
	lists:foldl(fun(O, Sum) ->
		get_value(O) + Sum
	end, 0, Outputs).

get_available_value(#utxop{color = ?Uncolored} = O) -> O#utxop.value;
get_available_value(#btxout{color = ?Uncolored} = O) -> O#btxout.value;
get_available_value(_) -> 0.

available(?Uncolored, Outputs) when is_list(Outputs) ->
	lists:foldl(fun(O, Sum) ->
		get_available_value(O) + Sum
	end, 0, Outputs).

%value(Color, Outputs) when is_list(Outputs) ->
%	lists:foldl(fun(O, Sum) ->
%		case O#btxout.color of
%			Color ->
%				get_quant(O) + Sum;
%			_ ->
%				O
%			end
%	end, 0, Outputs).
%
check_dust(Outputs) ->
	lists:map(fun(O) ->
		if O#btxout.value =< ?DUSTLIMIT ->
				O#btxout{value = ?DUSTLIMIT};
			true ->
				O
	end end, Outputs).


% Get Inputs from Utxop list 
% Which should be ordered
% Until Color is satisfied
% Total value is probably > Requested which will require change
% or Flow over into next payee
% Returns {Utxop Left, Utxop Selected, Total Value Delivered}

get_inputs(list,
		   [],
		   _Payee,
		   Remainder) when Remainder > 0 ->
	{[], [], Remainder};

get_inputs(list,
		   UnspentList,
		   Payee = #payee{color=?Uncolored},
	       Remainder) ->
    %?debugFmt("get uncolor inputs~n", []),
    %?debugFmt("Pcolor: ~p ~n", [?Uncolored]),
    %?debugFmt("U: ~p~n", [UnspentList]),
	lists:foldl(fun(U, {UL, SL, Total}) ->
				case U#utxop.color of
				    ?Uncolored ->
						if Total =< Payee#payee.value ->
							{UL, [U|SL], Total + U#utxop.value};
				   		true ->
				   			{[U|UL], SL, Total}
						end;
				    _ ->
				    	{[U|UL], SL, Total}
				end end, {[], [], Remainder}, UnspentList);

get_inputs(list,
		   UnspentList,
		   Payee,
	       Remainder) ->
    %?debugFmt("get color inputs~n", []),
    %?debugFmt("Pcolor: ~p ~n", [Payee#payee.color]),
    %?debugFmt("U: ~p~n", [UnspentList]),
	PColor = Payee#payee.color,
	lists:foldl(fun(U, {UL, SL, Total}) ->
				case U#utxop.color of
					PColor ->
					    %?debugFmt("COlor quant: ~p~n", [U#utxop.quantity]),
						if Total =< Payee#payee.value ->
								{UL, [U|SL], Total + U#utxop.quantity};
				   			true ->
				   				{[U|UL], SL, Total}
							end;
				    _ ->
				    	{[U|UL], SL, Total}
				end end, {[], [], Remainder}, UnspentList).

get_colored_unspents(dict, Color, Unspents) ->
	dict:filter(fun(_K, V) ->
					case V#utxop.color of
						Color ->
							true;
						_ ->
							false
					end
				end, Unspents).

get_quantity_unspents(dict, Color, Unspents) ->
	dict:fold(fun(_K, V, Acc) ->
				case {V#utxop.color, Color} of
					{Color, ?Uncolored} ->
						Acc + V#utxop.value;
					{Color, _} ->
						Acc + V#utxop.quantity;
					_ ->
						Acc
				end
				end, 0, Unspents);

get_quantity_unspents(map, Color, Unspents) ->
	maps:fold(fun(_K, V, Acc) ->
				case {V#utxop.color, Color} of
					{Color, ?Uncolored} ->
						Acc + V#utxop.value;
					{Color, _} ->
						Acc + V#utxop.quantity;
					_ ->
						Acc
				end
				end, 0, Unspents);

get_quantity_unspents(list, Color, Unspents) ->
	lists:foldl(fun(V, Acc) ->
				case {V#utxop.color, Color} of
					{Color, ?Uncolored} ->
						Acc + V#utxop.value;
					{Color, _} ->
						Acc + V#utxop.quantity;
					_ ->
						Acc
				end
				end, 0, Unspents).
