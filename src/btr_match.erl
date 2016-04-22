%%
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

%% Matching functions

-module(btr_match).
-author('mbranton@emberfinancial.com').

-export([true_fun/0,
		 true_fun/1,
		 false_fun/0,
		 false_fun/1,
		 bloom_fun/1,
		 bloom_fun/2,
		 match_helper/1,
		 confirmed/1,
		 unconfirmed/1,
		 color_fun/1,
		 depth_fun/2,
		 random_fun/1,
		 coinbase_fun/1,
		 run_fundefs/2]).

-include_lib("bitter.hrl").


run_fundefs(Defs, Datum) ->
	try
		run_fundefs_do(Defs, Datum)
	catch _:_ -> false
	end.

run_fundefs_do(F, Datum) when is_function(F) -> F(Datum);

run_fundefs_do([], _) -> false;
run_fundefs_do([D|T], Datum) ->
	case D(Datum) of
		true -> true;
		false -> run_fundefs_do(T, Datum)
	end.

match_helper({bloom, BloomFilter}) -> bloom_fun(BloomFilter);
match_helper({color, Color}) -> color_fun(Color);
match_helper(true) -> fun true_fun/1;
match_helper(false) -> fun false_fun/1;
match_helper(F) -> F.

true_fun() ->  fun true_fun/1.
false_fun() -> fun(X) -> false_fun(X) end.

true_fun(_) -> true.
false_fun(_) -> false.

confirmed(#utxop{state = ?Spent_Unconfirmed}) -> false;
confirmed(#utxop{state = ?Unspent_Unconfirmed}) -> false;
confirmed(#utxop{}) -> true;
confirmed(_) -> false.

unconfirmed(#utxop{state = ?Unspent_Unconfirmed}) -> true;
unconfirmed(#utxop{}) -> false;
unconfirmed(_) -> false.

color_fun(Color) ->
	fun(U) ->
			case lib_unspent:get_attribute(color, U, false) of
				false -> false;
				Color -> true
			end
	end.

depth_fun(-1, _Height) -> fun unconfirmed/1;
depth_fun(Depth, Height) ->
	fun(#utxop{height = H}) when Height-Depth >= H -> true;
	   (_) -> false
	end.

coinbase_fun(Height) ->
	fun(#utxop{coinbase = false}) -> true;
	   (#utxop{coinbase = true, height = H}) when H >= Height-50 -> true;
	   (_) -> false
	end.

random_fun(Thresh) ->
	X = random:uniform(),
	fun (_) when X > Thresh -> true;
		(_) -> false
	end.

bloom_fun(BloomFilter) -> bloom_fun(BloomFilter, all).

bloom_fun(BloomFilter, all) -> bloom_fun(BloomFilter, [all]);
bloom_fun(BloomFilter, FieldList) ->
	fun(Datum) ->
			try
				check_fields(BloomFilter, Datum, FieldList)
			catch
				_:_ -> true
			end
	end.

check_fields(BloomFilter, Datum, _) when is_record(Datum, btxout) ->
	false = (bitter_bloom:contains(BloomFilter, Datum#btxout.address) or
	bitter_bloom:contains(BloomFilter, Datum#btxout.script));
check_fields(BloomFilter, Datum, _) when is_record(Datum, btxin) ->
	false = (bitter_bloom:contains(BloomFilter, Datum#btxin.txhash) or
	bitter_bloom:contains(BloomFilter, Datum#btxin.script));
check_fields(BloomFilter, Datum, _) when is_record(Datum, utxop) ->
	false = (bitter_bloom:contains(BloomFilter, Datum#utxop.address) or
	bitter_bloom:contains(BloomFilter, Datum#utxop.script));

%% Selective Bloom Filter Checking on TX inputs and Outputs
check_fields(_BloomFilter, _Tx, []) -> false;
check_fields(BloomFilter, Tx, [Field|T]) ->
	compare_field(Field, Tx, BloomFilter),
	check_fields(BloomFilter, Tx, T).

compare_field(txhash, Tx, BloomFilter) ->
	false = bitter_bloom:contains(BloomFilter, Tx#btxdef.txhash);
compare_field(input, Tx, BloomFilter) ->
	lists:foreach(fun(Input) -> false = bitter_bloom:contains(BloomFilter, Input#btxin.txhash) end,
				  Tx#btxdef.txinputs);
compare_field(address, Tx, BloomFilter) ->
	lists:foreach(fun(Output) -> false = bitter_bloom:contains(BloomFilter, Output#btxout.address) end,
				  Tx#btxdef.txoutputs);
compare_field(script, Tx, BloomFilter) ->
	lists:foreach(fun(Output) -> false = bitter_bloom:contains(BloomFilter, Output#btxout.script) end,
				  Tx#btxdef.txoutputs);
%% Do I want to do this? What if color is <<0>> ?
compare_field(color, Tx, BloomFilter) ->
	lists:foreach(fun(Output) -> false = bitter_bloom:contains(BloomFilter, lib_unspent:get_attribute(color, Output, ?Uncolored)) end,
				  Tx#btxdef.txoutputs);
compare_field(all, Tx, BloomFilter) ->
	compare_field(txhash, Tx, BloomFilter),
	compare_field(input, Tx, BloomFilter),
	compare_field(address, Tx, BloomFilter),
	compare_field(script, Tx, BloomFilter),
	compare_field(color, Tx, BloomFilter).

