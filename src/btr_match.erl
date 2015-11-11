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

-module(btr_match).
-author('mbranton@emberfinancial.com').

-export([true_fun/0,
		 true_fun/1,
		 false_fun/0,
		 false_fun/1,
		 bloom_fun/1,
		 bloom_fun/2]).

-include_lib("bitter.hrl").

true_fun() -> fun(X) -> true_fun(X) end.
false_fun() -> fun(X) -> false_fun(X) end.

true_fun(_) -> true.
false_fun(_) -> false.

bloom_fun(BloomFilter) -> bloom_fun(BloomFilter, all).

bloom_fun(BloomFilter, all) -> bloom_fun(BloomFilter, [all]);
bloom_fun(BloomFilter, FieldList) ->
	fun(Datum) ->
			try
				check_fields(BloomFilter, Datum, FieldList),
				false
			catch
				_:_ -> true
			end
	end.

check_fields(BloomFilter, Datum, _) when is_record(Datum, btxout) ->
	bitter_bloom:contains(BloomFilter, Datum#btxout.address); 
check_fields(BloomFilter, Datum, _) when is_record(Datum, btxin) ->
	bitter_bloom:contains(BloomFilter, Datum#btxin.txhash);
check_fields(BloomFilter, Datum, _) when is_record(Datum, utxop) ->
	bitter_bloom:contains(BloomFilter, Datum#utxop.address);

%% Selective Bloom Filter Checking on TX inputs and Outputs
check_fields(_BloomFilter, _Tx, []) -> ok;
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
compare_field(color, Tx, BloomFilter) ->
	lists:foreach(fun(Output) -> false = bitter_bloom:contains(BloomFilter, Output#btxout.color) end,
				  Tx#btxdef.txoutputs);
compare_field(all, Tx, BloomFilter) ->
	compare_field(txhash, Tx, BloomFilter),
	compare_field(input, Tx, BloomFilter),
	compare_field(address, Tx, BloomFilter),
	compare_field(script, Tx, BloomFilter),
	compare_field(color, Tx, BloomFilter).

