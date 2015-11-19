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

-module(btr_rest_utils).

-export([bin_to_num/1,
		 bin_to_hsh/1,
		 list_to_num/1,
		 string_to_bin/1,
		 default_val/2,
		 hsh_to_bin/1]).

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

list_to_num(Bin) ->
    case string:to_float(Bin) of
        {error,no_float} -> list_to_integer(Bin);
        {F,_Rest} -> F
    end.

string_to_bin(S) when is_atom(S) -> S;
string_to_bin(S) when is_list(S) -> erlang:list_to_binary(S);
string_to_bin(S) -> S.

default_val(undefined, Val) -> Val;
default_val(Value, _) -> Value.

bin_to_hsh(S) when is_binary(S) ->
	hex:bin_reverse(hex:hexstr_to_bin(binary_to_list(S))).
hsh_to_bin(S) when is_binary(S) ->
	iolist_to_binary(hex:bin_to_hexstr(hex:bin_reverse(S))).
