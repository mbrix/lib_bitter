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

-module(lib_unspent_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
	btr_net_params:init(main),
	ok.

stop(_) ->
	ok.

json_serialization() ->
    Unspents = lists:map(fun(_) -> lib_test:random_unspent(random:uniform(350000)) end,
              lists:seq(1,100)),
    _JsonList = lib_unspent:to_json(btr_net_params:params(), Unspents, 1000000).

confirmation_filter() ->
    Unspents = [lib_test:random_unspent(100),
                lib_test:random_unspent(101),
                lib_test:random_unspent(102)],
    FilterA = lib_unspent:filter_by_confirmations(Unspents, 102, 0, 999999),
    ?assertEqual(Unspents, FilterA),
    FilterB = lib_unspent:filter_by_confirmations(Unspents, 102, 2, 999999),
    ?assertEqual(2, length(FilterB)),
    FilterC = lib_unspent:filter_by_confirmations(Unspents, 102, 0, 2),
    ?assertEqual(2, length(FilterC)).

unspent_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"Serialize unspents to json", fun json_serialization/0},
		{"Confirmation filter", fun confirmation_filter/0}
   ]
  }.
