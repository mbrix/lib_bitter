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

-module(lib_kd_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").


start() -> ok.

stop(_) -> ok.

oldest() ->
    A = lib_test:random_unspent(100),
    B = lib_test:random_unspent(101),
    C = lib_test:random_unspent(102),
    D = lib_test:random_unspent(103),
    UnspentDict = add_to_dict([A, B, C, D]),
    ?assertEqual([A,B,C,D], lib_kd:oldest(UnspentDict)).

oldest_unconfirmed() ->
    A = lib_test:random_unspent(-1),
    B = lib_test:random_unspent(100),
    C = lib_test:random_unspent(101),
    D = lib_test:random_unspent(102),
    UnspentDict = add_to_dict([A,B,C,D]),
    ?assertEqual([B,C,D,A], lib_kd:oldest(UnspentDict)).


kd_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"Oldest unspents", fun oldest/0},
		{"Unconfirmed order", fun oldest_unconfirmed/0}
   ]
  }.


add_to_dict(Unspents) -> add_to_dict(Unspents, dict:new()).
add_to_dict([], Acc) -> Acc;
add_to_dict(Unspents, Acc) ->
    [H|T] = Unspents,
    Hash = lib_unspent:hash(H),
    Index = lib_unspent:index(H),
    add_to_dict(T, dict:store({Hash,Index}, H, Acc)).

