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

-module(unspentset_tests).
-author('mbranton@emberfinancial.com').

-include_lib("../include/bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
	btr_net_params:init(main),
	ok.

stop(_) -> ok.


store() ->
    HexBlock = erlang:binary_to_list(lib_test:data("rawblock2.hex")),
    RawBlock = hex:hexstr_to_bin(HexBlock),
    {continue, Block, _BlockOffset, _Offsets, _} = lib_parse:parse_raw_block(bblock, btr_net_params:params(), RawBlock),

    %% Let's build an unspent set that has everything in it

	US = bblock:foldl(fun(Btx, UnspentSet) ->
	                          unspentset:add(bblock:hash(Btx), bblock:outputs(Btx), 1, false, UnspentSet)
                      end, unspentset:new(), Block),

    %% Now lookup every output and compare check_spent

    bblock:foldl(fun(Btx, _) ->
                         Hash = bblock:hash(Btx),
                         bblock:foldl_outputs(fun(Output, _) ->
                                                      ?assertMatch({ok, _}, unspentset:lookup(Hash, bblock:index(Output), US))
                                              end, ok, Btx)
                 end, ok, Block),

    %% Now spend everything in the output set

    US4 = bblock:foldl(fun(Btx, US2) ->
                         Hash = bblock:hash(Btx),
                         bblock:foldl_outputs(fun(Output, US3) ->
                                                      ?assertMatch({ok, _},
                                                                   unspentset:remove(Hash, bblock:index(Output), US3)),
                                                      US3
                                              end, US2, Btx)
                 end, US, Block),
    ?assertEqual(0, unspentset:count(US4)).


unspentset_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
    {"store", fun store/0}
   ]
  }.



