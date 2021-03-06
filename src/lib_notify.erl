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

-module(lib_notify).
-author('mbranton@emberfinancial.com').

-export([find/2,
		 connect/1,
         connect_nodes/1,
         get_best_pid/1,
         near/1,
         fast/1,
         random/1,
         where/2,
         bregister/2,
         bunregister/2,
         join/1,
         leave/1,
         broadcast/2,
         global_counter/1,
         inc/1,
         goodbye/0]).

%%% Connect functions

connect(Module) ->
	connect_nodes(application:get_env(Module, nodes, undefined)).

connect_nodes(undefined) -> ok;
connect_nodes(N) when is_atom(N) -> connect_node(N);
connect_nodes([]) -> ok;
connect_nodes(NodeList) when is_list(NodeList) ->
	[H|T] = NodeList,
	connect_node(H),
	connect_nodes(T).

connect_node(Node) ->
    net_kernel:connect_node(Node).

%% Pid identity functions

get_best_pid(Group) ->
  Members = pg2:get_members(Group),
  Members1 = lists:map(fun(Pid) ->
      [{message_queue_len, Messages}] =
            erlang:process_info(Pid,
                [message_queue_len]),
                {Pid, Messages}
            end, Members),
   case lists:keysort(2, Members1) of
     [{Pid, _} | _] -> Pid;
     [] -> {error, empty_process_group}
   end.

where(near, Mod) -> near(Mod);
where(fast, Mod) -> fast(Mod);
where(random, Mod) -> random(Mod);
where(Pid, _Mod) -> Pid.

%% Location
near(Mod) ->
	P = pg2:get_closest_pid(Mod),
    case P of 
        [] -> throw(pid_missing);
        {error, _} -> throw(pid_missing);
        Pid -> Pid
    end.

fast(Mod) ->
	case get_best_pid(Mod) of
		{error, _} -> throw(pid_missing);
		Pid -> Pid
	end.

random(Mod) ->
	Pids = pg2:get_members(Mod),
    case Pids of 
        {error, _} -> throw(pid_missing);
        [] -> throw(pid_missing);
         _  -> lists:nth(rand:uniform(length(Pids)), Pids)
    end.

%% Broadcast and messaging functions

find(local, Name) ->
    gproc:where({n,l,Name});

find(global, Name) ->
    gproc:where({n,g,Name}).


bregister(local, Name) ->
    gproc:reg({n,l,Name});

bregister(global, Name) ->
    gproc:reg({n,g,Name}).

bunregister(local, Name) ->
    gproc:unreg({n,l,Name});
bunregister(global, Name) ->
    gproc:unreg({n,g,Name}).

join(Group) ->
    gproc:reg({p,l,Group}).

leave(Group) ->
    gproc:unreg({p,l,Group}).

broadcast(Group, Msg) ->
	rpc:eval_everywhere(
	  gproc, send,
	  [{p, l, Group}, {Group, self(), Msg}]).

global_counter(Name) ->
    gproc:add_global_aggr_counter(Name).

inc(Name) ->
    gproc:update_counter({n,g,Name}, 1).

goodbye() ->
    gproc:goodbye().
