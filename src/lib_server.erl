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

%% Miscelaneous compound functions useful for testing

-module(lib_server).
-author('mbranton@emberfinancial.com').

-export([hibernation_on/1,
		 hibernation_on/2,
		 hibernation_on/3,
		 hibernate/2,
		 hibernate/3,
		 hibernate/4]).

-define(GCTHRESH, 500).

hibernation_on(S) -> hibernation_on(S, ?GCTHRESH). 
hibernation_on(S, Fun) when is_function(Fun) -> hibernation_on(S, Fun, ?GCTHRESH);
hibernation_on(S, Threshold) -> S#{gcthresh => Threshold, processed => 1}.

hibernation_on(S, Fun, Threshold) when is_function(Fun) -> S#{gcthresh => Threshold, processed => 1, hibernate_fun => Fun};

hibernation_on(S, GCThreshold, SizeThreshold) ->
    S#{gcthresh => GCThreshold, sizethresh => SizeThreshold, processedbytes => 0}.


%% Hibernate by gcthresh
hibernate(reply, Response, #{gcthresh := GCThresh, processed := P} = S) when P > 0 ->
	do_hibernate(reply, Response, P rem GCThresh, S);
hibernate(reply, Response, #{processed := P} = S) -> {reply, Response, S#{processed => P+1}};

hibernate(noreply, 0, S) -> 
	hibernate_fun(S),
	{noreply, S#{processed => 1}, hibernate};
hibernate(noreply, _, #{processed := P} = S) -> {noreply, S#{processed => P+1}}.


hibernate(reply, Response, #{sizethresh := SThresh, processedbytes := PB}=S, Size) when PB+Size > SThresh ->
    do_hibernate(reply, Response, 0, S);

hibernate(reply, Response, #{processedbytes := PB}=S, Size) ->
    {reply, Response, S#{processedbytes => PB + Size}}.

do_hibernate(reply, Response, 0, S) ->
	hibernate_fun(S),
	{reply, Response, S#{processed => 1, processedbytes => 0}, hibernate};
do_hibernate(reply, Response, _, #{processed := P} = S) -> {reply, Response, S#{processed => P+1}}.

hibernate(noreply, #{gcthresh := GCThresh, processed := P} = S) when P > 0 ->
	hibernate(noreply, P rem GCThresh, S);
hibernate(noreply, #{processed := P} = S) -> {noreply, S#{processed => P+1}}.


hibernate_fun(#{hibernate_fun := HF}) -> HF();
hibernate_fun(_) -> ok.
