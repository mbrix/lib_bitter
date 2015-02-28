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

-module(lib_config).
-author('mbranton@emberfinancial.com').

-export([load_config/1,
		 load_config/2,
		 save_config/2]).

%% Shared Configuration functions
%% Serialize save and load
load_config(Filename) -> load_config(#{}, Filename).
load_config(Defaults, Filename) ->
	try
		{ok, Data} = file:read_file(Filename),
		maps:merge(Defaults, erlang:binary_to_term(Data))
	catch
		_:_ -> Defaults 
	end.

save_config(Filename, State) ->
	try
		{ok, Fd} = file:open(Filename, [write, binary]),
		file:write(Fd, erlang:term_to_binary(State)),
		file:close(Fd),
		ok
	catch
		_:_ -> error
	end.
