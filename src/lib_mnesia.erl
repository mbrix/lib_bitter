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

% Mnesia Helper Functions

-module(lib_mnesia).
-export([db_migrate/4,
         migrate_fragments/3,
         get_fragments/1]).

db_migrate(dev, _, _, _) -> ok;
db_migrate(_, Tab, NodeList, Type) ->
    Fragments = get_fragments(Tab),
    migrate_fragments(Fragments, NodeList, Type).

migrate_fragments([], _NodeList, _Type) -> ok;
migrate_fragments(Fragments, NodeList, Type) ->
    [H|T] = Fragments,
    mnesia:change_table_copy_type(H, NodeList, Type),
    migrate_fragments(T, NodeList, Type).

get_fragments(Tab) ->
    mnesia:activity(async_dirty,
                    fun mnesia:table_info/2,
                    [Tab, frag_names], mnesia_frag).


