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

%% Idempotent dynamic module for network configuration data

-module(btr_net_params).

-export([init/1,
         params/0,
         params/1]).

-export([network/0,
         genesis_hash/0]).

-include_lib("bitter.hrl").

init(Network) ->
    init(code:is_loaded(btr_net_prms), Network).

init(false, Network) -> init(false, Network, default_params(Network));
init(_, _) -> ok.

init(_, _Network, Params) ->
    Module = erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(btr_net_prms)]),
    ModForm =  erl_syntax:revert(Module),

    Export = erl_syntax:attribute(erl_syntax:atom(export),
                                  [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(params),
                                                                               erl_syntax:integer(0))])]),
    ExportForm = erl_syntax:revert(Export),

    Clause1 =  erl_syntax:clause([],[],[ast_syntax_body(Params)]),

    Function =  erl_syntax:function(erl_syntax:atom(params),[Clause1]),
    FunctionForm = erl_syntax:revert(Function),

    {ok, Mod, Bin1} = compile:forms([ModForm,ExportForm, FunctionForm]),
    code:load_binary(Mod, [], Bin1),
    ok.

%% Helper functions

network() -> maps:get(network, params()).
genesis_hash() -> maps:get(genesis_hash, params()).


%% Default params

default_params(main = Net) ->
    #{network => Net,
      magicbyte          => ?MAGICBYTE_LIVE,
      p2pkh_checkbyte    => <<0:8>>,
      p2sh_checkbyte     => <<5:8>>,
      oa_checkbyte       => <<19:8>>,
      oa_assetbyte       => <<23:8>>,
      wif                => <<128:8>>,
      default_port       => 8333, 
      genesis_hash       => lib_utils:convert_hash("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"),
      block_path         => "/.bitcoin/blocks/"};


default_params(testnet = Net) ->
    #{network => Net,
      magicbyte          => ?MAGICBYTE_TESTNET,
      p2pkh_checkbyte    => <<111:8>>,
      p2sh_checkbyte     => <<196:8>>,
      oa_checkbyte       => <<19:8>>,
      oa_assetbyte       => <<115:8>>,
      wif                => <<239:8>>,
      default_port       => 18333, 
      genesis_hash       => lib_utils:convert_hash("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"),
      block_path         => "/.bitcoin/testnet/blocks/"};


default_params(testnet3 = Net) ->
    #{network => Net,
      magicbyte          => ?MAGICBYTE_TESTNET3,
      p2pkh_checkbyte    => <<111:8>>,
      p2sh_checkbyte     => <<196:8>>,
      wif                => <<239:8>>,
      oa_checkbyte       => <<19:8>>,
      oa_assetbyte       => <<115:8>>,
      default_port       => 18333, 
      genesis_hash       => lib_utils:convert_hash("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"),
      block_path         => "/.bitcoin/testnet3/blocks/"}.

ast_syntax_body(NetMap) ->
    erl_syntax:map_expr( maps:fold(fun(K, V, Acc) when is_integer(V) ->
                                           [erl_syntax:map_field_assoc(erl_syntax:atom(K),
                                                                       erl_syntax:integer(V))|Acc];
                                      (K, V, Acc) when is_binary(V) ->
                                           [erl_syntax:map_field_assoc(erl_syntax:atom(K),
                                                                       erl_syntax:binary(lists:map(fun(I) ->
                                                                                                           erl_syntax:binary_field(erl_syntax:integer(I))
                                                                                                   end, binary_to_list(V))))|Acc];
                                      (K, V, Acc) when is_atom(V) ->
                                           [erl_syntax:map_field_assoc(erl_syntax:atom(K), erl_syntax:atom(V))|Acc];
                                      (K, V, Acc) when is_list(V) ->
                                           [erl_syntax:map_field_assoc(erl_syntax:atom(K), erl_syntax:char(V))|Acc]
                                   end, [], NetMap)).

params() ->  get_params(code:is_loaded(btr_net_prms)).

params(Alternate) -> default_params(Alternate).

get_params(false) -> throw(network_params_not_initialized);
get_params(_)  -> btr_net_prms:params().

