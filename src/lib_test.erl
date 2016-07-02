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

-module(lib_test).
-author('mbranton@emberfinancial.com').

% Misc helper functions for debugging

-export([term_to_file/2,
	     term_from_file/1,
		 fake_block/1,
		 fake_block_pure/1,
         create_random_address/0,
         inputs_from_outputs/2,
         create_input/0,
         create_input/2,
         create_outputs/1,
         create_output/0,
         create_output/1,
         create_output/4,
         create_transaction/0,
         create_transaction/2,
         create_transaction/3,
         create_block/2,
	     create_block/4,
	     create_simple_chain/3,
	     create_simple_chain/4,
	     sum_outputs/1,
	     sum_inputs/1,
	     create_random_input/0,
	     create_input/1,
	     create_p2pkh_input/1,
	     create_p2sh_input/1,
	     create_random_p2pkh_input/0,
	     create_random_output/0,
	     create_random_output/1,
	     create_random_output/2,
		 output_to_unspent/5,
	     random_p2sh_input/1,
	     random_unspent/1,
	     random_unspent/2,
	     random_unspent/4,
	     data/1,
	     output_to_input/1]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

get_testfile(Filename) ->
	filename:join(filename:absname("../test-data/"), Filename).

term_to_file(Term, Filename) ->
	{ok, Fd} = file:open(get_testfile(Filename), [write, binary]),
	file:write(Fd, erlang:term_to_binary(Term)),
	file:close(Fd),
	ok.

data(Filename) ->
	{ok, Data} = file:read_file(get_testfile(Filename)),
	Data.

term_from_file(Filename) ->
	{ok, Data} = file:read_file(get_testfile(Filename)),
	erlang:binary_to_term(Data).

fake_block(Filename) ->
	A = term_from_file(Filename),
    B = zero_inputs(A), % Strip unused inputs
	B#bbdef{previoushash = ?CHAIN_ROOT}.

% don't zero inputs
fake_block_pure(Filename) ->
	A = term_from_file(Filename),
	B = A#bbdef{previoushash = ?CHAIN_ROOT},
	bblock:bblock(B).

zero_inputs(Block) ->
	Txdata = lists:map(fun(Tx) -> zero_inputs_tx(Tx) end,
			 Block#bbdef.txdata),
	Block#bbdef{txdata=Txdata}.

zero_inputs_tx(Tx) ->
	Tx#btxdef{txinputs=[]}.

create_random_address() ->
	{Public, _} = lib_address:generate_keypair(),
	lib_address:new(key, Public).

% Lets fake and mock transactions
inputs_from_outputs(Txhash, Outputs) ->
	lists:map(fun(E) ->  create_input(Txhash, bblock:index(E)) end, Outputs).

output_to_unspent(Output, Hash, Index, Height, Coinbase) when is_record(Output, btxout) ->
    O = #boutput{data = <<(Output#btxout.value):64/little,
                       (lib_parse:int_to_varint(size(Output#btxout.script)))/binary,
                    (Output#btxout.script)/binary>>,
                 meta = Output#btxout.attributes},
                 output_to_unspent(O, Hash, Index, Height, Coinbase);
%%	#utxop{hash_index = {Txhash, O#btxout.txindex},
%%		   value = O#btxout.value,
%%		   script = O#btxout.script,
%%		   address = O#btxout.address,
%%		   info = O#btxout.info, 
%%		   attributes = #{color => filter_color(lib_tx:get_attribute(color, O, ?Uncolored)),
%%		   				  quantity => lib_tx:get_attribute(quantity, O, 0)},
%%		   height = 0,
%%	       state = undefined,
%%	       coinbase = false}.
%%


output_to_unspent(Output, Hash, Index, Height, Coinbase) ->
    unspentset:create_unspent(Output, Hash, Index, Height, Coinbase).

random_unspent(utxop, Height) ->
    Address = create_random_address(),
	PubkeyBin = lib_address:hash160(Address),
	Script = lib_tx:create_script(p2pkh, PubkeyBin),
    #utxop{hash_index = {crypto:strong_rand_bytes(32), rand:uniform(1000)},
           value = rand:uniform(100000000),
           script = Script,
           address = lib_address:hash160(Address),
           info = lib_parse:parse_script(Script),
           attributes = #{color => ?Uncolored,
                          quantity => 0},
           height = Height,
           state = ?Unspent_Confirmed,
           coinbase = false}.

random_unspent(Height) -> random_unspent(Height,
                                         crypto:strong_rand_bytes(32),
                                         rand:uniform(1000),
                                         create_random_address()).

random_unspent(Height, Hash, Index, Address) ->
	PubkeyBin = lib_address:hash160(Address),
	Script = lib_tx:create_script(p2pkh, PubkeyBin),
    O = #boutput{data = <<(rand:uniform(100000000)):64/little,
                       (lib_parse:int_to_varint(size(Script)))/binary,
                       Script/binary>>,
             ext = #{index => Index},
             meta = #{}},
    output_to_unspent(O, Hash, Index, Height, false).

random_p2sh_input(Type) ->
	{Addr, KeyList} = lib_address:generate_p2sh_address(Type),
	O = create_random_output(Addr),
	Txhash = crypto:strong_rand_bytes(32),
	U = output_to_unspent(O, Txhash, bblock:index(O), 1, false),
	UnspentDict = lib_kd:add(U),
	I = lib_unspent:create_input(U),
	{Addr, KeyList, UnspentDict, I}.

% Create Unspent / Input pairs
create_p2sh_input(PubkeyList) ->
	I = create_random_input(),
	{#utxop{hash_index={I#btxin.txhash, I#btxin.txindex},
			script = lib_tx:create_script(p2sh, PubkeyList)},
	I}.

create_p2pkh_input(Address) ->
	I = create_random_input(),
	{#utxop{hash_index={I#btxin.txhash, I#btxin.txindex},
			script = lib_tx:create_script(p2pkh, Address)},
	I}.

create_random_p2pkh_input() ->
	I = create_random_input(),
	PubkeyBin = lib_address:hash160(create_random_address()),
	I#btxin{script = lib_tx:create_script(p2pkh, PubkeyBin)}.

create_random_input() ->
	create_input(crypto:strong_rand_bytes(32), rand:uniform(100)).

create_input(Addr) when is_record(Addr, addr) ->
	I = create_random_p2pkh_input(),
	{#utxop{hash_index={I#btxin.txhash, I#btxin.txindex},
			script = lib_address:script(Addr)},
	I}.

create_input() -> create_input(?COINBASE, 0).
create_input(Txhash, Txindex) ->
	#btxin{txhash=Txhash,
		   txindex=Txindex,
		   script = <<>>,
		   seqnum=0}.

create_outputs(Num) ->
	lists:map(fun(X) -> create_output(boutput, X) end, lists:seq(0,Num-1)).

create_random_output() ->
	create_output(boutput,
	              rand:uniform(100),
		          rand:uniform(10000000),
		          lib_address:hash160(create_random_address())).

create_output() -> create_output(boutput, 0).

create_output(TxIndex) -> create_output(boutput, TxIndex).

create_output(Type, TxIndex) ->
	create_output(Type,
	              TxIndex,
	              50000,
	              lib_address:address_to_hash160("15MLJpjve5pjPD5aTyK1aBRZ2aW8Vwcwyx")).

create_output(p2sh, TxIndex, Value, Address) ->
    Script = lib_tx:create_script(p2sh, Address),
    #boutput{data = <<Value:64/little,
                       (lib_parse:int_to_varint(size(Script)))/binary,
                       Script/binary>>,
             ext = #{index => TxIndex},
             meta = #{}};

create_output(Type, TxIndex, Value, Address) when is_record(Address, addr) ->
	create_output_type(Type, TxIndex, Value, lib_address:hash160(Address));

create_output(Type, TxIndex, Value, Address) ->
    create_output_type(Type, TxIndex, Value, Address).


create_output_type(boutput, TxIndex, Value, Address) ->
    Script = lib_tx:create_script(p2pkh, Address),
    #boutput{data = <<Value:64/little,
                       (lib_parse:int_to_varint(size(Script)))/binary,
                       Script/binary>>,
             ext = #{index => TxIndex},
             meta = #{}};

create_output_type(btxout, TxIndex, Value, Address) ->
	#btxout{txindex=TxIndex,
		    value=Value,
		    attributes = #{color => ?Uncolored,
		                   quantity => 0},
		    script = lib_tx:create_script(p2pkh, Address),
		    address=Address}.

create_random_output(Address) when is_record(Address, addr) ->
	create_random_output(lib_address:type(Address),
		                 lib_address:hash160(Address));

create_random_output(Type) ->
	create_output(Type,
	              rand:uniform(100),
		          rand:uniform(10000000),
		          lib_address:hash160(create_random_address())).


create_random_output(p2sh, Address) ->
	create_output(p2sh, rand:uniform(100),
		          rand:uniform(10000000),
				  Address).

create_transaction() ->
	create_transaction([create_input()], create_outputs(1)).

create_transaction(Inputs, Outputs) ->
	create_transaction(crypto:strong_rand_bytes(32), Inputs, Outputs).
create_transaction(Txhash, Inputs, Outputs) ->
		#btxdef{txhash=Txhash,
		        txversion=2,
		        inputcount=length(Inputs),
				outputcount=length(Outputs),
				txlocktime=0,
				txinputs=Inputs,
			    txoutputs=Outputs}.

create_block(Network, Txs) -> 
    NetworkParams = btr_net_params:params(Network),
    create_block(Network, maps:get(genesis_hash, NetworkParams), Txs, 1).

create_block(Network, PreviousHash, Txs, Height) ->
    NetworkParams = btr_net_params:params(Network),
    {ok, MerkleRoot} = lib_merkle:build(Txs),
    #bbdef{network = maps:get(magicbyte, NetworkParams),
           headerlength = 80,
           e_height = Height,
           version = 2,
           previoushash = PreviousHash,
           merkleroot = lib_merkle:hash(MerkleRoot),
           timestamp = timestamp(),
           difficulty = 1000,
           txdata = Txs,
           nonce = rand:uniform(100000)}.

create_simple_chain(Network, Iterations, NumOutputs) when is_atom(Network) ->
	T = create_transaction([create_input()], create_outputs(NumOutputs)),
	Block = create_block(Network, [T]),
	create_simple_chain(Block, Network, Iterations, NumOutputs).


create_simple_chain(Block, Network, Iterations, NumOutputs) when is_record(Block, bbdef) ->
    %% Seed the initial block
	{Acc, _B} = lists:mapfoldl(fun(Seq, Prev) ->
	                                   [Tdata|_] = Prev#bbdef.txdata,
	                                   B2 = inputs_from_outputs(bblock:hash(bblock:btx(Tdata)), Tdata#btxdef.txoutputs),
	                                   T2 = create_transaction(B2, create_outputs(NumOutputs)),
	                                   A2 = create_block(Network, bblock:hash(bblock:bblock(Prev)), [T2], Seq),
	                                   {A2, A2}
	                           end, Block, lists:seq(1, Iterations-1)),
	%% A is the block after genesis
	[Block|Acc].

timestamp() -> erlang:system_time().

sum_outputs(Txdata) ->
	Sum = lists:foldl(fun(X, S) -> S + length(X#btxdef.txoutputs) end,
		0, Txdata),
    Expected = lists:foldl(fun(X, S) -> S + X#btxdef.outputcount end,
		0, Txdata),
	{Sum, Expected}.

sum_inputs(Txdata) ->
	Sum = lists:foldl(fun(X, S) -> S + length(X#btxdef.txinputs) end,
		0, Txdata),
    Expected = lists:foldl(fun(X, S) -> S + X#btxdef.inputcount end,
		0, Txdata),
	{Sum, Expected}.

output_to_input(#utxop{}=U) -> 
		{Hash, Index} = U#utxop.hash_index,
		#btxin{txhash=Hash,
			   txindex=Index,
			   script=U#utxop.script,
			   seqnum=0};


output_to_input(#us{}=U) -> output_to_input(unspentset:hash(U),
                                            unspentset:index(U),
                                            unspentset:output(U)).

output_to_input(Hash, Index, #boutput{meta = M, ext = E}=O) ->
    Script = bblock:script(O),
    #binput{data = <<Hash:32/binary,
            Index:32/little,
            (lib_parse:int_to_varint(size(Script)))/binary,
            Script/binary,
            0:32/little>>,
            meta = M,
            ext = E}.
