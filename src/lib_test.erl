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
         create_random_address/0,
         inputs_from_outputs/2,
         create_input/0,
         create_input/2,
         create_outputs/1,
         create_output/0,
         create_output/1,
         create_output/3,
         create_output/4,
         create_transaction/2,
         create_transaction/3,
         create_block/0,
	     create_block/2,
	     create_chain/2,
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
		 output_to_unspent/2,
	     random_p2sh_input/1,
	     data/1]).

-include_lib("bitter.hrl").

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
	B.


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
	lists:map(fun(E) ->
				create_input(Txhash, E#btxout.txindex) end, Outputs).

output_to_unspent(Txhash, O) ->
	#utxop{hash_index = {Txhash, O#btxout.txindex},
		   value = O#btxout.value,
		   script = O#btxout.script,
		   address = O#btxout.address,
		   info = O#btxout.info, 
		   color = O#btxout.color,
		   quantity = O#btxout.quantity,
		   height = 0}.

random_p2sh_input(Type) ->
	{Addr, KeyList} = lib_address:generate_p2sh_address(Type),
	O = create_random_output(Addr),
	Txhash = crypto:rand_bytes(32),
	U = output_to_unspent(Txhash, O),
	UnspentDict = lib_kd:add(U),
	I = lib_tx:create_input(U),
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
	create_input(crypto:rand_bytes(32), random:uniform(100)).

create_input(Addr) when is_record(Addr, addr) ->
	I = create_random_input(),
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
	lists:map(fun(X) -> create_output(X) end,
		lists:seq(0,Num-1)).

create_random_output() ->
	create_output(random:uniform(100),
		          random:uniform(10000000),
		lib_address:hash160(create_random_address())).

create_output() -> create_output(0).
create_output(Txindex) ->
	create_output(Txindex, 50000, lib_address:address_to_hash160("15MLJpjve5pjPD5aTyK1aBRZ2aW8Vwcwyx")).
create_output(Txindex, Value, Address) when is_record(Address, addr) ->
	create_output(Txindex, Value, lib_address:hash160(Address));
create_output(Txindex, Value, Address) ->
	#btxout{txindex=Txindex,
		    value=Value,
		    script = lib_tx:create_script(p2pkh, Address),
		    address=Address}.

create_random_output(Address) when is_record(Address, addr) ->
	create_random_output(lib_address:type(Address),
		                 lib_address:hash160(Address)).

create_random_output(p2sh, Address) ->
	create_output(p2sh, random:uniform(100),
		          random:uniform(10000000),
				  Address).

create_output(p2sh, Txindex, Value, Address) ->
	#btxout{txindex=Txindex,
		    value=Value,
		    script = lib_tx:create_script(p2sh, Address),
		    address=Address}.

create_transaction(Inputs, Outputs) ->
	create_transaction(crypto:rand_bytes(32), Inputs, Outputs).
create_transaction(Txhash, Inputs, Outputs) ->
		#btxdef{txhash=Txhash,
		        inputcount=length(Inputs),
				outputcount=length(Outputs),
				txinputs=Inputs,
			    txoutputs=Outputs}.

create_block() -> create_block(crypto:rand_bytes(32), crypto:rand_bytes(32)).
create_block(Hash, PreviousHash) ->
			#bbdef{blockhash=Hash,
		    previoushash=PreviousHash,
		    difficulty=100,
		    e_height=1}.

create_chain(Iterations, NumOutputs) ->
	A = create_block(),
	C = create_outputs(NumOutputs),
	T = create_transaction([create_input()], C),
	NewBlock = A#bbdef{txdata=[T]},
	{Acc, _B} = lists:mapfoldl(fun(_, Prev) ->
		A2 = create_block(),
		[Tdata|_] = Prev#bbdef.txdata,
		B2 = inputs_from_outputs(Tdata#btxdef.txhash,
							     Tdata#btxdef.txoutputs),
		C2 = create_outputs(NumOutputs),
		T2 = create_transaction(B2, C2),
		PrevDifficulty = Prev#bbdef.e_sumdiff,
			N2 = A2#bbdef{txdata=[T2],
						  previoushash=Prev#bbdef.blockhash,
						  e_sumdiff=PrevDifficulty+100},
			{N2, N2}
		end, NewBlock, lists:seq(0, Iterations-1)),
			[NewBlock|Acc].


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



