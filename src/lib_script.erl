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

-module(lib_script).
-author('mbranton@emberfinancial.com').
-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ops/1,
		 ops/2,
		 eval/1,
		 eval/2,
		 eval/4,
		 revmap/0,
		 build/1]).

%% Util
revmap() -> revmap(?OP_MAP, #{}).

revmap(Map, NewMap) ->
	maps:fold(fun(K,V, AccIn) -> maps:put(V,K,AccIn) end, NewMap, Map).

build(ScriptList) ->
	RevMap = revmap(),
	lists:map(fun({hex, E}) ->
									   add_pushdata(hex:hexstr_to_bin(E));
								  (E) ->
								  	   case maps:find(E, RevMap) of
								  	   	   {ok, Int} -> <<Int:8>>;
								  	   	   error -> add_pushdata(E)
					  end end, ScriptList).

add_pushdata(Bin) -> <<(size(Bin)):8, Bin/bitstring>>.

%% Print

ops(hex, Bin) -> ops(true, Bin, ?OP_MAP, []).

ops(Bin) -> ops(false, Bin, ?OP_MAP, []).
ops(_, <<>>, _OPMap, Ops) -> lists:reverse(Ops);

%% PUSHData Short
ops(true, <<Op:8, Rest/binary>>, OPMap, Ops) when Op > 0, Op < 76 ->
	Size = Op*8,
	<<PushData:Size/bitstring, R2/binary>> = Rest,
	ops(true, R2, OPMap, [{hex, hex:bin_to_hexstr(PushData)}|Ops]);

ops(Hex, <<Op:8, Rest/binary>>, OPMap, Ops) when Op > 0, Op < 76 ->
	Size = Op*8,
	<<PushData:Size/bitstring, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [PushData|Ops]);

ops(Hex, <<Op:8, _Rest/binary>>, OPMap, Ops) when Op > 185, Op < 256 ->
	ops(Hex, <<>>, OPMap, [op_return|Ops]);

ops(Hex, <<?OP_PUSHDATA1, Size:8, Rest/binary>>, OPMap, Ops) ->
	BSize = Size*8,
	<<Datum:BSize/bitstring, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [Datum|Ops]);

ops(Hex, <<?OP_PUSHDATA2, Size:16, Rest/binary>>, OPMap, Ops) ->
	BSize = Size*8,
	<<Datum:BSize/bitstring, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [Datum|Ops]);

ops(Hex, <<?OP_PUSHDATA4, Size:32, Rest/binary>>, OPMap, Ops) ->
	BSize = Size*8,
	<<Datum:BSize/bitstring, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [Datum|Ops]);

ops(Hex, <<Op:8, Rest/binary>>, OPMap, Ops) ->
	{Datum, R2} = lookup(Op, OPMap, Rest),
	 ops(Hex, R2, OPMap, [Datum|Ops]).

lookup(Op, OPMap, Rest) ->
	case maps:find(Op, OPMap) of
		{ok, OPCode} -> {OPCode, Rest};
		error ->
			?debugFmt("Could not look up opcode ~p ~n", [Op]),
			%% Unspecified opcode = OP_RETURN
			{Rest, <<>>}
	end.

%% Evaluate

eval(Tx, LookupFun) when is_record(Tx, btxdef) ->
	eval_inputs(Tx, LookupFun).

eval_inputs(Tx, LookupFun) -> eval_inputs(Tx, Tx#btxdef.txinputs, LookupFun, 0).

eval_inputs(_Tx, [], _LookupFun, _Index) -> true;
eval_inputs(Tx, [I|Inputs], LookupFun, Index) ->
	case eval(I#btxin.script,
			  LookupFun(I#btxin.txhash, I#btxin.txindex), Index, Tx) of
		true -> eval_inputs(Tx, Inputs, LookupFun, Index+1);
		false -> false
	end.

check([]) -> false;
check([0]) -> false;
check(<<>>) -> false;
check(false) -> false;
check(_) -> true.

%% only works if checksig not defined, mostly for debugging
eval(Script) ->
	eval(Script, [], [], undefined, 0, undefined, []).

eval(ScriptSig, ScriptPubKey, Index, Tx) ->
	%% Lets forget the alt stack for a second
	{StartStack, StartAlt, SubScriptBin} = eval(ScriptSig, [], [], Tx, Index, ScriptPubKey, []),
	{EndStack, _EndAlt, _EndSubScriptBin} = eval(ScriptPubKey, StartStack, StartAlt,
												 Tx, Index, SubScriptBin, []),
	case check(EndStack) of
		true -> true;
		false ->
			?debugFmt("ScriptSig: ~p~n PubKey: ~p~n Index: ~p~n Tx: ~p ~n",
					  [ScriptSig, ScriptPubKey, Index, lib_tx:serialize(Tx)]),
			false
	end.

eval(<<>>, _Stack, _Alt, _Tx, _Index, SubScriptBin, IfFlag) when length(IfFlag) > 0 ->
	%% Uncompleted IF definition
	{false, false, SubScriptBin};

eval(<<>>, Stack, Alt, _Tx, _Index, SubScriptBin, _) ->
	{Stack,Alt,SubScriptBin};

eval(<<?OP_0, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_1NEGATE, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [-1|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

%% OP_? Ops
eval(<<Op:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) when Op > 80, Op < 97 ->
	eval(Rest, [Op-80|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

%% NOPS
eval(<<Op:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) when Op > 175, Op < 186 ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

%% OP_UNKNOWNs

eval(<<Op:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) when Op > 185, Op < 256 ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NOP, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

%% What does this do?
eval(<<?OP_VER, _/binary>>, _, _, _, _, _, _) ->
	{false, false, <<>>};

eval(<<?OP_VERIF, _/binary>>, _, _, _, _, _, _) ->
	{false, false, <<>>};

eval(<<?OP_VERNOTIF, _/binary>>, _, _, _, _, _, _) ->
	{false, false, <<>>};


eval(<<?OP_VERIFY, _/binary>>, [0|_], _, _, _, _, _) ->
	{false, false, <<>>};

eval(<<?OP_VERIFY, _/binary>>, [], _, _, _, _, _) ->
	{false, false, <<>>};

eval(<<?OP_VERIFY, Rest/binary>>, [1|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RETURN, _/binary>>, [], _, _, _, _, _) ->
	{false, false, <<>>};


eval(<<?OP_TOALTSTACK, Rest/binary>>, [X|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, [X|AltStack], Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_FROMALTSTACK, Rest/binary>>, Stack, [X|AltStack], Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [X|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);


eval(<<?OP_DROP, Rest/binary>>, [_|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_2DROP, Rest/binary>>, [_,_|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_DUP, Rest/binary>>, [A|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A,A|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_2DUP, Rest/binary>>, [A,B|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A,B,A,B|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_3DUP, Rest/binary>>, [A,B,C|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A,B,C,A,B,C|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_OVER, Rest/binary>>, [_,B|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [B|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_2OVER, Rest/binary>>, [_,_,C,D|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [C,D|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_2ROT, Rest/binary>>, [A,B,C,D,E,F|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [E,F,A,B,C,D|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_SWAP, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [B,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_2SWAP, Rest/binary>>, [A,B,C,D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [C,D,A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_IFDUP, Rest/binary>>, [0|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_IFDUP, Rest/binary>>, [X|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [X|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_DEPTH, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [length(S)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NIP, Rest/binary>>, [A,_|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_PICK, Rest/binary>>, [D|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [lists:nth(D+1,Stack)|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_ROLL, Rest/binary>>, [D|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	case roll(Stack, D) of
		{ok, NewStack} ->
			eval(Rest, NewStack, AltStack, Tx, Index, SubScriptBin, IfFlag);
		error ->
			{false, false, <<>>}
	end;

eval(<<?OP_TUCK, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A,B,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);


eval(<<?OP_CAT, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_SUBSTR, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_LEFT, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_RIGHT, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};

eval(<<?OP_SIZE, Rest/binary>>, [A|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [size(A)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_INVERT, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_AND, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_OR, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_XOR, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};


eval(<<?OP_EQUAL, Rest/binary>>, [Datum,Datum|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_EQUAL, Rest/binary>>, [_Datum,_Datum2|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, [{0,_,_}|_]=IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};

eval(<<?OP_RESERVED1, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, [{0,_,_}|_]=IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED1, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};

eval(<<?OP_RESERVED2, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, [{0,_,_}|_]=IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED2, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};

eval(<<?OP_1ADD, Rest/binary>>, [D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [D+1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_1SUB, Rest/binary>>, [D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [D-1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_MUL, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_2MUL, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_DIV, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_2DIV, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_MOD, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_LSHIFT, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};
eval(<<?OP_RSHIFT, _/binary>>, _, _, _, _, _, _) -> {false, false, <<>>};

eval(<<?OP_ROT, Rest/binary>>, [A,B,C|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [C,A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NEGATE, Rest/binary>>, [D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [D*-1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_ABS, Rest/binary>>, [D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [abs(D)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NOT, Rest/binary>>, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NOT, Rest/binary>>, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NOT, Rest/binary>>, [_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_0NOTEQUAL, Rest/binary>>, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_0NOTEQUAL, Rest/binary>>, [_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_ADD, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A+B|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_SUB, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [B-A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_BOOLAND, Rest/binary>>, [0,0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_BOOLAND, Rest/binary>>, [0,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_BOOLAND, Rest/binary>>, [_,0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_BOOLAND, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_BOOLOR, Rest/binary>>, [0,0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_BOOLOR, Rest/binary>>, [0,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_BOOLOR, Rest/binary>>, [_,0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_BOOLOR, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NUMEQUAL, Rest/binary>>, [A,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_NUMEQUAL, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_NUMEQUALVERIFY, Rest/binary>>, [A,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_NUMEQUALVERIFY, _/binary>>, [_,_|_], _, _, _, _, _) -> {false, false, <<>>};

eval(<<?OP_NUMNOTEQUAL, Rest/binary>>, [A,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_NUMNOTEQUAL, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_LESSTHAN, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A < B -> 
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_LESSTHAN, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_GREATERTHAN, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A > B -> 
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_GREATERTHAN, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_LESSTHANOREQUAL, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A =< B -> 
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_LESSTHANOREQUAL, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_GREATERTHANOREQUAL, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A >= B -> 
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_GREATERTHANOREQUAL, Rest/binary>>, [_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_MIN, Rest/binary>>, [A,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) -> 
	eval(Rest, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_MIN, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A < B -> 
	eval(Rest, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_MIN, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A > B ->
	eval(Rest, [B|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_MAX, Rest/binary>>, [A,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) -> 
	eval(Rest, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_MAX, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A > B -> 
	eval(Rest, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);
eval(<<?OP_MAX, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A < B ->
	eval(Rest, [B|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_WITHIN, Rest/binary>>, [C,B,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) when A >= B, A < C ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_WITHIN, Rest/binary>>, [_,_,_|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RIPEMD160, Rest/binary>>, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(ripemd160, A)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_SHA1, Rest/binary>>, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(sha, A)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_SHA256, Rest/binary>>, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(sha256, A)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_HASH160, Rest/binary>>, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(ripemd160, crypto:hash(sha256, A))|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_HASH256, Rest/binary>>, [A|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(sha256, crypto:hash(sha256, A))|S], AltStack, Tx, Index, SubScriptBin, IfFlag);


%% Should this be in the last executed OP?
eval(<<?OP_CODESEPARATOR, Rest/binary>>, S, AltStack, Tx, Index, _SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, Rest, IfFlag);


%% IF Logic is complicated...
%% If is true continue executing
eval(<<?OP_IF, Rest/binary>>, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	%% Push the stack to the Flags
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, [{1, S, AltStack}|IfFlag]);

eval(<<?OP_IF, Rest/binary>>, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, [{0, S, AltStack}|IfFlag]);

eval(<<?OP_NOTIF, Rest/binary>>, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	%% Push the stack to the Flags
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, [{1, S, AltStack}|IfFlag]);

eval(<<?OP_NOTIF, Rest/binary>>, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, [{0, S, AltStack}|IfFlag]);


eval(<<?OP_ELSE, _Rest/binary>>, _Stack, _AltStack, _Tx, _Index, _SubScriptBin, []) ->
	%% Else with no IF block
	{false, false, <<>>};
eval(<<?OP_ELSE, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, [{1, _OldStack, _OldAlt}|IfFlag]) ->
	%% Last If block executed, push O stack
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, [{0, Stack, AltStack}|IfFlag]);

eval(<<?OP_ELSE, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, [{0, OldStack, OldAlt}|IfFlag]) ->
	%% Last block did not execute, but stack altered.
	%% Pop Oldstack and add new stack
	eval(Rest, OldStack, OldAlt, Tx, Index, SubScriptBin, [{1, Stack, AltStack}|IfFlag]);

eval(<<?OP_ENDIF, _Rest/binary>>, _Stack, _AltStack, _Tx, _Index, _SubScriptBin, []) ->
	%% Endif without IF block
	{false, false, <<>>};

eval(<<?OP_ENDIF, Rest/binary>>, _Stack, _AltStack, Tx, Index, SubScriptBin, [{0,OldStack,OldAlt}|IfFlag]) ->
	%% Last block did not execute but stack altered
	%% Pop Oldstack
	eval(Rest, OldStack, OldAlt, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_ENDIF, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, [_|IfFlag]) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

	
eval(<<Op:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) when Op > 0, Op < 76 ->
	Size = Op*8,
	<<PushData:Size/bitstring, R2/binary>> = Rest,
	eval(R2, [PushData|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_PUSHDATA1, Size:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	BSize = if Size > size(Rest) -> size(Rest)*8;
			   true -> Size*8
			end,
	<<Datum:BSize/bitstring, R2/binary>> = Rest,
	eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_PUSHDATA2, Size:16, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	BSize = if Size > size(Rest) -> size(Rest)*8;
			   true -> Size*8
			end,
	<<Datum:BSize/bitstring, R2/binary>> = Rest,
	eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_PUSHDATA4, Size:32, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	BSize = if Size > size(Rest) -> size(Rest)*8;
			   true -> Size*8
			end,
	<<Datum:BSize/bitstring, R2/binary>> = Rest,
	eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_DUP, Rest/binary>>, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [Datum,Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_HASH160, Rest/binary>>, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(ripemd160,
							crypto:hash(sha256, Datum))|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_EQUALVERIFY, Rest/binary>>, [Datum,Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_EQUALVERIFY, _Rest/binary>>, [_Datum,_Datum2|_Stack], _AltStack, _Tx, _Index, _SubScriptBin, _IfFlag) ->
	{false, false, <<>>};

eval(<<?OP_CHECKSIGVERIFY, Rest/binary>>, [Pubkey,Sig|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	case eval(<<?OP_CHECKSIG>>, [Pubkey,Sig], AltStack, Tx, Index, SubScriptBin, IfFlag) of
		[1] -> eval(Rest, [1|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);
		[0] -> false
	end;

eval(<<?OP_CHECKSIG:8, Rest/binary>>, [Pubkey,Sig|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	%?debugFmt("SubscriptBin: ~p~n", [SubScriptBin]),
	SigSize = size(Sig)*8-8,
	<<SigStr:(SigSize)/bitstring, SigHashType:8>> = Sig,
	%?debugFmt("Sighashtype: ~p~n", [SigHashType]),
	<<_:(SigSize)/bitstring, _:3, SigType:5>> = Sig,
	Tx2 = prepare_tx(Tx, Index, SubScriptBin, SigType),
	%?debugFmt("Prepared TX ~p~n", [Tx2]),
	Tx3 = anyonecanpay(SigHashType band ?SIGHASH_ANYONECANPAY, Tx2, Index),
	SerializedTx = iolist_to_binary([lib_tx:serialize_btxdef(Tx3), <<SigHashType:32/little>>]),
	%?debugFmt("MSG: ~p~n", [hex:bin_to_hexstr(lib_tx:hash_tx(SerializedTx))]),
	%?debugFmt("SIG: ~p~n", [hex:bin_to_hexstr(SigStr)]),
	%?debugFmt("PUB: ~p~n", [hex:bin_to_hexstr(Pubkey)]),

	case libsecp256k1:ecdsa_verify(lib_tx:hash_tx(SerializedTx), SigStr, Pubkey) of
		ok -> 
	%		?debugFmt("ecdsa verify~n", []),
			eval(Rest, [1|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);
		error -> eval(Rest, [0|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag)
	end;


eval(<<?OP_CHECKMULTISIG:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	{Keys, Stack2} = pop_elements(Stack),
	{Sigs, Stack3} = pop_elements(Stack2),
	[_|Stack4] = Stack3, %% Pop another item out because of Bug in bitcoind
	%% Now every single sig must have a matching key
	eval(Rest, [check_sigs(Sigs, Keys, Tx, Index, SubScriptBin)|Stack4], AltStack, Tx, Index,
		 SubScriptBin, IfFlag); 


eval(<<Op:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	{Datum, R2} = lookup(Op, ?OP_MAP, Rest),
	if erlang:bit_size(Datum) > 8 ->
		   eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);
	   true ->
	   	   ?debugFmt("Unknown OP: ~p~n", [Op]),
		   {false, false, <<>>} %% NOT_IMPLEMENTED OP_CODE
	end.

check_sigs([], _Keys, _Tx, _Index, _SubScriptBin) -> 1;
check_sigs([Sig|Sigs], Keys, Tx, Index, SubScriptBin) ->
	SigSize = size(Sig)*8-8,
	<<SigStr:(SigSize)/bitstring, SigHashType:8>> = Sig,
	<<_:(SigSize)/bitstring, _:3, SigType:5>> = Sig,
	Tx2 = prepare_tx(Tx, Index, SubScriptBin, SigType),
	SerializedTx = iolist_to_binary([lib_tx:serialize_btxdef(Tx2), <<SigHashType:32/little>>]),
	DHash = lib_tx:hash_tx(SerializedTx),
	case match_sig(DHash, SigStr, Keys, []) of
		{ok, KeysLeft} -> check_sigs(Sigs, KeysLeft, Tx, Index, SubScriptBin);
		missing -> 0
	end.

match_sig(_DHash, _SigStr, [], _KeysLeft) -> missing;
match_sig(DHash, SigStr, [K|Keys], KeysLeft) ->
	case libsecp256k1:ecdsa_verify(DHash, SigStr, K) of
		ok -> {ok, Keys ++ KeysLeft};
		error -> match_sig(DHash, SigStr, Keys, [K|KeysLeft]) 
	end.

% Pop this number of elements off stack
pop_elements([Num|Stack]) -> pop_elements(Num, Stack, []).

pop_elements(0, Stack, NewStack) -> {lists:reverse(NewStack), Stack};
pop_elements(Num, [E|Stack], NewStack) ->
	pop_elements(Num-1, Stack, [E|NewStack]).

roll(L, X) -> roll(L,X,[]).
roll([], _, _) -> error;
roll([H|Stack], 0, HList) -> {ok, [H|(lists:reverse(HList) ++ Stack)]};
roll([H|T],X,HList) ->
	roll(T,X-1,[H|HList]).

prepare_tx(Tx, Index, SubScript, ?SIGHASH_ALL) ->
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txinputs = lists:map(fun(I) when I =:= InterestingInput ->
										   InterestingInput#btxin{script = SubScript};
									  (I2) -> I2#btxin{script = <<>>}
								   end, Tx#btxdef.txinputs)};
prepare_tx(Tx, Index, SubScript, ?SIGHASH_OLD) ->
	prepare_tx(Tx, Index, SubScript, ?SIGHASH_ALL);

prepare_tx(Tx, Index, SubScript, ?SIGHASH_NONE) ->
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txoutputs = [],
			  txinputs = lists:map(fun(I) when I =:= InterestingInput ->
				  							   InterestingInput#btxin{script = SubScript};
			  						  (I2) -> I2#btxin{seqnum = 0, script = <<>>}
								   end, Tx#btxdef.txinputs)};

prepare_tx(Tx, Index, SubScript, ?SIGHASH_SINGLE) ->
	SubList = lists:sublist(Tx#btxdef.txoutputs, 1, Index),
	CurrentOutput = lists:nth(Index+1, Tx#btxdef.txoutputs),
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txoutputs = lists:map(fun(O) ->
											O#btxout{value = -1,
													 script = <<>>}
									end, SubList) ++ [CurrentOutput],
	  txinputs = lists:map(fun(I) when I =:= InterestingInput ->
				  							   InterestingInput#btxin{script = SubScript};
			  						  (I2) -> I2#btxin{seqnum = 0, script = <<>>}
								   end, Tx#btxdef.txinputs)};



prepare_tx(Tx, Index, SubScript, X) ->
	?debugFmt("UNHANDLED TX SIGHASHTYPE ~p~n", [X]),
	prepare_tx(Tx, Index, SubScript, ?SIGHASH_ALL).



anyonecanpay(0, Tx2, _) -> Tx2;
anyonecanpay(_, Tx2, Index) ->
	Tx2#btxdef{txinputs = [lists:nth(Index+1, Tx2#btxdef.txinputs)]}.

%% UTILITY

%isValidSignatureEncoding(Sig) when size(Sig) < 9 -> false;
%isValidSignatureEncoding(Sig) when size(Sig) > 73 -> false;
%isValidSignatureEncoding(<<_:8, L:8, _/binary>> = Sig) when (L-3) /= size(Sig) -> false;
%isValidSignatureEncoding(<<_:8, _:8, _:8, RLen:8, _/binary>>=S) when RLen >= size(Sig) -> false;
%isValidSignatureEncoding(<<_:8, L:8, _:8, RLen:8, _/binary>>=S) when 
%
%isValidSignatureEncoding(Sig) {
%    % Format: 0x30 [total-length] 0x02 [R-length] [R] 0x02 [S-length] [S] [sighash]
%    % * total-length: 1-byte length descriptor of everything that follows,
%    %   excluding the sighash byte.
%    % * R-length: 1-byte length descriptor of the R value that follows.
%    % * R: arbitrary-length big-endian encoded R value. It must use the shortest
%    %   possible encoding for a positive integers (which means no null bytes at
%    %   the start, except a single one when the next byte has its highest bit set).
%    % * S-length: 1-byte length descriptor of the S value that follows.
%    % * S: arbitrary-length big-endian encoded S value. The same rules apply.
%    % * sighash: 1-byte value indicating what data is hashed (not part of the DER
%    %   signature)
%
%    // Minimum and maximum size constraints.
%    if (sig.size() < 9) return false;
%    if (sig.size() > 73) return false;
%
%    // A signature is of type 0x30 (compound).
%    if (sig[0] != 0x30) return false;
%
%    // Make sure the length covers the entire signature.
%    if (sig[1] != sig.size() - 3) return false;
%
%    // Extract the length of the R element.
%    unsigned int lenR = sig[3];
%
%    // Make sure the length of the S element is still inside the signature.
%    if (5 + lenR >= sig.size()) return false;
%
%    // Extract the length of the S element.
%    unsigned int lenS = sig[5 + lenR];
%
%    // Verify that the length of the signature matches the sum of the length
%    // of the elements.
%    if ((size_t)(lenR + lenS + 7) != sig.size()) return false;
% 
%    // Check whether the R element is an integer.
%    if (sig[2] != 0x02) return false;
%
%    // Zero-length integers are not allowed for R.
%    if (lenR == 0) return false;
%
%    // Negative numbers are not allowed for R.
%    if (sig[4] & 0x80) return false;
%
%    // Null bytes at the start of R are not allowed, unless R would
%    // otherwise be interpreted as a negative number.
%    if (lenR > 1 && (sig[4] == 0x00) && !(sig[5] & 0x80)) return false;
%
%    // Check whether the S element is an integer.
%    if (sig[lenR + 4] != 0x02) return false;
%
%    // Zero-length integers are not allowed for S.
%    if (lenS == 0) return false;
%
%    // Negative numbers are not allowed for S.
%    if (sig[lenR + 6] & 0x80) return false;
%
%    // Null bytes at the start of S are not allowed, unless S would otherwise be
%    // interpreted as a negative number.
%    if (lenS > 1 && (sig[lenR + 6] == 0x00) && !(sig[lenR + 7] & 0x80)) return false;
%
%    return true;
%}
