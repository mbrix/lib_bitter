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

ops(Hex, <<?OP_PUSHDATA1, Rest/binary>>, OPMap, Ops) ->
	<<Datum:8, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [Datum|Ops]);

ops(Hex, <<?OP_PUSHDATA2, Rest/binary>>, OPMap, Ops) ->
	<<Datum:16, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [Datum|Ops]);

ops(Hex, <<?OP_PUSHDATA4, Rest/binary>>, OPMap, Ops) ->
	<<Datum:32, R2/binary>> = Rest,
	ops(Hex, R2, OPMap, [Datum|Ops]);

ops(Hex, <<Op:8, Rest/binary>>, OPMap, Ops) ->
	{Datum, R2} = lookup(Op, OPMap, Rest),
	 ops(Hex, R2, OPMap, [Datum|Ops]).

lookup(Op, OPMap, Rest) ->
	case maps:find(Op, OPMap) of
		{ok, OPCode} -> {OPCode, Rest};
		error ->
			%% Unspecified opcode = OP_RETURN
			{Rest, <<>>}
	end.

%% Evaluate

eval(Tx, LookupFun) when is_record(Tx, btxdef) ->
	eval_inputs(Tx, LookupFun).

eval_inputs(Tx, LookupFun) -> eval_inputs(Tx, Tx#btxdef.txinputs, LookupFun, 1).

eval_inputs(_Tx, [], _LookupFun, _Index) -> true;
eval_inputs(Tx, [I|Inputs], LookupFun, Index) ->
	case eval(LookupFun(I#btxin.txhash, I#btxin.txindex),
		 I#btxin.script,
		 Index, Tx) of
		true -> eval_inputs(Tx, Inputs, LookupFun, Index+1);
		false -> false
	end.

check([]) -> false;
check([0]) -> false;
check(_) -> true.

%% only works if checksig not defined, mostly for debugging
eval(Script) ->
	eval(Script, [], [], undefined, 0, undefined, []).

eval(ScriptSig, ScriptPubKey, Index, Tx) ->
	check(eval(<<ScriptSig/binary, ScriptPubKey/binary>>, [], [], Tx, Index, ScriptPubKey, [])).

eval(<<>>, _Stack, _Alt, _Tx, _Index, _SubScriptBin, IfFlag) when length(IfFlag) > 0 ->
	%% Uncompleted IF definition
	false;
eval(<<>>, Stack, _Alt, _Tx, _Index, _SubScriptBin, _) -> Stack;

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

eval(<<?OP_NOP, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

%% What does this do?
eval(<<?OP_VER, _/binary>>, _, _, _, _, _, _) ->
	false;

eval(<<?OP_VERIF, _/binary>>, _, _, _, _, _, _) ->
	false;

eval(<<?OP_VERNOTIF, _/binary>>, _, _, _, _, _, _) ->
	false;


eval(<<?OP_VERIFY, _/binary>>, [0|_], _, _, _, _, _) ->
	false;

eval(<<?OP_VERIFY, _/binary>>, [], _, _, _, _, _) ->
	false;

eval(<<?OP_VERIFY, Rest/binary>>, [1|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RETURN, _/binary>>, [], _, _, _, _, _) ->
	false;


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
			false
	end;

eval(<<?OP_TUCK, Rest/binary>>, [A,B|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [A,B,A|S], AltStack, Tx, Index, SubScriptBin, IfFlag);


eval(<<?OP_CAT, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_SUBSTR, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_LEFT, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_RIGHT, _/binary>>, _, _, _, _, _, _) -> false;

eval(<<?OP_SIZE, Rest/binary>>, [A|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [size(A)|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_INVERT, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_AND, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_OR, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_XOR, _/binary>>, _, _, _, _, _, _) -> false;


eval(<<?OP_EQUAL, Rest/binary>>, [Datum,Datum|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_EQUAL, Rest/binary>>, [_Datum,_Datum2|_]=S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [0|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, [{0,_,_}|_]=IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED, _/binary>>, _, _, _, _, _, _) -> false;

eval(<<?OP_RESERVED1, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, [{0,_,_}|_]=IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED1, _/binary>>, _, _, _, _, _, _) -> false;

eval(<<?OP_RESERVED2, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, [{0,_,_}|_]=IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_RESERVED2, _/binary>>, _, _, _, _, _, _) -> false;

eval(<<?OP_1ADD, Rest/binary>>, [D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [D+1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_1SUB, Rest/binary>>, [D|S], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [D-1|S], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_MUL, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_2MUL, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_DIV, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_2DIV, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_MOD, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_LSHIFT, _/binary>>, _, _, _, _, _, _) -> false;
eval(<<?OP_RSHIFT, _/binary>>, _, _, _, _, _, _) -> false;

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
eval(<<?OP_NUMEQUALVERIFY, _/binary>>, [_,_|_], _, _, _, _, _) -> false;

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

eval(<<?OP_CODESEPARATOR, Rest/binary>>, S, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, S, AltStack, Tx, Index, SubScriptBin, IfFlag);


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
	false;
eval(<<?OP_ELSE, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, [{1, _OldStack, _OldAlt}|IfFlag]) ->
	%% Last If block executed, push O stack
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, [{0, Stack, AltStack}|IfFlag]);

eval(<<?OP_ELSE, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, [{0, OldStack, OldAlt}|IfFlag]) ->
	%% Last block did not execute, but stack altered.
	%% Pop Oldstack and add new stack
	eval(Rest, OldStack, OldAlt, Tx, Index, SubScriptBin, [{1, Stack, AltStack}|IfFlag]);

eval(<<?OP_ENDIF, _Rest/binary>>, _Stack, _AltStack, _Tx, _Index, _SubScriptBin, []) ->
	%% Endif without IF block
	false;

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

eval(<<?OP_PUSHDATA1, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	<<Datum:8, R2/binary>> = Rest,
	eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_PUSHDATA2, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	<<Datum:16, R2/binary>> = Rest,
	eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_PUSHDATA4, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	<<Datum:32, R2/binary>> = Rest,
	eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_DUP, Rest/binary>>, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [Datum,Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_HASH160, Rest/binary>>, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, [crypto:hash(ripemd160,
							crypto:hash(sha256, Datum))|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_EQUALVERIFY, Rest/binary>>, [Datum,Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	eval(Rest, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag);

eval(<<?OP_EQUALVERIFY, _Rest/binary>>, [_Datum,_Datum2|_Stack], _AltStack, _Tx, _Index, _SubScriptBin, _IfFlag) ->
	false;

eval(<<?OP_CHECKSIGVERIFY, Rest/binary>>, [Pubkey,Sig|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	case eval(<<?OP_CHECKSIG>>, [Pubkey,Sig], AltStack, Tx, Index, SubScriptBin, IfFlag) of
		[1] -> eval(Rest, [1|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);
		[0] -> false
	end;

eval(<<?OP_CHECKSIG:8, Rest/binary>>, [Pubkey,Sig|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	%% Look through ScriptPubKey original for codeseparator
	SubScript = code_sep(SubScriptBin),
	SigSize = size(Sig)*8-8,
	<<SigStr:(SigSize)/bitstring, SigHashType:8>> = Sig,
	Tx2 = prepare_tx(Tx, Index, SubScript),
	SerializedTx = iolist_to_binary([lib_tx:serialize_btxdef(Tx2), <<SigHashType:32/little>>]),
	case libsecp256k1:ecdsa_verify(lib_tx:hash_tx(SerializedTx), SigStr, Pubkey) of
		ok -> eval(Rest, [1|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);
		error -> eval(Rest, [0|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag)
	end;

eval(<<Op:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
	{Datum, R2} = lookup(Op, ?OP_MAP, Rest),
	if erlang:bit_size(Datum) > 8 ->
		   eval(R2, [Datum|Stack], AltStack, Tx, Index, SubScriptBin, IfFlag);
	   true -> false %% NOT_IMPLEMENTED OP_CODE
	end.


%eval(<<?OP_CHECKMULTISIG:8, Rest/binary>>, Stack, AltStack, Tx, Index, SubScriptBin, IfFlag) ->
%	{Keys, Stack2} = pop_elements(Stack),
%	{Sigs, Stack3} = pop_elements(Stack2),
%	[_|Stack4] = Stack3, %% Pop another item out because of Bug in bitcoind
%	ok.

%pop_elements([Num|Stack]) -> pop_elements(Num, Stack, []).
%
%pop_elements(0, Stack, NewStack) -> {lists:reverse(NewStack), Stack};
%pop_elements(Num, [E|Stack], NewStack) ->
%	pop_elements(Num-1, Stack, [E|NewStack]).
%
roll(L, X) -> roll(L,X,[]).
roll([], _, _) -> error;
roll([H|Stack], 0, HList) -> {ok, [H|(lists:reverse(HList) ++ Stack)]};
roll([H|T],X,HList) ->
	roll(T,X-1,[H|HList]).

code_sep(SubScriptBin) -> code_sep(SubScriptBin, SubScriptBin).

code_sep(<<>>, NewSubScript) -> NewSubScript;
code_sep(<<?OP_CODESEPARATOR, Rest/binary>>, _OldSubScript) -> code_sep(Rest, Rest);
code_sep(<<_:8, Rest/binary>>, NewSubScript) -> code_sep(Rest, NewSubScript).

prepare_tx(Tx, Index, SubScript) ->
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txinputs = lists:map(fun(I) when I =:= InterestingInput ->
										   InterestingInput#btxin{script = SubScript};
									  (_) -> <<>>
								   end, Tx#btxdef.txinputs)}.
