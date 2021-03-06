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

-module(lib_color_tests).
-author('mbranton@emberfinancial.com').

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/bitter.hrl").

-export([colored_outputs/1,
	     colored_inputs/1,
	     outputs_to_inputs/1]).

start() ->
	btr_net_params:init(main),
	fakeutxo:start(),  % Real working utxo pool
	fake_colored_block(),
	ok.

stop(_) ->
  fakeutxo:stop(),
  ok.

% Markers

no_marker() ->
	Outputs = create_outputs(5), 
	ColorDef = lib_color:find_marker(Outputs),
	?assertEqual(?Uncolored, ColorDef).

marker_only() ->
	Outputs = [create_marker(create_output(), [1,2,3,4])],
	ColorDef = lib_color:find_marker(Outputs),
	?assertMatch({[1,2,3,4], [], _, []}, ColorDef).

% Building color lists

uncolored_list() ->
	Inputs = colored_inputs(?Uncolored),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	?assertEqual(0, length(ColorList)).

mixed_list() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue),
	%?debugFmt("~p~n", [Inputs]),
	Unspents = inputs_to_unspents(Inputs),
	%?debugFmt("~p~n", [Unspents]),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	%?debugFmt("~p~n", [ColorList]),
	?assertEqual(length(Inputs), length(ColorList)),
	[F|_] = ColorList,
	?assertMatch({red, 1000}, F),
	[F2|_] = lists:reverse(ColorList),
	?assertMatch({blue, 100}, F2).

uncolor_outputs() ->
	Outputs = [create_output(red, 100, 0),
			   create_output(red, 200, 1),
			   create_output(red, 300, 2)],
	NewOutputs = lib_color:uncolor_all(Outputs),
	lists:foldl(fun(X,_) -> 
				?assertMatch({?Uncolored, 0},
					         {lib_color:color(X),
							  lib_color:quantity(X)}), 
				[] end, [], NewOutputs).

issue_color() ->
	Outputs = colored_outputs(red),
	Inputs = outputs_to_inputs(Outputs),
	Unspents = inputs_to_unspents(Inputs),
	[H|_] = Outputs,
	Color = crypto:hash(ripemd160, crypto:hash(sha256, lib_unspent:script(H))),
	RealColor = lib_color:get_issue_color(Inputs, Unspents),
	?assertEqual(Color, RealColor).

single_color_quant() ->
	?assertMatch({?Uncolored, []}, lib_color:get_color_quant([], 10)),
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	{Color, NewColorList} = lib_color:get_color_quant(ColorList, 1000),
	?assertMatch(red, Color),
	[H|_] = NewColorList,
	?assertMatch({red, 1000}, H).

partial_quant() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	{Color, NewColorList} = lib_color:get_color_quant(ColorList, 999),
	?assertMatch(red, Color),
	[H|_] = NewColorList,
	?assertMatch({red, 1}, H).

multi_partial_quant() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	{Color, NewColorList} = lib_color:get_color_quant(ColorList, 1240),
	?assertMatch(red, Color),
	[H|_] = NewColorList,
	?assertMatch({red, 760}, H).

total_fill_quant() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TotalQuant = get_total_quant(ColorList, red), 
	{Color, NewColorList} = lib_color:get_color_quant(ColorList, TotalQuant),
	?assertMatch(red, Color),
	?assertMatch([], NewColorList).

multi_fill_quant() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	FinalList = lists:foldl(fun(_,L) ->
					{_,CList} = lib_color:get_color_quant(L, 1),
						CList
				end,
		ColorList, lists:seq(1,1000)),
	{_, NewColorList} = lib_color:get_color_quant(FinalList, 1),
	[H|_] = NewColorList,
	?assertMatch({red, 999}, H).

multi_color_quant() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TotalRed = get_total_quant(ColorList, red),
	TotalBlue = get_total_quant(ColorList, blue),
	{_, CL2} = lib_color:get_color_quant(ColorList, TotalRed),
	{_, CL3} = lib_color:get_color_quant(CL2, TotalBlue),
	?assertMatch([], CL3),
	{Color,CL4} = lib_color:get_color_quant(CL2, 99),
	[H|_] = CL4,
	?assertEqual(blue, Color),
	?assertMatch({blue, 401}, H).

%% This functionality changed, if we try to request a quantity greater than the
%% total available quant of a specific color instead of an overflow we should uncolor everything.
mixed_colors() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TotalRed = get_total_quant(ColorList, red),
	{Color, CL2} = lib_color:get_color_quant(ColorList, TotalRed+2000),
	?assertEqual(?Uncolored, Color),
	?assertEqual([], CL2).
	%%[H|_]= CL2,
	%%?debugFmt("~p~n", [CL2]),
	%%?assertEqual({blue, 8500}, H). % Or should list be ?Uncolored?

uncolored_overflow() ->
	Inputs = colored_inputs(red) ++ colored_inputs(?Uncolored) ++ colored_inputs(blue),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	%?debugFmt("XXX: ~p~n", [ColorList]),
	TotalRed = get_total_quant(ColorList, red),
	{Color, _CL2} = lib_color:get_color_quant(ColorList, TotalRed),
	?assertEqual(red, Color).

zero_color_quant() ->
	ColorList = [{red, 1000}],
	{Color, CL} = lib_color:get_color_quant(ColorList, 0),
	?assertEqual(?Uncolored, Color),
	?assertMatch([{red, 1000}], CL).

%%% Issuance Tests
simple_issuance() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents), %won't be red
	QList = [100],
	O = create_output(),
	{RColorList, IssuedOutputs} = lib_color:do_issuance(IC, QList, [O]),
	%?debugFmt("~p~n", [IssuedOutputs]),
	[H|_] = IssuedOutputs,
	?assertEqual(1, length(IssuedOutputs)),
	?assertEqual([], RColorList),
	?assertEqual(IC, lib_color:color(H)),
	?assertEqual(100, lib_color:quantity(H)). 

multiple_issuance() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents), %won't be red
	QList = [100,1000,10000,99999],
	Outputs = [create_output(), create_output(?Uncolored, 0, 1), create_output(?Uncolored, 0, 2)],
	{RColorList, IssuedOutputs} = lib_color:do_issuance(IC, QList, Outputs),
	[H|T] = IssuedOutputs,
	[H2|T2] = T,
	[H3|_] = T2,
	?assertEqual(3, length(IssuedOutputs)),
	?assertEqual([99999], RColorList),
	?assertEqual(100, lib_color:quantity(H)),
	?assertEqual(1000, lib_color:quantity(H2)),
	?assertEqual(10000, lib_color:quantity(H3)). 

uncolored_issuance() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents), %won't be red
	QList = [99999999],
	Outputs = [create_output(), create_output(blue, 0, 1)],
	{_, IssuedOutputs} = lib_color:do_issuance(IC, QList, Outputs),
	[_|[T]] = IssuedOutputs,
	?assertEqual(0, lib_color:quantity(T)), 
	?assertEqual(?Uncolored, lib_color:color(T)). 

empty_marker_issue() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents), %won't be red
	QList = [0],
	Outputs = [create_output(red, 10000, 0), create_output(blue, 100, 1)],
	{_, IssuedOutputs} = lib_color:do_issuance(IC, QList, Outputs),
	[H|_T] = IssuedOutputs,
	?assertEqual(0, lib_color:quantity(H)), 
	?assertEqual(?Uncolored, lib_color:color(H)). 

%% Transfer tests

one_transfer() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	CL = lib_color:build_color_list(Inputs, Unspents),
	QList = [100],
	O = create_output(),
	TransferOutputs = lib_color:do_transfers(CL, QList, [O]),
	%?debugFmt("~p~n", [IssuedOutputs]),
	[H|_] = TransferOutputs,
	?assertEqual(1, length(TransferOutputs)),
	?assertEqual(red, lib_color:color(H)), 
	?assertEqual(100, lib_color:quantity(H)). 

multiple_transfers() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue) ++ colored_inputs(?Uncolored),
	Unspents = inputs_to_unspents(Inputs),
	CL = lib_color:build_color_list(Inputs, Unspents),
	TotalRed = get_total_quant(CL, red),
	TotalBlue = get_total_quant(CL, blue),
	%TotalUncolored = get_total_quant(CL, ?Uncolored),
	QList = [TotalRed-100, 100, TotalBlue-100, 100, 100],
	O = [create_output(),
		 create_output(?Uncolored, 0, 1),
		 create_output(?Uncolored, 0, 2),
		 create_output(?Uncolored, 0, 3),
		 create_output(?Uncolored, 0, 4),
		 create_output(?Uncolored, 0, 5)],
	TransferOutputs = lib_color:do_transfers(CL, QList, O),
	%?debugFmt("~p~n", [IssuedOutputs]),
	[H|T] = TransferOutputs,
	?assertEqual(6, length(TransferOutputs)),
	?assertEqual(red, lib_color:color(H)), 
	?assertEqual(TotalRed-100, lib_color:quantity(H)), 
	[H2|T2] = T,
	?assertEqual(red, lib_color:color(H2)), 
	?assertEqual(100, lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(blue, lib_color:color(H3)),
	?assertEqual(TotalBlue-100, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(blue, lib_color:color(H4)), 
	?assertEqual(100, lib_color:quantity(H4)), 
	[H5|T5] = T4,
	?assertEqual(?Uncolored, lib_color:color(H5)), 
	?assertEqual(0, lib_color:quantity(H5)), 
	[H6|_T6] = T5,
	?assertEqual(?Uncolored, lib_color:color(H6)), 
	?assertEqual(0, lib_color:quantity(H6)). 

multiple_boundary_transfers() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue) ++ colored_inputs(?Uncolored),
	Unspents = inputs_to_unspents(Inputs),
	CL = lib_color:build_color_list(Inputs, Unspents),
	TotalRed = get_total_quant(CL, red),
	TotalBlue = get_total_quant(CL, blue),
	%TotalUncolored = get_total_quant(CL, ?Uncolored),
	QList = [1, 1000, TotalRed-1001, TotalBlue+1],
	O = [create_output(),
		 create_output(?Uncolored, 0, 1),
		 create_output(?Uncolored, 0, 2),
		 create_output(?Uncolored, 0, 3),
		 create_output(?Uncolored, 0, 4)],
	TransferOutputs = lib_color:do_transfers(CL, QList, O),
	%?debugFmt("~p~n", [IssuedOutputs]),
	[H|T] = TransferOutputs,
	?assertEqual(5, length(TransferOutputs)),
	?assertEqual(red, lib_color:color(H)),
	?assertEqual(1, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(red, lib_color:color(H2)), 
	?assertEqual(1000, lib_color:quantity(H2)), 
	[H3|T3] = T2,
	?assertEqual(red, lib_color:color(H3)), 
	?assertEqual(TotalRed-1001, lib_color:quantity(H3)),
	[H4|T4] = T3,
	?assertEqual(?Uncolored, lib_color:color(H4)), 
	?assertEqual(0, lib_color:quantity(H4)), 
	[H5|_] = T4,
	?assertEqual(?Uncolored, lib_color:color(H5)),
	?assertEqual(0, lib_color:quantity(H5)). 

full_color_issuance_only() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents),
	QList = [100,100,100,100],
	MOutput = create_output(?Uncolored, 0, 4),
	Marker = create_marker(MOutput, QList),
	O = [create_output(),
		 create_output(?Uncolored, 0, 1),
		 create_output(?Uncolored, 0, 2),
		 create_output(?Uncolored, 0, 3),
		 Marker],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	%?debugFmt("~p~n", [IssuedOutputs]),
	[H|T] = TransferOutputs,
	?assertEqual(5, length(TransferOutputs)),
	?assertEqual(IC, lib_color:color(H)), 
	?assertEqual(100, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(IC, lib_color:color(H2)), 
	?assertEqual(100, lib_color:quantity(H2)), 
	[H3|T3] = T2,
	?assertEqual(IC, lib_color:color(H3)), 
	?assertEqual(100, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(IC, lib_color:color(H4)), 
	?assertEqual(100, lib_color:quantity(H4)), 
	[H5|_] = T4,
	?assertEqual(?Uncolored, lib_color:color(H5)), 
	?assertEqual(0, lib_color:quantity(H5)). 

full_color_transfer() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	QList = [1000, 100],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [Marker,
		 create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	%?debugFmt("~p~n", [IssuedOutputs]),
	[H|T] = TransferOutputs,
	?assertEqual(3, length(TransferOutputs)),
	?assertEqual(?Uncolored, lib_color:color(H)), 
	?assertEqual(0, lib_color:quantity(H)), 
	[H2|T2] = T,
	?assertEqual(red, lib_color:color(H2)), 
	?assertEqual(1000, lib_color:quantity(H2)), 
	[H3|_T3] = T2,
	?assertEqual(red, lib_color:color(H3)), 
	?assertEqual(100, lib_color:quantity(H3)). 

multi_color_transfer_issue() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents),
	QList = [100000, 1000, 2901, 100, 1000000],
	MOutput = create_output(?Uncolored, 0, 2),
	Marker = create_marker(MOutput, QList),
	O = [create_output(),
			create_output(?Uncolored, 0, 1),
			Marker,
			create_output(?Uncolored, 0, 3),
			create_output(?Uncolored, 0, 4),
			create_output(?Uncolored, 0, 5),
			create_output(?Uncolored, 0, 6)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(7, length(TransferOutputs)),
	?assertEqual(IC, lib_color:color(H)),
	?assertEqual(100000, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(IC, lib_color:color(H2)),
	?assertEqual(1000, lib_color:quantity(H2)), 
	[H3|T3] = T2,
	?assertEqual(?Uncolored, lib_color:color(H3)),  %% Marker
	?assertEqual(0, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(red, lib_color:color(H4)), 
	?assertEqual(2901,  lib_color:quantity(H4)), 
	[H5|T5] = T4,
	?assertEqual(blue, lib_color:color(H5)), 
	?assertEqual(100, lib_color:quantity(H5)), 
	[H6|T6] = T5,
	?assertEqual(?Uncolored, lib_color:color(H6)), 
	?assertEqual(0, lib_color:quantity(H6)), 
	[H7|_T7] = T6,
	?assertEqual(?Uncolored, lib_color:color(H7)), 
	?assertEqual(0, lib_color:quantity(H7)). 

rainbow_colors() ->
	Inputs = colored_inputs(red) ++
		     colored_inputs(blue) ++
			 colored_inputs(red) ++
			 colored_inputs(purple) ++
			 colored_inputs(black),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TRed = 2901,
	TBlue = get_total_quant(ColorList, blue),
	TPurple = get_total_quant(ColorList, purple),
	TBlack = get_total_quant(ColorList, black),
	QList = [TRed, TBlue, TRed, TPurple, TBlack],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [Marker,
			create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			create_output(?Uncolored, 0, 3),
			create_output(?Uncolored, 0, 4),
			create_output(?Uncolored, 0, 5),
			create_output(?Uncolored, 0, 6),
			create_output(?Uncolored, 0, 7)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(8, length(TransferOutputs)),
	?assertEqual(?Uncolored, lib_color:color(H)), % Marker
	?assertEqual(0, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(red, lib_color:color(H2)),
	?assertEqual(TRed,lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(blue, lib_color:color(H3)),
	?assertEqual(TBlue, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(red, lib_color:color(H4)), 
	?assertEqual(TRed, lib_color:quantity(H4)), 
	[H5|T5] = T4,
	?assertEqual(purple, lib_color:color(H5)), 
	?assertEqual(TPurple, lib_color:quantity(H5)), 
	[H6|T6] = T5,
	?assertEqual(black, lib_color:color(H6)), 
	?assertEqual(TBlack, lib_color:quantity(H6)), 
	[H7|T7] = T6,
	?assertEqual(?Uncolored, lib_color:color(H7)), 
	?assertEqual(0, lib_color:quantity(H7)), 
	[H8|_T8] = T7,
	?assertEqual(?Uncolored, lib_color:color(H8)), 
	?assertEqual(0, lib_color:quantity(H8)). 

color_quantity() ->
	Unspents = colored_outputs(red),
	WalletUnspents = unspents_to_dict(Unspents),
	ColoredUnspents = lib_transact:get_colored_unspents(dict, red, WalletUnspents),
	?assertEqual(dict:size(WalletUnspents), dict:size(ColoredUnspents)),
	Quant = lib_transact:get_quantity_unspents(dict, red, WalletUnspents),
	?assertEqual(2901, Quant).

%%% TEST related to generating color transactions
%%% from dict of Unspents

color_aggregate() ->
	?assertEqual([], lib_color:color_aggregate([])),
	P1 = lib_transact:payee("3GwgaiTpTqnRXB2aKyYjPpUb5EAZC54XHM",
                            "3GwgaiTpTqnRXB2aKyYjPpUb5EAZC54XHM",
                            <<1>>, 100),
	P2 = P1#payee{color = <<2>>},
	P3 = P1#payee{color = <<3>>},
	P4 = P1#payee{color = <<4>>},
	P5 = P1#payee{color = ?Uncolored},
	P6 = P1#payee{color = <<4>>},
  
	PayeeList = [P1,P2,P3,P4,P5,P6],
	[A, B, C, D, E, D2] = PayeeList,
	?assertEqual([D, D2, C, B, A, E], lib_color:color_aggregate(PayeeList)).

marker_reverse() ->
	A = lib_color:create_marker_output([23000]),
	B = lib_parse:parse_script(A#btxout.script),
	?assertEqual({openassets, {[23000], <<>>}}, B).

% Value in marker exceed maximum allowed leb128 quant
marker_toobig() ->
    A = lib_color:create_marker_output([100, 10, 9223372036854775808, 1000]),
    B = lib_parse:parse_script(A#btxout.script),
	?assertMatch({op_return, _}, B).

% Interleave colored and ?Uncolored outputs
% I'm duplicating inputs here, wouldn't normally happen
interleave() ->
	Inputs = colored_inputs(red) ++
		     colored_inputs(?Uncolored) ++
			 colored_inputs(red) ++
			 colored_inputs(?Uncolored) ++
			 colored_inputs(black),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TRed = 2901,
	TBlack = get_total_quant(ColorList, black),
	QList = [TRed, 0, TRed, 0, TBlack],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [Marker,
			create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			create_output(?Uncolored, 0, 3),
			create_output(?Uncolored, 0, 4),
			create_output(?Uncolored, 0, 5)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(6, length(TransferOutputs)),
	?assertEqual(?Uncolored, lib_color:color(H)), % Marker
	?assertEqual(0, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(red, lib_color:color(H2)),
	?assertEqual(TRed,lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(?Uncolored, lib_color:color(H3)),
	?assertEqual(0, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(red, lib_color:color(H4)), 
	?assertEqual(TRed, lib_color:quantity(H4)), 
	[H5|T5] = T4,
	?assertEqual(?Uncolored, lib_color:color(H5)), 
	?assertEqual(0, lib_color:quantity(H5)), 
	[H6|_T6] = T5,
	?assertEqual(black, lib_color:color(H6)), 
	?assertEqual(TBlack, lib_color:quantity(H6)). 

partial_interleave() ->
	Inputs = colored_inputs(red) ++
		     colored_inputs(?Uncolored) ++
			 colored_inputs(red) ++
			 colored_inputs(?Uncolored) ++
			 colored_inputs(black),
	Unspents = inputs_to_unspents(Inputs),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TRed = 2901,
	TBlack = get_total_quant(ColorList, black),
	QList = [TRed*2, 0, 0, TBlack],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [Marker,
			create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			create_output(?Uncolored, 0, 3),
			create_output(?Uncolored, 0, 4)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(5, length(TransferOutputs)),
	?assertEqual(?Uncolored, lib_color:color(H)), % Marker
	?assertEqual(0, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(red, lib_color:color(H2)),
	?assertEqual(TRed*2,lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(?Uncolored, lib_color:color(H3)),
	?assertEqual(0, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(?Uncolored, lib_color:color(H4)), 
	?assertEqual(0, lib_color:quantity(H4)), 
	[H5|_T5] = T4,
	?assertEqual(black, lib_color:color(H5)), 
	?assertEqual(TBlack, lib_color:quantity(H5)). 

uncolored_issue_transfer() ->
	Inputs = colored_inputs(?Uncolored) ++ colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents),
	QList = [100000, 10000, 2900, 1],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3),
			create_output(?Uncolored, 0, 4)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(5, length(TransferOutputs)),
	?assertEqual(IC, lib_color:color(H)),
	?assertEqual(100000, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(IC, lib_color:color(H2)),
	?assertEqual(10000, lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(?Uncolored, lib_color:color(H3)), %% Marker
	?assertEqual(0, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(red, lib_color:color(H4)), 
	?assertEqual(2900,  lib_color:quantity(H4)), 
	[H5|_T5] = T4,
	?assertEqual(red, lib_color:color(H5)), 
	?assertEqual(1, lib_color:quantity(H5)).

colored_issue_transfer() ->
	Inputs = colored_inputs(red) ++ colored_inputs(blue),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents),
	ColorList = lib_color:build_color_list(Inputs, Unspents),
	TRed = get_total_quant(ColorList, red),
	TBlue = get_total_quant(ColorList, blue),
	QList = [100000, 100, TRed, TBlue],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3),
			create_output(?Uncolored, 0, 4)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(5, length(TransferOutputs)),
	?assertEqual(IC, lib_color:color(H)),
	?assertEqual(100000, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(IC, lib_color:color(H2)),
	?assertEqual(100, lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(?Uncolored, lib_color:color(H3)), %% Marker
	?assertEqual(0, lib_color:quantity(H3)), 
	[H4|T4] = T3,
	?assertEqual(red, lib_color:color(H4)), 
	?assertEqual(TRed,  lib_color:quantity(H4)), 
	[H5|_T5] = T4,
	?assertEqual(blue, lib_color:color(H5)), 
	?assertEqual(TBlue, lib_color:quantity(H5)). 


% Marker tries to color outputs that don't exist
% Should discard overflow
marker_overrun() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents),
	QList = [100000, 10000, 2800, 101],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(4, length(TransferOutputs)),
	?assertEqual(IC, lib_color:color(H)),
	?assertEqual(100000, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(IC, lib_color:color(H2)),
	?assertEqual(10000, lib_color:quantity(H2)),
	[H3|T3] = T2,
	?assertEqual(?Uncolored, lib_color:color(H3)), %% Marker
	?assertEqual(0, lib_color:quantity(H3)), 
	[H4|_T4] = T3,
	?assertEqual(red, lib_color:color(H4)), 
	?assertEqual(2800,  lib_color:quantity(H4)). 

% Shouldn't happen, but maybe if a colored output was
% inserted into a coinbase transaction....
marker_no_input() ->
	Inputs = [],
	_IC = lib_color:get_issue_color(Inputs, []),
	Unspents = inputs_to_unspents(Inputs),
	QList = [100000, 10000, 2800, 101],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|_T] = TransferOutputs,
	?assertEqual(4, length(TransferOutputs)),
	?assertEqual(?Uncolored, lib_color:color(H)),
	?assertEqual(0, lib_color:quantity(H)).

% This would be a malformed a malicious marker set
% Should we color like normal or discard?
% I think we should color like normal but funds are probably
% lost in OP_RETURN outputs
% Current network code will reject a transaction with more than 1 OP_RETURN
multiple_markers() ->
	Inputs = colored_inputs(red),
	Unspents = inputs_to_unspents(Inputs),
	IC = lib_color:get_issue_color(Inputs, Unspents),
	QList = [100000, 2800, 101],
	QList2 = [200000, 1800, 201],
	MOutput = create_output(?Uncolored, 0, 1),
	MOutput2 = create_output(?Uncolored, 0, 2),
	Marker = create_marker(MOutput, QList),
	Marker2 = create_marker(MOutput2, QList2),
	O = [create_output(?Uncolored, 0, 0),
		 Marker,
		 Marker2,
		 create_output(?Uncolored, 0, 4)],
	TransferOutputs = lib_color:color_outputs(Inputs, O, Unspents),
	[H|T] = TransferOutputs,
	?assertEqual(4, length(TransferOutputs)),
	?assertEqual(IC, lib_color:color(H)),
	?assertEqual(100000, lib_color:quantity(H)),
	[H2|T2] = T,
	?assertEqual(?Uncolored, lib_color:color(H2)), %% Marker 1
	?assertEqual(0, lib_color:quantity(H2)), 
	[H3|_T3] = T2,
	?assertEqual(red, lib_color:color(H3)),  %% Marker 2
	?assertEqual(2800, lib_color:quantity(H3)).


is_colored() ->
	Outputs = [create_output(?Uncolored, 0, 0),
			  create_output(?Uncolored, 0, 1),
			  create_output(?Uncolored, 0, 2)],
	Outputs2 = [create_output(?Uncolored, 0, 0),
			  create_output(red, 0, 1),
			  create_output(?Uncolored, 0, 2)],

	?assertEqual(false, lib_color:is_colored(Outputs)),
	?assertEqual(true, lib_color:is_colored(Outputs2)).

issue_and_find() ->
	Inputs = colored_inputs(red),
	Unspents = colored_outputs(red),
	WalletUnspents = unspents_to_dict(Unspents),
	IC = lib_color:get_issue_color(Inputs, WalletUnspents),
	ColoredUnspents = lib_transact:get_colored_unspents(dict, red, WalletUnspents),
	X = lib_color:find_color(IC, lib_kd:oldest(ColoredUnspents)),
	?assertEqual(1, length(X)).

create_and_compare() ->
	Outputs = colored_outputs(red),
	[O|_] = Outputs,
	A = lib_color:from(O),
	B = lib_color:new(unknown, red),
	?assertEqual(A, B).

colors() ->
	Unspents = colored_outputs(red),
	WalletUnspents = unspents_to_dict(Unspents),
	C = lib_color:colors(lib_kd:oldest(WalletUnspents)),
	?assertEqual(1, length(C)),
	[Color|_] = C,
	?assertEqual(red, Color),
	Unspents2 = colored_outputs(red) ++
		colored_outputs(blue) ++
		colored_outputs(black),
	WalletUnspents2 = unspents_to_dict(Unspents2),
	C2 = lib_color:colors(lib_kd:oldest(WalletUnspents2)),
	?assertEqual(3, length(C2)),
	[_|T] = C2,
	[Color2|_] = T,
	?assertEqual(blue, Color2).

parse_definition() ->
  Def = lib_test:data("exampleasset.json"),
  Color = lib_color:from_json(Def),
  ?assertEqual(<<"blahtest">>, Color#color.name).

includes_color() ->
    Def = lib_test:data("exampleasset.json"),
    Color = lib_color:from_json(Def),
    ?assertEqual(true, lib_color:includes(Color, "3LT1JUt54hEnqZERBf3r1HLpNMQTqCbwEa")).

new_colors() ->
    C = lib_color:new("3LT1JUt54hEnqZERBf3r1HLpNMQTqCbwEa"),
    ?assertEqual(C, lib_color:new(C)).

serialize_color() ->
	ok.
%    Def = lib_test:data("exampleasset2.json"),
%    Color = lib_color:from_json(Def),
%    Json = lib_color:to_json(Color),
%    ?assertEqual(Def, erlang:iolist_to_binary([Json, "\n"])).

get_marker() ->
	% Just return the raw marker from a set of outputs..
	O1 = [],
	?assertMatch(error, lib_color:marker(O1)),
	O2 = [create_output(red, 0, 1),
		  create_output(blue, 0, 2),
		  create_output(green, 0, 3)],
	?assertMatch(error, lib_color:marker(O2)),
	QList = [100000, 10000, 2800, 101],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3)],
	M = lib_color:marker(O),
	?assert(M =/= error).

get_meta() ->
	O1 = [],
	?assertMatch(error, lib_color:meta(O1)),
	O2 = [create_output(red, 0, 1),
		  create_output(blue, 0, 2),
		  create_output(green, 0, 3)],
	?assertMatch(error, lib_color:meta(O2)),
	QList = [100000, 10000, 2800, 101],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList, "metadatatest"),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3)],
	M = lib_color:meta(O),
	?assertEqual(<<"metadatatest">>, M).


% No URL should be returned on transfer Marker
get_meta_url_transfer() ->
	QList = [100000, 10000, 2800, 101],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList, "u=https://cpr.sm/ziL7oMhHos"),
	O = [Marker,
	  create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			create_output(?Uncolored, 0, 3)],
    ?assertEqual(error, lib_color:meta_url(O)).


get_meta_url() ->
	QList = [100000, 10000, 2800, 101],
	MOutput = create_output(),
	Marker = create_marker(MOutput, QList, "u=https://cpr.sm/ziL7oMhHos"),
	O = [create_output(?Uncolored, 0, 1),
			create_output(?Uncolored, 0, 2),
			Marker,
			create_output(?Uncolored, 0, 3)],
	M = lib_color:meta_url(O),
    ?assertEqual("https://cpr.sm/ziL7oMhHos", M).

new_addresses() ->
    C = lib_color:new("ALn3aK1fSuG27N96UGYB1kUYUpGKRhBuBC"),
    ?assertEqual("ALn3aK1fSuG27N96UGYB1kUYUpGKRhBuBC", lib_color:readable(btr_net_params:params(), C)),
    D = lib_color:new("3QzJDrSsi4Pm2DhcZFXR9MGJsXXtsYhUsq"),
    ?assertEqual("Af59wop4VJjXk2DAzoX9scAUCcAsghPHFX", lib_color:readable(btr_net_params:params(), D)).

is_color_address() ->
    ?assertEqual(true, lib_color:is_color_address("Af59wop4VJjXk2DAzoX9scAUCcAsghPHFX")),
    ?assertEqual(false, lib_color:is_color_address("Af59wop4VJjXk2DAzoX9scAUCcAblahah")).


%% Validate color records
validate_color() ->
    A = #color{},
    ?assertThrow(color_record_validation_error, lib_color:validate(A)),
    B = #color{asset_ids=[], short_name="short", name="short_test"},
    ?assertThrow(color_record_validation_error, lib_color:validate(B)),
    C = #color{asset_ids=["Af59wop4VJjXk2DAzoX9scAUCcAsghPHFX",
                          "3QzJDrSsi4Pm2DhcZFXR9MGJsXXtsYhUsq"],
               short_name="Short",
               name="Short_test"},
    ?assertEqual(true, lib_color:validate(C)),
    D = #color{asset_ids=["Af59wop4VJjXk2DAzoX9scAUCcAsghPHFX",
                          "Madeupassetname"],
               short_name="Short",
               name="Short_test"},
    ?assertThrow(color_record_validation_error, lib_color:validate(D)),
    E = #color{asset_ids=["Af59wop4VJjXk2DAzoX9scAUCcAsghPHFX",
                          "3QzJDrSsi4Pm2DhcZFXR9MGJsXXtsYhUsq"],
               short_name="Verylongshortname",
               name="Short_test"},
    ?assertThrow(color_record_validation_error, lib_color:validate(E)).

bin_color_address() ->
    ?assertEqual(lib_color:color_address("ASUAMR8Kqcy3WEgBE7nXVbUwbhK1KMZH8N"),
                 lib_color:color_address(<<"ASUAMR8Kqcy3WEgBE7nXVbUwbhK1KMZH8N">>)).

json_decode() ->
    JsonString = "{\r\n  \"asset_ids\": [\r\n    \"ASUAMR8Kqcy3WEgBE7nXVbUwbhK1KMZH8N\"\r\n  ],\r\n  \"contract_url\": null,\r\n  \"name_short\": \"NCK\",\r\n  \"name\": \"ColorMaster\",\r\n  \"issuer\": \"\",\r\n  \"description\": \"\",\r\n  \"description_mime\": \"text/x-markdown; charset=UTF-8\",\r\n  \"type\": \"Points\",\r\n  \"divisibility\": 9,\r\n  \"link_to_website\": false,\r\n  \"icon_url\": null,\r\n  \"image_url\": null,\r\n  \"version\": \"1.0\"\r\n}",
    A = lib_color:from_json(JsonString),
    ?assertEqual(true, lib_color:validate(A)).

bad_type_color() ->
    A = {color,
        <<162,242,229,197,141,84,159,165,66,12,
          127,192,204,5,49,139,47,255,59,251>>,
        <<"knifecoins">>,
        [<<"AWdU6PhcDETQWDg5ogrt5DF7N7wnmawoFx">>],
        "https://cpr.sm/ozvkimjr_m",<<"KFC">>,
        <<"KFCrypto">>,
        <<"The cutting edge. ">>,
        <<"text/x-markdown; charset=UTF-8">>,
        <<"Collectible">>,0,false,
        <<"https://coinprism.blob.core.windows.net/profile/icon/3MWCMBTsF3tNzuK3WmvRfpsTVkEZxBmyFQ.jpg">>,
        <<"https://coinprism.blob.core.windows.net/profile/image/3MWCMBTsF3tNzuK3WmvRfpsTVkEZxBmyFQ.jpg">>,
        <<"1.0">>,
        true},
    B = #color{bin = <<162,242,229,197,141,84,159,165,66,12,127,192,204,5,49,139,47,255,59,251>>},
    AColor = A#color.bin,
    BColor = B#color.bin,
    AColor = BColor.

% Demonstrates encoded marker size variation on number of disparate colors encoded
marker_size() ->
    %% 24 Encoded Transfers of large quant
    ?assertThrow(marker_error,
                 lib_color:create_marker_output(lists:seq(1000000,1000023))),
    %% 72 encoded transfer
    ?assertThrow(marker_error,
                 lib_color:create_marker_output(lists:seq(1,72))),
    %% 71 encoded transfer OK (80 byte OP_RETURN)
    lib_color:create_marker_output(lists:seq(1,71)).

marker_meta() ->
    M = lib_color:create_marker_output([10000,100,300], "u=http://cpr.sm/Ekdisj"),
	B = lib_parse:parse_script(M#btxout.script),
	?assertEqual({openassets, {[10000,100,300], <<"u=http://cpr.sm/Ekdisj">>}}, B).

encode_metaurl() ->
    ?assertEqual(<<"u=http://blah.com">>, lib_color:encode_metaurl("http://blah.com")),
    ?assertEqual(<<>>, lib_color:encode_metaurl("u=http://blah.com")),
    ?assertEqual(<<>>, lib_color:encode_metaurl(<<"http://www.test.com">>)).

% Issue and transfer from coinprism broke
% These are the raw transactions in the transaction chain

%coinprism_broke() ->
%ParentTx = lib_tx:from_hex("0100000001ed93b0b8661a963ffa8bf6892ab3f6fe759841f5acabf5f6ab5ec8bc405c671d010000006b483045022100868d9c79852fea63379dfacfda41283e18059e65410e9234c8c708a100e76ecf02203c36590324f9aeb3044f483a604f92197b4068325c7a92ac2ad3de1e56f9b0820121034b60caaa6cbdb682cab6ff7e549a85293b2d4316c34609ac4c2bfd933bb23772ffffffff0220a10700000000001976a914c678474b21289570874d18caffe1949eb95860ef88ac00171100000000001976a914ed5f49321a16029d76b627e273a6747280df0d5a88ac00000000"),
%IssueTx = lib_tx:from_hex("010000000162ec5ca0046a019b3938c6565ddb0298a902c54773712cdbb614c85c88819834000000006b4830450221009dbed0e66be2e1ddf9f399b98a49684515364395a9cfb37416217db75c54238d0220640cec128d4fbdfb3ebaecfe8f82a81a2f099aa0819abb4d8dceb68ee0c2d94f012103c685c4455bdcb0c416791c8f903e51aee77c40d082a56a2b0e140e18b2952b2fffffffff0358020000000000001976a914c678474b21289570874d18caffe1949eb95860ef88ac0000000000000000266a244f41010001c0843d1b753d68747470733a2f2f6370722e736d2f7a694c376f4d68486f73b8770700000000001976a914c678474b21289570874d18caffe1949eb95860ef88ac00000000"),
%	utxo:unconfirmed_tx_sync(ParentTx),
%	% Try to color outputs.
%	Outputs = lib_color:color_outputs(IssueTx#btxdef.txinputs,
%			IssueTx#btxdef.txoutputs, fun(X,Y) -> utxo:lookup_tx(X,Y) end),
%	[O1|_] = Outputs, 
%	?assertEqual(lib_color:hash160("3LT1JUt54hEnqZERBf3r1HLpNMQTqCbwEa"),
%		O1#btxout.color),
%	% manually color IssueTx
%	utxo:unconfirmed_tx_sync(IssueTx#btxdef{txoutputs =
%			lib_color:color_outputs(IssueTx#btxdef.txinputs, IssueTx#btxdef.txoutputs, fun(X,Y) -> utxo:lookup_tx(X,Y) end)}),
%	{ok, Unspent} = utxo:lookup_tx(<<185,62,24,217,60,152,109,190,13,17,101,50,134,210,129,67,147,130,195,16,63,
%  18,0,228,148,145,93,106,34,162,135,251>>, 0),
%	?assertEqual(lib_color:hash160("3LT1JUt54hEnqZERBf3r1HLpNMQTqCbwEa"),
%		Unspent#utxop.color),
%	?assertEqual(1000000, Unspent#utxop.quantity),
%	NextTx = lib_tx:from_hex("0100000002b93e18d93c986dbe0d11653286d281439382c3103f1200e494915d6a22a287fb000000006c493046022100dff7df518f48adf12f0d867070dbbbda8629a7789d0eeb96877f23dd32f373c202210082e76547c72f3f86350508b03d8f4446a64fa8540a300ef077ed17bb13c84e20012103c685c4455bdcb0c416791c8f903e51aee77c40d082a56a2b0e140e18b2952b2fffffffffb93e18d93c986dbe0d11653286d281439382c3103f1200e494915d6a22a287fb020000006a473044022071bd4ef116145c5563f9d3327384448a79bebc06d7002fac1e77bb094d911a3b02200e7f644760f0d4b4905f41fafcbf172c97158566b9a2a053a17392ef9adce2d9012103c685c4455bdcb0c416791c8f903e51aee77c40d082a56a2b0e140e18b2952b2fffffffff0400000000000000000e6a0c4f4101000290a10fb0e32d0058020000000000001976a914453546cf55d55d7570efd08e0ca5840fb3a4171e88ac58020000000000001976a914c678474b21289570874d18caffe1949eb95860ef88ac504e0700000000001976a914c678474b21289570874d18caffe1949eb95860ef88ac00000000"),
%	Outputs2 = lib_color:color_outputs(NextTx#btxdef.txinputs, NextTx#btxdef.txoutputs, fun utxo:lookup_tx/2),
%	[_|R] = Outputs2,
%	[O2|R2] = R,
%	[O3|R3] = R2,
%	[O4|_] = R3,
%	?assertEqual(lib_color:hash160("3LT1JUt54hEnqZERBf3r1HLpNMQTqCbwEa"),
%                 O2#btxout.color),
%	?assertEqual(250000, O2#btxout.quantity),
%	?assertEqual(lib_color:hash160("3LT1JUt54hEnqZERBf3r1HLpNMQTqCbwEa"),
%                 O3#btxout.color),
%	?assertEqual(750000, O3#btxout.quantity),
%	?assertEqual(?Uncolored, O4#btxout.color),
%	?assertEqual(0, O4#btxout.quantity).
%
%% What is the correct coloring behaviour for multiple embedded OP_Returns?

color_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
		{"No Color Marker", fun no_marker/0},
		{"Only marker malformed", fun marker_only/0},
		{"Uncolored List", fun uncolored_list/0},
        {"Mixed Color List", fun mixed_list/0},
		{"Uncoloring", fun uncolor_outputs/0},
        {"Issue Color", fun issue_color/0},
		{"Single color quant", fun single_color_quant/0},
        {"Zero color quant", fun zero_color_quant/0},
		{"Partial quantity", fun partial_quant/0},
		{"Multiple fill plus partial", fun multi_partial_quant/0},
		{"Total multiple fill", fun total_fill_quant/0},
		{"Multiple small fills", fun multi_fill_quant/0},
		{"Multiple Color fills", fun multi_color_quant/0},
		{"Mixed over boundary fill", fun mixed_colors/0},
	    {"Mixed boundary fill with uncolored components", fun uncolored_overflow/0},
		{"Single issuance", fun simple_issuance/0},
		{"Multiple issuance", fun multiple_issuance/0},
		{"Uncolored issuance", fun uncolored_issuance/0},
		{"Empty marker issue", fun empty_marker_issue/0},
		{"One simple transfer", fun one_transfer/0},
		{"Multiple transfers", fun multiple_transfers/0},
		{"Multiple boundary transfers", fun multiple_boundary_transfers/0},
		{"Color outputs only issuance.", fun full_color_issuance_only/0},
		{"Full color transfer.", fun full_color_transfer/0},
		{"Multi color transfer and issue.", fun multi_color_transfer_issue/0},
		{"Rainbow test", fun rainbow_colors/0},
		{"Wallet color quantity", fun color_quantity/0},
		{"Color aggregate payees", fun color_aggregate/0},
		{"Encode and Decode marker", fun marker_reverse/0},
		{"Marker too big", fun marker_toobig/0},
		{"Interleave colored and ?Uncolored", fun interleave/0},
		{"Partial Interleave", fun partial_interleave/0},
		{"Uncolored issue and transfer", fun uncolored_issue_transfer/0},
		{"Colored issue and transfer", fun colored_issue_transfer/0},
		{"Marker overrun into missing outputs", fun marker_overrun/0},
		{"Marker with no corresponding first input", fun marker_no_input/0},
		{"Multiple conflicting markers", fun multiple_markers/0},
		{"Are the outputs colored", fun is_colored/0},
		{"Issue and find", fun issue_and_find/0},
		{"Create and compare", fun create_and_compare/0},
		{"Grab unique color list", fun colors/0},
		{"Parse color definition to color def", fun parse_definition/0},
		{"Includes color in asset ids", fun includes_color/0},
		{"New Color constructors", fun new_colors/0},
		{"Serialize color def to definition", fun serialize_color/0},
	    {"Grab the marker from outputs", fun get_marker/0},
	    {"Get metadata from outputs", fun get_meta/0},
	    {"Get metadata URL", fun get_meta_url/0},
	    {"Get metadata URL transfer", fun get_meta_url_transfer/0},
	    {"New style addresses", fun new_addresses/0},
	    {"New address check", fun is_color_address/0},
	    {"Validate color recs", fun validate_color/0},
	    {"Json decode test", fun json_decode/0},
	    {"Bin or List color", fun bin_color_address/0},
        {"Bad color type", fun bad_type_color/0},
        {"Marker size too big", fun marker_size/0},
        {"Marker meta data encode", fun marker_meta/0},
        {"Encode meta url", fun encode_metaurl/0}
%	    {"Coinprism issue", fun coinprism_broke/0}
   ]
  }.

%%% Utxo Input Set Generators

outputs_to_inputs(OutputList) ->
	lists:map(fun(X) -> lib_test:output_to_input(X) end, OutputList).

colored_inputs(Color) ->
	outputs_to_inputs(lists:reverse(colored_outputs(Color))).

colored_outputs(Color) ->
    fakeutxo:foldl(fun(Unspent, Acc) -> 
					  case lib_color:color(Unspent) of
					  	  Color -> [Unspent|Acc];
					  	  _ -> Acc
					  end
				   end, []).

%%% Lets mock up outputs

create_marker(Output, QuantList) when is_list(QuantList) ->
	create_marker(Output, QuantList, "Random meta data").
create_marker(Output, QuantList, Metadata) when is_list(QuantList) ->
    bblock:replace_script(Output, lib_color:encode_marker(QuantList, list_to_binary(Metadata))).

create_outputs(Num) -> create_outputs(?Uncolored, 0, Num).
create_outputs(Color, ColorQuant, Num) ->
	lists:map(fun(X) -> create_output(Color, ColorQuant, X) end, lists:seq(0,Num-1)).

create_output() -> create_output(?Uncolored, 0, 0).
create_output(Color, ColorQuant, TxIndex) -> lib_color:set_color(lib_test:create_output(TxIndex), Color, ColorQuant).

%%% Block injection
%%% Let's inject the colors of the Rainbow!
fake_colored_block() -> fakeutxo:import().

get_total_quant(ColorList, Color) ->
	lists:foldl(fun(L,Sum) ->
					case L of
						{Color, Y} ->
							Sum + Y;
						_ ->
							Sum
					end
				end, 0, ColorList).

%%% Wallet / Utxo Dict

unspents_to_dict(Utxo) ->
	lists:foldl(fun(O, Acc) ->
					dict:store({lib_unspent:hash(O), lib_unspent:index(O)}, O, Acc)
				end, dict:new(), Utxo).

inputs_to_unspents(Inputs) ->
	lists:foldl(fun(I, Acc) ->
	                    Hash = bblock:hash(I),
	                    Index = bblock:index(I),
	            {ok, U} = fakeutxo:lookup_tx(Hash, Index),
	            dict:store({Hash, Index}, U, Acc)
	    end, dict:new(), Inputs).
