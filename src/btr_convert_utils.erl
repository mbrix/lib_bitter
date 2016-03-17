%% Quick utilities for doing format conversion when
%% block records are modified.

-module(btr_convert_utils).
-author('mbranton@emberfinancial.com').

-export([convert_block/1]).
-include_lib("bitter.hrl").

convert_block(Filename) ->
	A = lib_test:term_from_file(Filename),
	NewBlock = convert(A),
	lib_test:term_to_file(NewBlock, Filename).

convert(Block) when is_record(Block, bbdef) ->
	Block#bbdef{txdata = convert_txdata(Block#bbdef.txdata)}.

convert_txdata(Txdata) -> convert_txdata(Txdata, []).

convert_txdata([], Data) -> lists:reverse(Data);
convert_txdata([H|T], Data) ->
	convert_txdata(T, [H#btxdef{txoutputs = convert_outputs(H#btxdef.txoutputs)}|Data]).

convert_outputs(Outputs) -> convert_outputs(Outputs, []).

convert_outputs([], Converted) -> lists:reverse(Converted);
convert_outputs([H|T], Converted) ->
	{Name, Txindex, Value, Script, Address, Info, Color, Quantity} = H,
	case Color of
		?Uncolored -> convert_outputs(T, [{Name, Txindex, Value, Script, Address, Info, #{}}|Converted]);
		_ -> convert_outputs(T, [{Name, Txindex, Value, Script, Address, Info, #{color => Color,
																				quantity => Quantity}}|Converted])
	end.
