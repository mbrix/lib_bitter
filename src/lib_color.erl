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

-module(lib_color).
-author('mbranton@emberfinancial.com').

-export([color_outputs/3]).

% Testing exports
-export([find_marker/1,
         encode_marker/2,
	     build_color_list/2,
	     uncolor_all/1,
	     get_issue_color/2,
	     get_issue_color_unspents/1,
	     get_color_quant/2,
	     do_issuance/3,
		 do_transfers/3,
	     color_aggregate/1,
	     colors/1,
	     is_colored/1,
	     check_colored/1,
	     marker/1,
	     meta/1,
	     meta_url/1,
	     readable/2,
	     readable/3,
	     readable_colors/2,
	     readable_issue_colors/2,
	     find_color/2,
	     from/1,
	     new/0,
	     new/1,
	     new/2,
	     quantity/1,
	     color/1,
	     from_json/1,
	     set_color/3,
	     erase_color/1,
	     hash160/1,
	     find_spend_color/2,
	     new_spend_color/1,
	     script_to_ic/1,
	     validate/1,
	     is_color_address/1,
	     color_address/1,
	     includes/2,
	     encode_metaurl/1,
	     to_json/2,
	     to_map/2,
	     to_cprism/2,
	     to_cprism_map/2,
	     unspent_to_ic/1]).

% testing
-export([create_marker_output/1,
         create_marker_output/2]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

% Open assets extended meta data support
%
%

quantity(V) -> lib_unspent:get_attribute(quantity, V, 0).
color(V) -> lib_unspent:get_attribute(color, V, ?Uncolored).

val(Name, Map) ->
	try maps:get(Name, Map) of
		Value ->
			Value
	catch
		_:_ ->
			null
	end.

parse_color(Map) ->
	case val(<<"asset_ids">>, Map) of
		X when is_list(X) ->
			[C|_] = X,
			hash160(C);
		_ ->
		    case val(<<"source_addresses">>, Map) of
		        X when is_list(X) ->
		            [C|_] = X,
		            hash160(C);
		        _ ->
		            null
            end
	end.

source_list(NetworkParams, Color) ->
	[erlang:list_to_binary(readable(NetworkParams, Color))].


get_asset_ids(D) ->
    case val(<<"source_addresses">>, D) of
        null ->
            case val(<<"asset_ids">>, D) of
                null -> [];
                A -> A
            end;
        N -> N
    end.

includes(ColorRec, Color) when is_record(ColorRec, color) ->
    C = hash160(new(Color)),
    A = lists:map(fun(E) -> hash160(E) end, ColorRec#color.asset_ids),
    lists:any(fun(E) -> E =:= C end, A).


from_json(Definition) ->
	D = jiffy:decode(Definition, [return_maps]),
	#color{name = val(<<"name">>, D),
	    bin  = parse_color(D),
		asset_ids = get_asset_ids(D), 
		contract_url = val(<<"contract_url">>, D),
		short_name = val(<<"name_short">>, D),
		issuer = val(<<"issuer">>, D),
		description = val(<<"description">>, D),
		mime_type = val(<<"description_mime">>, D),
		type = val(<<"type">>, D),
		divisibility = val(<<"divisibility">>, D),
		link_to_website = val(<<"link_to_website">>, D),
		icon_url = val(<<"icon_url">>, D),
		image_url = val(<<"image_url">>, D),
		version = val(<<"version">>, D)}.

to_json(NetworkParams, Color) when is_record(Color, color) ->
	jiffy:encode(to_map(NetworkParams, Color)).

to_map(NetworkParams, Color) when is_record(Color, color) ->
	#{<<"name">> => Color#color.name,
			       <<"asset_ids">> => source_list(NetworkParams, Color),
			       <<"contract_url">> => to_binary(Color#color.contract_url),
			       <<"name_short">> => to_binary(Color#color.short_name),
			       <<"issuer">> => to_binary(Color#color.issuer),
			       <<"description">> => to_binary(Color#color.description),
			<<"description_mime">> => to_binary(Color#color.mime_type),
			<<"type">> => to_binary(Color#color.type),
			<<"divisibility">> => Color#color.divisibility,
			<<"link_to_website">> => to_binary(Color#color.link_to_website),
			<<"icon_url">> => to_binary(Color#color.icon_url),
			<<"image_url">> => to_binary(Color#color.image_url),
			<<"version">> => Color#color.version
	 }.

to_cprism(NetworkParams, Color) when is_record(Color, color) ->
	jiffy:encode(to_cprism_map(NetworkParams, Color)).

to_cprism_map(NetworkParams, Color) when is_record(Color, color) ->
	#{<<"asset_id">> => erlang:list_to_binary(readable(NetworkParams, Color)),
	  <<"metadata_url">> => to_binary(Color#color.contract_url),
	  <<"final_metadata_url">> => to_binary(Color#color.contract_url),
	  <<"verified_issuer">> => false,
	  <<"name">> => Color#color.name,
	  <<"contract_url">> => to_binary(Color#color.contract_url),
	  <<"name_short">> => to_binary(Color#color.short_name),
	  <<"issuer">> => <<"null">>,
	  <<"description">> => to_binary(Color#color.description),
	  <<"description_mime">> => to_binary(Color#color.mime_type),
	  <<"type">> => to_binary(Color#color.type),
	  <<"divisibility">> => Color#color.divisibility,
	  <<"icon_url">> => to_binary(Color#color.icon_url),
	  <<"image_url">> => to_binary(Color#color.image_url)
	 }.


to_binary(A) when is_list(A) -> iolist_to_binary(A);
to_binary(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
to_binary(A) when is_binary(A) -> A;
to_binary(A) -> A.

validate(Color) when is_record(Color, color) ->
    validate_field(asset_ids, Color#color.asset_ids),
    validate_field(name_short, Color#color.short_name),
    validate_field(name, Color#color.name),
    true.

validate_field(_, null) ->
    throw(color_record_validation_error);
validate_field(asset_ids, []) ->
    throw(color_record_validation_error);
validate_field(asset_ids, Assets) when is_list(Assets) ->
    try
        lists:foreach(fun(E) -> color_address(E) end, Assets),
        true
    catch
        _:_ ->  throw(color_record_validation_error)
    end;
validate_field(name_short, Name) when is_list(Name), length(Name) > 10 ->
    throw(color_record_validation_error);
validate_field(name_short, "") ->
    throw(color_record_validation_error);
validate_field(name_short, _) -> true;
validate_field(name, "") ->
    throw(color_record_validation_error);
validate_field(name, _) ->
    true;
validate_field(_, _) -> throw(color_record_validation_error).

% Open Assets Color Support
%
from(U) when is_record(U, utxop) or is_record(U, us) ->
	% Should do color identity lookup
	#color{name = unknown,
		   bin = lib_unspent:get_attribute(color, U, ?Uncolored)};

from(O) when is_record(O, btxout) or is_record(O, btx) ->
	% Should do color identity lookup
	#color{name = unknown,
		   bin  = lib_unspent:get_attribute(color, O, ?Uncolored)}.

new() ->
	#color{}.

new(<<"uncolored">>) -> new(uncolored);
new("uncolored") -> new(uncolored);

new(uncolored) ->
    #color{name = "Uncolored",
           bin = ?Uncolored};

new(Color) when is_record(Color, color) ->
    Color;

% Used by test code
new(ColorAtom) when is_atom(ColorAtom) ->
    #color{name = ColorAtom,
           bin = ColorAtom};

new(ColorName) when is_list(ColorName) ->
	#color{name = unknown,
		   bin = hash160(ColorName)};

new(ColorBin) when is_binary(ColorBin) ->
    #color{name = unknown,
       bin = ColorBin}.

new(binary, ColorName) ->
	#color{name = unknown,
		   bin = hash160(ColorName)};

new(Name, U) when is_record(U, utxop) ->
	IC = unspent_to_ic(U),
	#color{name = Name,
		   bin = IC};

new(Name, O) when is_record(O, btxout) or is_record(O, boutput) ->
	#color{name = Name,
		   bin = lib_unspent:get_attribute(color, O, ?Uncolored)};

% Should be binary, but occasionally non-binary in test code
new(Name, ColorBin) ->
	#color{name = Name,
		   bin  = ColorBin}.

% Search the unspent pool for
% uncolored outputs that match
% the desired issue color spec
find_color(Color, Unspents) when is_record(Color, color) ->
	find_color(Color#color.bin, Unspents);
find_color(Color, Unspents) ->
	lists:filter(fun(E) ->
		unspent_to_ic(E) =:= Color
		end, Unspents).

% If a member of the marker is invalid
% Invalidate the entire marker quantity list
check_quantity(X) when is_float(X) -> invalid;
check_quantity(X) when X < 0 -> invalid;
check_quantity(_) -> valid.

validate_quantity_list(QList) ->
	lists:foldl(fun(X, List) ->
				case check_quantity(X) of
					valid ->
						List;
					invalid ->
						[]
				end
		end, QList, QList).

% This checks if the block or tx might be colored based on the presence
% of the marker output.
check_colored(Tx) when is_record(Tx, btx) or is_record(Tx, btxdef) -> has_marker(find_marker(bblock:outputs(Tx)));

check_colored(Block) when is_record(Block, bblock) ->
	try
		bblock:foreach(fun(Btx) -> false = has_marker(find_marker(bblock:outputs(Btx))) end, Block),
		false
	catch _:_ -> true
	end;

check_colored(Block) when is_record(Block, bbdef) ->
    try
        lists:foreach(fun(Tx) ->
                    false = has_marker(find_marker(Tx#btxdef.txoutputs)) end,
                      Block#bbdef.txdata),
        false
    catch _:_ -> true
    end.

has_marker(?Uncolored) -> false;
has_marker(_) -> true.



find_marker(O) -> find_marker(O, []).
find_marker([], _) -> ?Uncolored;
find_marker(Outputs, Issuance) ->
	[H|T] = Outputs,
	case bblock:info(H) of
		{openassets, M} ->
			{QList, _} = M,
			{validate_quantity_list(QList),
				lists:reverse(Issuance), H, T};
		_ ->
			find_marker(T, [H|Issuance])
	end.

color_outputs([], O, _) -> uncolor_all(O);
color_outputs(Inputs, Outputs, UnspentDict) ->
	case find_marker(Outputs) of
		{[], _, _, _} ->
			uncolor_all(Outputs);
		{Q, [], M, Transfer} ->
			TransferList = do_transfers(build_color_list(Inputs,
						UnspentDict),
				         Q, Transfer),
			[M|TransferList];
		{Q, Issuance, M, Transfer} ->
			IC = get_issue_color(Inputs, UnspentDict),
			{Q2, IssuedList} = do_issuance(IC, Q, Issuance),
			ColorList = build_color_list(Inputs, UnspentDict),
			TransferList = do_transfers(ColorList,
				         Q2, Transfer),
			IssuedList ++ [M] ++ TransferList;
		?Uncolored ->
			uncolor_all(Outputs)
	end.

get_color_quant([], _) -> {?Uncolored, []};
get_color_quant(ColorList, Q) ->
	[{Color, _}|_CT] = ColorList,
	get_color_quant(Color, ColorList, Q).

%% This should prevent overflow
%% and terminate coloring when color boundaries are broken
get_color_quant(?Uncolored, _, Q) when Q > 0 ->
    {?Uncolored, []};
get_color_quant(_Color, [], Q) when Q > 0 ->
	{?Uncolored, []};
get_color_quant(_, ColorList, 0) ->
	{?Uncolored, ColorList};
get_color_quant(Color, ColorList, Q) ->
	[{CColor, AvailableQuantity}|CT] = ColorList,
	NextColor = if CColor =/= Color ->
	      			?Uncolored;
	   			   true ->
		  			Color
    			end,
	if Q < AvailableQuantity ->
	        try
			    {NextColor, [{CColor, AvailableQuantity-Q}|CT]}
            catch _:_ ->
                    io:format("XXXX: ~p ~p ~p  ~n", [CColor, AvailableQuantity, Q]),
                    io:format("XXXX: ~p~n", [ColorList]),
                    throw(coloring_error)
            end;
	   Q =:= AvailableQuantity ->
	   		{NextColor, CT};
	   Q > AvailableQuantity ->
	   		get_color_quant(NextColor, CT, Q - AvailableQuantity)
	end.


do_transfers(ColorList, M, TransferOutputs) ->
	do_transfers(ColorList, M, TransferOutputs, []).

do_transfers(_, [], TransferOutputs, Acc) ->
	lists:reverse(Acc) ++ uncolor_all(TransferOutputs);
do_transfers([], _, TransferOutputs, Acc) ->
	lists:reverse(Acc) ++ uncolor_all(TransferOutputs);
do_transfers(_, _, [], Acc) -> % OverFlow
	lists:reverse(Acc);
do_transfers(ColorList, M, TransferOutputs, Acc) ->
	[QuantityNeeded|MT] = M,
	[Output|OT] = TransferOutputs,
	{OutputColor, NewColorList} = get_color_quant(ColorList, QuantityNeeded),
	case OutputColor of
		?Uncolored -> do_transfers(NewColorList, MT, OT, [set_color(Output, OutputColor, 0)|Acc]);
	    _ -> do_transfers(NewColorList, MT, OT, [set_color(Output, OutputColor, QuantityNeeded)|Acc])
	end.

do_issuance(IC, M, IssuedOutputs) ->
	do_issuance(IC, M, IssuedOutputs, []).
do_issuance(_IC, M, [], Acc) ->
	{M, lists:reverse(Acc)};
do_issuance(_IC, [], IssuedOutputs, Acc) ->
	{[], lists:reverse(Acc) ++ uncolor_all(IssuedOutputs)};
do_issuance(IC, M, IssuedOutputs, Acc) ->
	[Q|MT] = M,
	[O|T] = IssuedOutputs,
	case Q of
		0 -> do_issuance(IC, MT, T, [erase_color(O)|Acc]);
		_ -> do_issuance(IC, MT, T, [set_color(O, IC, Q)|Acc])
	end.

set_color(R, Color, Quant) -> lib_unspent:set_attribute(quantity, Quant, lib_unspent:set_attribute(color, Color, R)).
erase_color(R) -> lib_unspent:del_attribute(quantity, lib_unspent:del_attribute(color, R)).


get_issue_color([], _) -> ?Uncolored;
get_issue_color(Inputs, UnspentDict) ->
	try
		[I|_] = Inputs,
		%io:format("~p ~p ~p ~n", [bblock:hash(I), bblock:index(I), UnspentDict]),
		N = fetch(bblock:hash(I), bblock:index(I), UnspentDict),
		crypto:hash(ripemd160, crypto:hash(sha256, lib_unspent:script(N)))
	catch
		E:R -> 
			io:format("ZZZ E: ~p ~n R: ~p~n", [E, R]),
			throw(coloring_error)
	end.

get_issue_color_unspents([]) -> ?Uncolored;
get_issue_color_unspents(Unspents) ->
	[U|_] = Unspents,
	unspent_to_ic(U).

unspent_color_dict(UnspentList) ->
    unspent_color_dict(UnspentList, dict:new()).

unspent_color_dict([], ColorDict) -> ColorDict;
unspent_color_dict(UnspentList, ColorDict) ->
    [H|T] = UnspentList,
	case lib_unspent:get_attribute(color, H, ?Uncolored) of
        ?Uncolored ->
            HColor = unspent_to_ic(H),
            C2 = dict:store(HColor, H, ColorDict),
            unspent_color_dict(T, C2);
        _ -> unspent_color_dict(T, ColorDict)
    end.

find_spend_color(Color, Unspents) ->
    C2 = new(Color),
    ColorDict = unspent_color_dict(Unspents),
    case dict:find(C2#color.bin, ColorDict) of
        {ok, Unspent} -> {ok, C2, Unspent};
        error -> error
    end.

new_spend_color(Unspents) ->
    ColorList = dict:to_list(unspent_color_dict(Unspents)),
    get_spend_color(ColorList).

get_spend_color([]) -> error;
get_spend_color(ColorList) ->
    [{ColorBin, _}|_] = ColorList,
    new(ColorBin).


unspent_to_ic(Unspent) ->
	crypto:hash(ripemd160, crypto:hash(sha256, lib_unspent:script(Unspent))).

script_to_ic(Script) when is_binary(Script) ->
    crypto:hash(ripemd160, crypto:hash(sha256, Script)).

hash160(ColorAtom) when is_atom(ColorAtom) -> ColorAtom;
hash160(Color) when is_list(Color) ->
    color_address(Color);
hash160(Color) when is_binary(Color) ->
	try
        color_address(erlang:binary_to_list(Color))
	catch
		_:_ -> Color
	end;
hash160(Color) when is_record(Color, color) ->
	Color#color.bin;
hash160(Color) -> Color. % Catch all those atoms

string_to_bin(S) when is_atom(S) -> S;
string_to_bin(S) when is_list(S) -> erlang:list_to_binary(S);
string_to_bin(S) -> S.

readable(binary, NetworkParams, Color) -> string_to_bin(readable(NetworkParams, Color)).

readable(_, ?Uncolored) -> uncolored;
readable(_NetworkParams, IssueColor) when is_atom(IssueColor) -> IssueColor;
readable(NetworkParams, IssueColor) when is_record(IssueColor, color) ->
	readable(NetworkParams, IssueColor#color.bin);
readable(NetworkParams, IssueColor) ->
    lib_address:base58_check(maps:get(oa_assetbyte, NetworkParams), IssueColor).

color_address(ColorBin) when is_binary(ColorBin) ->
    color_address(erlang:binary_to_list(ColorBin));
color_address(ColorAddress) when is_list(ColorAddress) ->
    case lib_address:checksum(23, ColorAddress) of
        true -> lib_address:address_to_hash160(ColorAddress);
        false -> 
            % Original Open asset spec
            case lib_address:checksum(5, ColorAddress) of
                true -> lib_address:address_to_hash160(ColorAddress);
                false -> throw(color_address_checksum_error)
            end
    end.

uncolor_all(O) ->  lists:map(fun(R) -> erase_color(R) end, O).

build_color_list(Inputs, UnspentDict) ->
	try
		lists:filtermap(fun(R) -> 
				  N = fetch(bblock:hash(R), bblock:index(R), UnspentDict),
					case lib_unspent:get_attribute(color, N, ?Uncolored) of
						?Uncolored -> false;
						_ -> {true, {lib_unspent:get_attribute(color, N, ?Uncolored),
									 lib_unspent:get_attribute(quantity, N, 0)}}
					end end, Inputs)
	catch E:R ->
			io:format("XXX E: ~p ~n R: ~p~n", [E, R]),
			throw(coloring_error)
	end.

fetch(Hash, Index, UnspentDict) when is_record(UnspentDict, unspentset) ->
    unspentset:lookup(Hash, Index, UnspentDict);
fetch(Hash, Index, UnspentDict) -> dict:fetch({Hash, Index}, UnspentDict).

% Is New Style color address
is_color_address(Address) when is_list(Address) ->
    lib_address:checksum(23, Address).
	

% Generate list of unique colors
colors(Unspents) ->
	sets:to_list(lists:foldl(fun(E, Acc) ->
				sets:add_element(lib_unspent:get_attribute(color, E, ?Uncolored), Acc) end, sets:new(), Unspents)).

readable_colors(NetworkParams, Unspents) ->
	sets:to_list(lists:foldl(fun(E, Acc) ->
									 sets:add_element(readable(NetworkParams,
									 						   lib_unspent:get_attribute(color, E, ?Uncolored)), Acc)
							 end, sets:new(), Unspents)).

readable_issue_colors(NetworkParams, Unspents) ->
	dict:to_list(lists:foldl(fun(E, Acc) ->
									 case lib_unspent:get_attribute(color, E, ?Uncolored) of
									 	 ?Uncolored ->
									 	 	 dict:store(readable(NetworkParams, unspent_to_ic(E)),
									 	 	 			[lib_address:openassets(E),
									 	 	 			 lib_address:readable(NetworkParams, E)], Acc);
									 	 _ -> Acc
									 end end, dict:new(), Unspents)).

% This is so that like colors are stacked
% uncolored outputs feature last in coloring
color_aggregate([]) -> [];
color_aggregate(Payees) when is_list(Payees) ->
	lists:sort(fun(A,B) ->
				A#payee.color >= B#payee.color
				end, Payees).

encode_marker(M, Meta) ->
	OA = erlang:iolist_to_binary([
	<<16#4f:8, 16#41:8, 16#01, 16#00>>,
	lib_parse:int_to_varint(length(M)),
	lists:map(fun(E) -> leb128:encode(E, unsigned) end, M),
				lib_parse:int_to_varint(size(Meta)), Meta]),
	erlang:iolist_to_binary([<<?OP_RETURN>>,
			lib_parse:int_to_pushdata(size(OA)), OA]).


create_marker_output(M) when is_list(M) ->
    BinMarker = encode_marker(M, <<>>),
	validate_marker(BinMarker).

create_marker_output(M, Meta) when is_list(M) ->
    MetaBin = erlang:iolist_to_binary([Meta]),
    BinMarker = encode_marker(M, MetaBin),
    validate_marker(BinMarker).

%% This is ok right now, since transaction building
%% still uses bbdef/btxdef etc
validate_marker(B) when size(B) > 80 ->
    throw(marker_error);
validate_marker(B) ->
	#btxout{value = 0,
		    script = B,
		    info = lib_parse:parse_script(B)}.

marker(P) when is_record(P, payment) ->
	L = lists:takewhile(fun(E) ->
								case lib_unspent:get_attribute(color, E, ?Uncolored) of
									?Uncolored -> false;
									_ -> true
								end end, P#payment.outputs),
	M = lists:map(fun(E) -> lib_unspent:get_attribute(quantity, E, 0) end, L),
	insert_marker(P, create_marker_output(M, P#payment.metaurl));

% Iterate over O until marker is found
% or return error
marker(Btx) when is_record(Btx, btx) or is_record(Btx, btxdef) -> marker(bblock:outputs(Btx));
marker([]) -> error;
marker(O) when is_list(O) ->
	[H|T] = O,
	case bblock:info(H) of
		{openassets, M} -> M;
		_ -> marker(T)
	end.

meta(O) ->
	case marker(O) of
		{_, Meta} -> Meta;
		_ -> error
	end.

meta_url(Outputs) when is_list(Outputs) -> do_meta_url(Outputs);
meta_url(Tx) -> do_meta_url(bblock:outputs(Tx)).

do_meta_url(O) ->
    [H|_] = O,
    % If the metainfo is the first output, then this is a transfer only
    % and we shouldn't return a URL for this asset.
    case meta([H]) of
        error ->
	        case meta(O) of
	        	error -> error;
	        	<<>> -> error;
	        	<<"u=", M/bitstring>> ->
	        		case http_uri:parse(binary_to_list(M)) of
                        {ok, _Result} -> binary_to_list(M);
	        			{error, _} ->
	        				error
	        		end;
	        	_ -> error
	        end;
	    _ -> error
    end.

encode_metaurl(Url) when is_list(Url) ->
    case http_uri:parse(Url) of
        {ok, _Result} -> iolist_to_binary(["u=", Url]);
        {error, _} -> <<>>
    end;

encode_metaurl(_) -> <<>>.

insert_marker(P, M) ->
	{Start, End} = lists:split(P#payment.issuances, P#payment.outputs),
	P#payment{outputs = Start ++ [M] ++ End}.


is_colored(Outputs) when is_list(Outputs) ->
	UncoloredOutputs = lists:takewhile(fun(E) ->
											   case lib_unspent:get_attribute(color, E, ?Uncolored) of
											   	   ?Uncolored -> true;
											   	   _ -> false
											   end end, Outputs),
	if (length(UncoloredOutputs) < length(Outputs)) -> true;
		true -> false
	end.
