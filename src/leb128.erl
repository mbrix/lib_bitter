-module(leb128).
-export([encode/2, decode/2]).
 
encode(Value, signed) ->
    encode_ranged(Value, 0, 16#40);
 
encode(Value, unsigned) when Value >= 0 ->
    encode_ranged(Value, 0, 16#80).
 
 
 
decode(Bin, signed) ->
    {Size, BinValue, Tail} = take_chunks(Bin),
    <<Value:Size/signed-integer>> = BinValue,
    {Value, Tail};
 
decode(Bin, unsigned) ->
    {Size, BinValue, Tail} = take_chunks(Bin),
    <<Value:Size/unsigned-integer>> = BinValue,
    {Value, Tail}.
 
 
 
encode_ranged(Value, Shift, Range) when -Range =< Value andalso Value < Range ->
    Chunk = Value bsr Shift,
    <<0:1, Chunk:7/integer>>;
 
encode_ranged(Value, Shift, Range) ->
    Chunk = Value bsr Shift,
    Tail = encode_ranged(Value, Shift+7, Range bsl 7),
    <<1:1, Chunk:7/integer, Tail/binary>>.
 
 
take_chunks(<<0:1, Chunk:7/bitstring, Tail/bitstring>>) ->
    {7, Chunk, Tail};
 
take_chunks(<<1:1, Chunk:7/bitstring, Tail/bitstring>>) ->
    {Size2, Chunks2, Tail2} = take_chunks(Tail),
    {Size2+7, <<Chunks2/bitstring, Chunk/bitstring>>, Tail2}.
