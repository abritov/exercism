-module(roman_numerals).

-export([roman/1]).


roman(N) -> lists:concat(lists:reverse(convert(N, []))).

convert(1, Acc) -> ["I" | Acc];
convert(2, Acc) -> ["II" | Acc];
convert(3, Acc) -> ["III" | Acc];

convert(N, Acc) ->
  if 
    N div 1000 > 0 ->
      convert(N - 1000, ["M" | Acc]);
    N div 900 == 1 ->
      convert(N - 900, ["CM" | Acc]);
    N div 500 == 1 ->
      convert(N - 500, ["D" | Acc]);
    N div 400 == 1 ->
      convert(N - 400, ["CD" | Acc]);
    N div 100 == 1 ->
      convert(N - 100, ["C" | Acc]);
    N div 90 == 1 ->
      convert(N - 90, ["XC" | Acc]);
    N div 50 == 1 ->
      convert(N - 50, ["L" | Acc]);
    N div 40 == 1 ->
      convert(N - 40, ["XL" | Acc]);
    N div 10 > 0 ->
      convert(N - 10, ["X" | Acc]);
    N div 9 == 1 ->
      convert(N - 9, ["IX" | Acc]);
    N div 5 == 1 ->
      convert(N - 5, ["V" | Acc]);
    N div 4 == 1 ->
      convert(N - 4, ["IV" | Acc]);
    true ->
      io:format("DEBUG: ~p~n", [N]),
      Acc
  end.
