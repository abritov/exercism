-module(pascals_triangle).
-export([rows/1, sum/2]).

rows(0) -> [];
rows(Count) -> make_rows(Count, []).

make_rows(I, Result) when length(Result) == 0, I > 0 ->
  make_rows(I - 1, [[1]]);

make_rows(I, Result) when length(Result) == 1, I > 0 ->
  make_rows(I - 1, Result ++ [[1, 1]]);

make_rows(I, Result) when I > 0 ->
  Prev = lists:last(Result),
  Sum = sum(Prev, []),
  make_rows(I - 1, Result ++ [[1 | Sum] ++ [1]]);

make_rows(0, Result) -> Result.

sum([A, B], Result) ->
  [A + B | Result];

sum([A], Result) ->
  Prev = lists:last(Result),
  [A + Prev | Result];

sum([A, B | Rest], Result) ->
  sum([B | Rest], [A + B | Result]).

