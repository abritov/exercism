-module(armstrong_numbers).

-export([is_armstrong_number/1]).

is_armstrong_number(N) ->
  List = integer_to_list(N),
  Len = length(List),
  Sum = lists:foldl(fun (Char, Acc) -> trunc(math:pow(Char - 48, Len)) + Acc end, 0, List),
  Sum == N.
