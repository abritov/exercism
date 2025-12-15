-module(sum_of_multiples).

-export([sum/2]).


sum([], _Limit) -> 0;
sum(Factors, Limit) ->
  Multipliers = lists:foldl(fun (F, Acc) ->
                                lists:concat([find_multiples(F, Limit), Acc])
                            end, [], Factors),
  lists:sum(sets:to_list(sets:from_list(Multipliers))).

find_multiples(0, _Limit) -> [0];
find_multiples(Factor, Limit) ->
  find_multiples(Factor, Limit, 1, []).


find_multiples(Factor, Limit, Multiplier, Result) ->
  case Factor * Multiplier of
    Continue when Continue < Limit ->
      find_multiples(Factor, Limit, Multiplier + 1, [Continue | Result]);
    _Stop ->
      Result
  end.
