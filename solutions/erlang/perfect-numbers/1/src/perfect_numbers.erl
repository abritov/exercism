-module(perfect_numbers).

-export([classify/1]).


classify(N) when N =< 0 -> erlang:error("invalid number");

classify(Number) ->
  Factors = find_factors(Number),
  case lists:sum(Factors) of
    Sum when Sum == Number ->
      perfect;
    Sum when Sum > Number ->
      abundant;
    Sum when Sum < Number ->
      deficient;
    _ ->
      unknown
  end.


find_factors(N) ->
  find_factors(N, 1, []).

find_factors(N, Current, Result) when N > Current ->
  if N rem Current == 0 ->
       find_factors(N, Current + 1, [Current | Result]);
     true ->
       find_factors(N, Current + 1, Result)
  end;

find_factors(_N, _Current, Result) ->
  Result.
