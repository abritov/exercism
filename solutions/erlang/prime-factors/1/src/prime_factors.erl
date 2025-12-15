-module(prime_factors).

-export([factors/1]).


factors(1) -> [];

factors(Value) -> find_factors(Value, 2, []).

find_factors(1, _CurrentFactor, Acc) ->
  lists:reverse(Acc);

find_factors(Value, CurrentFactor, Acc) ->
  case Value rem CurrentFactor of
    0 ->
      find_factors(Value div CurrentFactor, CurrentFactor, [CurrentFactor | Acc]);
    _ ->
      find_factors(Value, CurrentFactor + 1, Acc)
  end.
