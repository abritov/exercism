-module(change).

-export([find_fewest_coins/2]).


find_fewest_coins(Target, _Coins) when Target < 0 -> erlang:error(badarg);
find_fewest_coins(Target, _Coins) when Target == 0 -> [];
find_fewest_coins(Target, Coins) ->
  % io:format("Result ~w~n", [step(Target, Coins, [])]).
  step(Target, Coins, []).

step(0, _Coins, Result) -> Result;
step(Target, Coins, Result) ->
  io:format("LessThanTarget ~p~n", [lists:filter(fun (Coin) -> Target >= Coin end, Coins)]),
  case lists:filter(fun (Coin) -> Target >= Coin end, Coins) of
    [] when Target == 0 ->
      Result;
    [] ->
      undefined;
    LessThanTarget -> 
      Last = lists:last(LessThanTarget),
      Count = Target div Last,
      Repeat = [Last || _ <- lists:seq(1, Count)],
      step(Target rem Last, LessThanTarget, lists:concat([Repeat, Result]))
  end.
