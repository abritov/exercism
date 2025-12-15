-module(hamming).

-export([distance/2]).


distance(S1, S2) when length(S1) =/= length(S2) -> {error, badarg};

distance(S1, S2) -> dist(0, S1, S2).

dist(Acc, [], []) ->
  Acc;
dist(Acc, [S1 | S1Rest], [S2 | S2Rest]) ->
  if
    S1 == S2 ->
      dist(Acc, S1Rest, S2Rest);
    true ->
      dist(Acc + 1, S1Rest, S2Rest)
  end.
