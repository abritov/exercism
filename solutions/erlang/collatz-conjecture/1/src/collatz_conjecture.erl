-module(collatz_conjecture).

-export([steps/1]).

step(Acc, 1) ->
    Acc;

step(Acc, N) ->
    if
        N rem 2 == 0 ->
            step(Acc + 1, N div 2);
        true ->
            step(Acc + 1, N * 3 + 1)
    end.

steps(N) -> step(0, N).
