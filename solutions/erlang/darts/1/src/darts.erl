-module(darts).

-export([score/2, radius_distance/2]).

radius_distance(X, Y) ->
    math:sqrt(X * X + Y * Y).

score(X, Y) ->
    Dist = radius_distance(X, Y),
    if
        Dist =< 1 ->
            10;
        Dist =< 5 ->
            5;
        Dist =< 10 ->
            1;
        true ->
            0
    end.
