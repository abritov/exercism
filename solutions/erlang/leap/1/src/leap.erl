-module(leap).

-export([leap_year/1, divisible_by/2]).

divisible_by(Num, Divider) ->
    if Num rem Divider == 0 ->
            true;
        true ->
            false
    end.

leap_year(Year) -> divisible_by(Year, 400) orelse (divisible_by(Year, 4) and not((divisible_by(Year, 100)))).
