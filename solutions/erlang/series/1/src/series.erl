-module(series).

-export([slices/2]).

slices(0, _Str) ->
  error(bararg);

slices(Length, _Str) when Length < 0 ->
  error(bararg);

slices(Length, Str) when Length > length(Str) ->
  error(bararg);

slices(Length, Str) ->
  lists:takewhile(fun (SubStr) -> length(SubStr) >= Length end, [string:slice(Str, X, Length) || X <- lists:seq(0, string:len(Str))]).
