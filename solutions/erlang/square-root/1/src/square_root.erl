-module(square_root).

-export([square_root/1]).

isqrt(Y) ->
  isqrt(0, Y).

isqrt(L, Y) when (L + 1) * (L + 1) =< Y ->
  isqrt(L + 1, Y);
isqrt(L, _Y) ->
  L.

square_root(Radicand) -> isqrt(Radicand).
