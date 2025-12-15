-module(sieve).

-export([primes/1]).


primes(Limit) -> primes(lists:seq(2, Limit), []).

primes([], Primes) -> lists:reverse(Primes);
primes([First|Rest], Primes) ->
  primes([It || It <- Rest, It rem First =/= 0], [First|Primes]).
