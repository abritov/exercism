-module(difference_of_squares).

-export([difference_of_squares/1, square_of_sum/1, sum_of_squares/1]).


difference_of_squares(N) ->
  square_of_sum(N) - sum_of_squares(N).

square_of_sum(N) ->
    Sum = N * (N + 1) div 2,
    Sum * Sum.

sum_of_squares(N) when is_integer(N), N >= 1 ->
    N * (N + 1) * (2 * N + 1) div 6.
