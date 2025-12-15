-module(triangle).

-export([kind/3, valid/4]).

kind(0, 0, 0) ->
  {error, "all side lengths must be positive"};

kind(X, X, X) -> equilateral;

kind(X, X, Y) ->
  valid(X, X, Y, isosceles);
kind(X, Y, X) ->
  valid(X, Y, X, isosceles);
kind(Y, X, X) ->
  valid(Y, X, X, isosceles);

kind(A, B, C) ->
  valid(A, B, C, scalene).

valid(A, B, C, Type) ->
  if
    A + B >= C andalso B + C >= A andalso A + C >= B ->
      Type;
    true ->
      {error, "side lengths violate triangle inequality"}
  end.
