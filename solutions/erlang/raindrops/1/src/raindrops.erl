-module(raindrops).

-export([convert/1]).


add_if_divisible(N, 3) when N rem 3 == 0 -> "Pling";
add_if_divisible(_N, 3) -> "".

add_if_divisible(Acc, N, 5) when N rem 5 == 0 -> Acc ++ "Plang";
add_if_divisible(Acc, _N, 5) -> Acc ++ "";

add_if_divisible(Acc, N, 7) when N rem 7 == 0 -> Acc ++ "Plong";
add_if_divisible(Acc, _N, 7) -> Acc ++ "".

convert(N) ->
  Result = add_if_divisible(N, 3),
  Result1 = add_if_divisible(Result, N, 5),
  Result2 = add_if_divisible(Result1, N, 7),
  if
    Result2 == "" ->
      integer_to_list(N);
    true ->
      Result2
  end.
