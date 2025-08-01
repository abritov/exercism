-module(palindrome_products).

-export([largest/2, smallest/2, dedupe/1]).


largest(X, X) -> undefined;

largest(Min, Max) ->
  Step = get_step(Min, Max),
  Products = [{X * Y, {X, Y}} || X <- lists:seq(Max - Step, Max), Y <- lists:seq(Max - Step, Max), is_palindrome(X * Y)],
  Uniq = maps:groups_from_list(fun ({Product, _}) -> Product end, fun ({_, Factor}) -> Factor end, Products),
  dedupe(lists:last(lists:sort(maps:to_list(Uniq)))).

smallest(Min, Max) ->
  Step = get_step(Min, Max),
  Products = [{X * Y, {X, Y}} || X <- lists:seq(Min, Min + Step), Y <- lists:seq(Min, Min + Step), is_palindrome(X * Y)],
  Uniq = maps:groups_from_list(fun ({Product, _}) -> Product end, fun ({_, Factor}) -> Factor end, Products),
  case lists:sort(maps:to_list(Uniq)) of
    [Some | _] ->
      dedupe(Some);
    [] ->
      undefined
  end.

dedupe({Product, Factors}) when length(Factors) == 1 ->
  {Product, Factors};

dedupe({Product, Factors}) ->
  io:format("dedupe ~p ~p~n", [Product, Factors]),
  Uniq = lists:foldr(fun ({A, A}, Acc) ->
                         Acc;
                         ({A, B} = Elem, Acc) ->
                         case lists:member({B, A}, Acc) of
                           true ->
                             Acc -- [Elem];
                           false ->
                             Acc
                         end
                     end, Factors, Factors),
  {Product, Uniq}.

get_step(Min, Max) ->  
    if 
        Max - Min < 10 -> Max - Min;
        true -> 100
    end.

is_palindrome(X) when is_integer(X), X < 10 ->
  true;

is_palindrome(X) when not is_list(X) ->
  is_palindrome(integer_to_list(X));

is_palindrome([First | Rest]) when length(Rest) > 0 ->
  case First == lists:last(Rest) of
    true ->
      is_palindrome(lists:droplast(Rest));
    false ->
      false
  end;

is_palindrome([_]) -> true;

is_palindrome([]) -> true.
