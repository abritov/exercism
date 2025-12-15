-module(palindrome_products).

-export([largest/2, smallest/2, products/2, is_palindrome/1, normalize/1, dedupe/1]).


largest(Min, Max) ->
  dedupe(lists:last(lists:sort(maps:to_list(maps:filter(fun (Product, _Factors) -> is_palindrome(Product) end, products(Min, Max)))))).

smallest(Min, Max) ->
  dedupe(hd(lists:sort(maps:to_list(maps:filter(fun (Product, _Factors) -> is_palindrome(Product) end, products(Min, Max)))))).

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

is_palindrome(X) when is_integer(X), X < 10 ->
  true;

is_palindrome(X) when not is_list(X) ->
  % io:format("is_palindrome ~p ~n", [X]),
  is_palindrome(integer_to_list(X));

is_palindrome([First | Rest]) when length(Rest) > 0 ->
  % io:format("~p ~p~n", [First, Rest]),
  case First == lists:last(Rest) of
    true ->
      is_palindrome(lists:droplast(Rest));
    false ->
      false
  end;

is_palindrome([_]) -> true;

is_palindrome([]) -> true.

products(Start, Stop) ->
  Products = [{X * Y, {X, Y}} || X <- lists:seq(Start, Stop), Y <- lists:seq(Start, Stop)],
  maps:groups_from_list(fun ({Product, _}) -> Product end, fun ({_, Factor}) -> Factor end, Products).

normalize({V, F}) ->
    {
        V,
        lists:sort(
            lists:map(
                fun
                    ({A, B}) when A>B -> {B, A};
                    (AB) -> AB
                end,
                F
            )
        )
    }.
