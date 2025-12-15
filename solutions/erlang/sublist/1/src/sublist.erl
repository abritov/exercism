-module(sublist).

-export([is_equal/2, is_sublist/2, is_superlist/2, is_unequal/2, relation/2]).


is_equal(L1, L2) -> L1 == L2.

is_sublist([], []) -> true;
is_sublist([], _) -> true;
is_sublist(_, []) -> false;

is_sublist([F1 | _] = L1, [_ | _] = L2) ->
  timer:sleep(500),
  io:format("L1 ~p, L2 ~p~n", [L1, L2]),
  case lists:dropwhile(fun (El) -> El /= F1 end, L2) of
    [] ->
      false;
    Found ->
      io:format("Found ~p~n", [Found]),
      case lists:sublist(Found, 1, length(L1)) of
        Sublist when Sublist == L1 ->
          io:format("Sublist ~p~n", [Sublist]),
          true;
        _ ->
          is_sublist(L1, Found)
      end
  end;

is_sublist(_, _) -> false.

is_superlist([], []) -> true;
is_superlist([], _) -> false;
is_superlist(L1, L2) -> is_sublist(L2, L1).

is_unequal(L1, L2) -> L1 /= L2.

relation(L1, L2) when L1 == L2 -> equal;
relation(_, []) -> superlist;

relation(L1, L2) ->
  case is_sublist(L1, L2) of
    true ->
      sublist;
    false ->
      io:format("is not sublist, trying is_superlist~n"),
      case is_superlist(L1, L2) of
        true ->
          superlist;
        false ->
          unequal
      end
  end.
