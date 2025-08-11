-module(sublist).

-export([is_equal/2, is_sublist/2, is_superlist/2, is_unequal/2, relation/2]).


is_equal(L1, L2) -> L1 == L2.

is_sublist([], []) -> true;
is_sublist([], _) -> true;
is_sublist(_, []) -> false;

is_sublist([F1 | _] = L1, [_ | R2] = L2) ->
  case lists:dropwhile(fun (El) -> El /= F1 end, L2) of
    Found when length(Found) >= length(L1) ->
      case lists:sublist(Found, 1, length(L1)) of
        Sublist when Sublist == L1 ->
          true;
        _ ->
          is_sublist(L1, R2)
      end;
    _ ->
      false
  end;

is_sublist(_, _) -> false.

is_superlist(L1, L2) -> is_sublist(L2, L1).

is_unequal(L1, L2) -> L1 /= L2.

relation(L1, L2) when L1 == L2 -> equal;
relation(_, []) -> superlist;

relation(L1, L2) ->
  case is_sublist(L1, L2) of
    true ->
      sublist;
    false ->
      case is_superlist(L1, L2) of
        true ->
          superlist;
        false ->
          unequal
      end
  end.
