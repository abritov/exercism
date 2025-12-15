-module(strain).

-export([keep/2, discard/2]).

keep(Fn, List) ->
  discard(fun (Elem) -> not Fn(Elem) end, List).

discard(Fn, List) ->
  do_discard([], Fn, List).

do_discard(Acc, _Fn, []) ->
  lists:reverse(Acc);

do_discard(Acc, Fn, [Head | Tail]) ->
  Discard = Fn(Head),
  if
    Discard -> do_discard(Acc, Fn, Tail);
    true -> do_discard([Head | Acc], Fn, Tail)
  end.

