-module(all_your_base).

-export([rebase/3]).

rebase(_, InputBase, _) when InputBase < 2 -> {error, "input base must be >= 2" };
rebase(_, _, OutputBase) when OutputBase < 2 -> { error, "output base must be >= 2"};
rebase(Digits, Src, Dest) ->
  case lists:all(fun (X) -> 0 =< X andalso X < Src end, Digits) of
    true ->
      Dec = lists:foldl(fun (X, Acc) -> X + Acc * Src end, 0, Digits),
      {ok, dec2base(Dec, Dest, [])};
    false ->
      {error, "all digits must satisfy 0 <= d < input base"}
  end.

dec2base(0, _, []) -> [0];
dec2base(0, _, Acc) -> Acc;
dec2base(Value, Base, Acc) ->
  dec2base(Value div Base, Base, [ Value rem Base | Acc ]).
