-module(yacht).

-export([score/2]).

inc(N) -> N + 1.

all_same([]) -> true;
all_same([H|T]) -> lists:all(fun(X) -> X =:= H end, T).

get_key_by_value(Map, Value) ->
    maps:fold(fun(K, V, Acc) ->
        case V =:= Value of
            true -> K;
            false -> Acc
        end
    end, not_found, Map).

get_frequences(List) ->
  lists:foldl(fun (Elem, Acc) when is_map_key(Elem, Acc) -> maps:update_with(Elem, fun inc/1, Acc);
                  (Elem, Acc) -> maps:put(Elem, 1, Acc) end, #{}, List).

dice_sum(Dice, Dices) ->
  lists:foldl(fun (Elem, Acc) when Elem == Dice -> Acc + Elem;
                  (_Elem, Acc) -> Acc end, 0, Dices).

sum_all(Dices) ->
  lists:foldl(fun (Elem, Acc) -> Acc + Elem end, 0, Dices).

score(Dices, ones) ->
  dice_sum(1, Dices);

score(Dices, twos) ->
  dice_sum(2, Dices);

score(Dices, threes) ->
  dice_sum(3, Dices);

score(Dices, fours) ->
  dice_sum(4, Dices);

score(Dices, fives) ->
  dice_sum(5, Dices);

score(Dices, sixes) ->
  dice_sum(6, Dices);

score(Dices, full_house) ->
  Freqs = lists:sort(maps:values(get_frequences(Dices))),
  case Freqs of
    [2, 3] ->
      sum_all(Dices);
    _ ->
      0
  end;

score([FirstDice | Rest] = Dices, four_of_a_kind) ->
  Freqs = get_frequences(Dices),
  io:format("Freqs: ~p~n", [Freqs]),
  case lists:sort(maps:values(Freqs)) of
    [1, 4] ->
      Dice = get_key_by_value(Freqs, 4),
      dice_sum(Dice, Dices);
    [5] ->
      dice_sum(FirstDice, Rest);
    _ ->
      0
  end;

score(Dices, little_straight) ->
  case lists:sort(Dices) of
    [1, 2, 3, 4, 5] ->
      30;
    _ ->
      0
  end;

score(Dices, big_straight) ->
  case lists:sort(Dices) of
    [2, 3, 4, 5, 6] ->
      30;
    _ ->
      0
  end;

score(Dices, choice) ->
  sum_all(Dices);

score(Dices, yacht) ->
  case all_same(Dices) of
    true ->
      50;
    false ->
      0
  end;

score(_Dices, _Category) ->
  0.
