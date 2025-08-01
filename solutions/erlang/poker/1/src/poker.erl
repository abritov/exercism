-module(poker).

-export([best_hands/1, suit_rank/1, is_tie/1, check_pair/1]).


best_hands([Hand]) -> [Hand];

best_hands([_, _ | []] = Hands) ->
  case check_pair(Hands) of
    equal ->
      case is_tie(Hands) of
        [] ->
          [{_, Hand} | _] = sort_by_max_rank(Hands),
          io:format("winner by max rank ~p~n", [Hand]),
          [Hand];
        Tie ->
          Tie
      end;
    WinnerPair ->
      [WinnerPair]
  end;

best_hands(Hands) -> 
  case is_tie(Hands) of
    [] ->
      [{_, Hand} | _] = sort_by_max_rank(Hands),
      io:format("winner by max rank ~p~n", [Hand]),
      [Hand];
    Tie ->
      Tie
  end.

% poker:best_hands(["4S 5H 6C 8D KH", "2S 4H 6S 4D JH"]).
check_pair(List) when length(List) == 2 ->
  [First, Second | []] = lists:map(fun (Hand) -> maps:groups_from_list(fun suit_rank/1, string:split(Hand, " ", all)) end, List),
  io:format("check pair ~p ~n           ~p~n", [First, Second]),
  {A, MaxA, RestA} = maps:fold(fun get_highest_pair/3, {0, 0, First}, First),
  {B, MaxB, RestB} = maps:fold(fun get_highest_pair/3, {0, 0, Second}, Second),
  io:format("A ~p, B ~p~n", [A, B]),
  io:format("max pair rank(~p, ~p)~n", [MaxA, MaxB]),
  if A == B ->
       io:format("max pair rank(~p, ~p)~n", [MaxA, MaxB]),
       if MaxA == MaxB ->
            io:format("pairs (~p, ~p) are equal, checking kicker~n", [A, B]),
            io:format("~p~n", [RestA]),
            io:format("~p~n", [RestB]),
            case {maps:size(RestA), maps:size(RestB)} of
              {1, 1} ->
                case {maps:values(RestA), maps:values(RestB)} of
                  {AA, BB} when AA > BB ->
                    hd(List);
                  {_, _} ->
                    lists:last(List)
                end;
              _ ->
                io:format("Cannot check kicker"),
                equal
            end;
          MaxA > MaxB ->
            hd(List);
          true ->
            lists:last(List)
       end;
     A > B ->
       hd(List);
     true ->
       lists:last(List)
  end.

get_highest_pair(PairRank, Cards, {Acc, Max, Rest}) when length(Cards) == 2, PairRank > Max ->
  {PairRank + Acc, PairRank, maps:remove(PairRank, Rest)};
get_highest_pair(PairRank, Cards, {Acc, Max, Rest}) when length(Cards) == 2 ->
  {PairRank + Acc, Max, maps:remove(PairRank, Rest)};
get_highest_pair(_PairRank, _Cards, Acc) ->
  Acc.

is_tie(Hands) ->
  Ranks = lists:map(fun (Hand) ->
                            {lists:sum(maps:keys(maps:groups_from_list(fun suit_rank/1, string:split(Hand, " ", all)))), Hand}
                        end, Hands),
  Groups = maps:groups_from_list(fun ({Sum, _}) -> Sum end, fun ({_, Hand}) -> Hand end, Ranks),
  case maps:values(maps:filter(fun (_, H) -> length(H) > 1 end, Groups)) of
    [First | _] = Tie when length(Tie) == 1, is_list(hd(Tie)), length(First) > 1 ->
      io:format("Tie ~p~n", [Tie]),
      First;
    _ ->
      []
  end.


sort_by_max_rank(Hands) ->
  MaxHands = lists:map(fun (Hand) ->
                           Cards = string:split(Hand, " ", all),
                           CardsRanked = lists:map(fun (Card) -> {suit_rank(Card), Card} end, Cards),
                           Sorted = lists:sort(fun ({A, _}, {B, _}) -> A > B end, CardsRanked),
                           {Sorted, Hand}
                       end, Hands),
  io:format("MaxHands ~p~n", [MaxHands]),
  lists:sort(fun ({A, _}, {B, _}) -> A > B end, MaxHands).

suit_rank(Card) when is_list(Card) ->
  case string:to_integer(Card) of
    {error, no_integer} ->
      [Rank | [_Suit]] = Card,
      suit_rank(Rank);
    {Rank, _Suit} ->
      Rank
  end;

suit_rank($J) -> 11;
suit_rank($Q) -> 12;
suit_rank($K) -> 13;
suit_rank($A) -> 14.
