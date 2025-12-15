-module(change).
-export([find_fewest_coins/2]).

find_fewest_coins(Target, _Coins) when Target < 0 -> 
    error(badarg);
find_fewest_coins(0, _Coins) -> 
    [];
find_fewest_coins(Target, Coins) ->
    DP = build_dp(Target, Coins),
    case maps:find(Target, DP) of
        {ok, {_Count, _Coin, _Prev}} -> 
            backtrack(Target, DP, []);
        error -> 
            undefined
    end.

build_dp(Target, Coins) ->
    build_dp(1, Target, Coins, maps:put(0, {0, none, none}, #{})).

build_dp(Amount, Target, _Coins, DP) when Amount > Target -> 
    DP;
build_dp(Amount, Target, Coins, DP) ->
    Best = lists:foldl(
        fun(Coin, BestSoFar) ->
            case Amount >= Coin of
                true ->
                    Remain = Amount - Coin,
                    case maps:find(Remain, DP) of
                        {ok, {Count, _, _}} ->
                            NewCount = Count + 1,
                            Candidate = {NewCount, Coin, Remain},
                            case BestSoFar of
                                undefined -> Candidate;
                                {OldCount, _, _} when NewCount < OldCount -> 
                                    Candidate;
                                _ -> BestSoFar
                            end;
                        error -> 
                            BestSoFar
                    end;
                false -> 
                    BestSoFar
            end
        end,
        undefined,
        Coins
    ),
    NewDP = case Best of
        undefined -> DP;
        _ -> maps:put(Amount, Best, DP)
    end,
    build_dp(Amount + 1, Target, Coins, NewDP).

backtrack(0, _DP, Acc) -> 
    lists:sort(Acc);
backtrack(Amount, DP, Acc) ->
    {_, Coin, Prev} = maps:get(Amount, DP),
    backtrack(Prev, DP, [Coin | Acc]).
