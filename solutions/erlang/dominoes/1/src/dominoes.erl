-module(dominoes).

-export([can_chain/1, to_map/1, is_valid/1]).


can_chain([]) -> true;
can_chain([{Same, Same}]) -> true;

can_chain(D) ->
  [First | Rest] = lists:sort(D),
  Map = to_map(Rest),
  io:format("map ~p~n", [Map]),
  Reversed = to_map_reversed(Rest),
  io:format("map_reversed ~p~n", [Reversed]),
  case make_chain(First, Map, []) of
    false ->
      false;
    Solution ->
      is_valid(Solution)
  end.

% dominoes:can_chain([{1, 2}, {2, 3}, {3, 1}, {1, 1}, {2, 2}, {3, 3}]).
% [1, 1] [1, 2] [2, 3] [3, 1] - wrong
% [{1, 1}, {1, 2}, {2, 2}, {2, 3}, {3, 3}, {3, 1}] 
make_chain({_, Second} = D, Map, []) ->
  io:format("init ~p ~p ~n", [D, Map]),
  case maps:find(Second, Map) of
    {ok, [Found]} ->
      io:format("init found next elem ~p ~p ~n", [Found, Map]),
      make_chain(D, Map, chain_size(Map), [D]);
    {ok, [First, _] = Found} when is_list(Found)  ->
      io:format("init found multiple elems ~p ~p ~n", [Found, Map]),
      % how to choose correct next chain here?
      % find_branch_solution(Found, Map, chain_size(Map) - 1, [D]);
      make_chain(D, Map, chain_size(Map), [D]);
    error ->
      io:format("init doesn't found next elem ~p ~p ~n", [D, Map]),
      false
  end.

make_chain(_, Map, ChainSize, Result) when ChainSize == 0 ->
  io:format("finish(~p) ~p ~p ~n", [ChainSize, Map, Result]),
  Result;

make_chain({_, Second} = D, Map, ChainSize, Result) ->
  CurrentChainSize = chain_size(Map) - 1,
  io:format("make_chain(~p/~p) for ~p ~p ~p~n", [CurrentChainSize, ChainSize, D, Map, Result]),
  case maps:find(Second, Map) of
    {ok, [Found]} ->
      io:format("found next chain ~p~n", [Found]),
      make_chain(Found, maps:remove(Second, Map), ChainSize, Result ++ [Found]);
    {ok, [Try | _] = FoundList} when is_list(FoundList) ->
      io:format("checking nested solution ~p~n", [FoundList]),
      % make_chain(Found, maps:update_with(Second, fun (List) -> lists:delete(, Result ++ [Found]);
      NewMap = maps:update(Second, [Try], Map),
      BranchSolution = find_branch_solution(FoundList, NewMap, chain_size(NewMap) - 1, Result),
      io:format("find_branch_solution ~p ~p~n", [D, BranchSolution]),
      io:format("NewMap ~p~n", [NewMap]),
      case BranchSolution of
        not_found ->
          not_found;
        {ok, Next, NewResult, undefined} ->
          io:format("NewMap1 is undefined~n"),
          make_chain(Next, NewMap, ChainSize, NewResult);
        {ok, Next, NewResult, NewMap1} ->
          io:format("NewMap1 ~p~n", [NewMap1]),
          make_chain(Next, NewMap1, ChainSize, NewResult)
      end;

      % case make_chain(D, NewMap, chain_size(NewMap) - 1, Result) of
      %   false ->
      %     io:format("branch checking failed"),
      %     false;
      %   Nested when length(Nested) == ChainSize ->
      %     io:format("solution found ~p~n", [Nested]),
      %     Result;
      %   _ ->
      %     io:format("solution ~p is not valid, checking ~p, variants ~p ~n", [Try, Rest, Map]),
      %     RestMap = maps:update(Second, Rest, Map),
      %     make_chain(D, RestMap, chain_size(RestMap) - 1, Result)
      % end;
    error when ChainSize > CurrentChainSize ->
      io:format("backtrack ~p~n", [Result]),
      % make_chain(Found, maps:remove(Second, Map), ChainSize, Result ++ [Found]);
      Result;
    error ->
      io:format("can't continue~n"),
      false
  end.

find_branch_solution([D | Solutions] = All, Map, ChainSize, Result) ->
  io:format("find_branch_solution(~p/~p) ~p ~p ~p~n", [chain_size(Map) - 1, ChainSize, All, Map, Result]),
  case make_chain(D, Map, chain_size(Map) - 1, Result) of
    false ->
      io:format("branch checking failed~n"),
      % find_branch_solution(Solutions, Map, ChainSize, Result);
      not_found;
    Nested when length(Nested) == ChainSize ->
      io:format("solution found ~p is_valid ~p~n", [Nested, is_valid(Nested)]),
      io:format("context ~p ~p ~p ~p ~p~n", [D, Solutions, Map, ChainSize, Result]),
      io:format("1 ChainSize ~p RestMapSize ~p~n", [ChainSize, chain_size(to_map(Solutions)) - 1]),
      case chain_size(to_map(Solutions)) - 1 of
        0 ->
          {ok, D, Nested, undefined};
        _ ->
          {ok, D, Nested, Map}
      end;
    not_found ->
      io:format("solution not found~n"),
      {First, _} = D,
      RestMap = maps:update(First, Solutions, Map),
      io:format("2 ChainSize ~p RestMapSize ~p~n", [ChainSize, chain_size(RestMap) - 1]),
      find_branch_solution(Solutions, RestMap, chain_size(RestMap) - 1, Result);
    Nested ->
      case is_valid(Nested) of
        true ->
          io:format("solution is valid ~p ~n", [Nested]),
          io:format("3 ChainSize ~p RestMapSize ~p~n", [ChainSize, chain_size(to_map(Solutions)) - 1]),
          {ok, D, Nested, to_map(Solutions)};
        false ->
          io:format("solution check failed for ~p ~p~n", [Nested, is_valid(Nested)]),
          io:format("solution ~p is not valid, checking ~p, variants ~p ~n", [D, Solutions, Map]),
          {First, _} = D,
          RestMap = maps:update(First, Solutions, Map),
          io:format("4 ChainSize ~p RestMapSize ~p~n", [ChainSize, chain_size(RestMap) - 1]),
          find_branch_solution(Solutions, RestMap, chain_size(RestMap) - 1, Result)
      end
  end.

chain_size(Map) ->
  maps:fold(fun (_, Val, Acc) -> length(Val) + Acc end, 1, Map).

is_valid([]) -> true;
is_valid(not_found) -> false;
is_valid([{Same, Same}]) -> true;

is_valid([{_, Second} | Rest]) ->
  case Rest of
    [{Second, _} | _] ->
      is_valid(Rest);
    [] ->
      true;
    Other ->
      io:format("is not valid ~p ~p~n", [Other, Rest]),
      false
  end.


to_map(List) ->
  lists:foldl(fun ({First, _} = Elem, Acc) ->
                  maps:update_with(First, fun (Old) -> [Elem | Old] end, [Elem], Acc) end,
              #{}, List).

to_map_reversed(List) ->
  lists:foldl(fun ({_, Second} = Elem, Acc) -> maps:put(Second, Elem, Acc) end, #{}, List).
