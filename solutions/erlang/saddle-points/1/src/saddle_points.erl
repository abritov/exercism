-module(saddle_points).

-export([saddle_points/1, transpose/1, find_column_min_values/1]).

saddle_points([[]]) -> [];

saddle_points([SingleRow]) ->
	Max = lists:last(lists:sort(SingleRow)),
	[{0, Idx} || {Idx, Elem} <- lists:zip(lists:seq(0, length(SingleRow) - 1), SingleRow), Elem =:= Max];

saddle_points([[_SingleColumnElem] | _RestRows] = Matrix) ->
	Column = lists:map(fun ([Elem | _]) -> Elem end, Matrix),
	[Min | _] = lists:sort(Column),
	[{Idx, 0} || {Idx, Elem} <- lists:zip(lists:seq(0, length(Column) - 1), Column), Elem =:= Min];

saddle_points(M) ->
  io:format("Matrix: ~p~n", [M]),
	% M = [[9, 8, 7, 8],
	% 		 [5, 3, 2, 4],
  %      [6, 6, 7, 1]],
	Rows = find_row_max_values2(M),
	Columns = find_column_min_values2(M),
  io:format("Rows   : ~p~n", [Rows]),
  io:format("Columns: ~p~n", [Columns]),
	% [Intersection] = sets:to_list(sets:intersection(sets:from_list(maps:values(Rows)), sets:from_list(maps:values(Columns)))),
	[Intersection] = sets:to_list(sets:intersection(Rows, Columns)),
  io:format("Intersection: ~p~n", [Intersection]),
	{ok, Pos} = find_index(Intersection, M),
	[Pos].

find_index(Elem, Matrix) ->
    case [ {R, C} || 
            {R, Row} <- lists:zip(lists:seq(0, length(Matrix) - 1), Matrix),
            {C, V} <- lists:zip(lists:seq(0, length(Row) - 1), Row),
            V =:= Elem ] of
        [] -> not_found;
        Pos -> {ok, Pos}
    end.

find_row_max_values2(Matrix) ->
	lists:foldl(fun (Row, Acc) -> sets:add_element(lists:last(lists:sort(Row)), Acc) end,
							sets:new(),
							Matrix).

find_column_min_values2(Matrix) ->
	Columns = transpose(Matrix),
  io:format("Columns: ~p~n", [Columns]),
	lists:foldl(fun (Column, Acc) -> sets:add_element(hd(lists:sort(Column)), Acc) end,
							sets:new(),
							Columns).

find_row_max_values(Matrix) ->
	lists:foldl(fun ({Row, I}, Acc) -> maps:put(I, lists:last(lists:sort(Row)), Acc) end,
							#{},
							lists:zip(Matrix, lists:seq(0, length(Matrix) - 1))).

find_column_min_values(Matrix) ->
	Columns = transpose(Matrix),
	lists:foldl(fun ({Column, I}, Acc) -> maps:put(I, hd(lists:sort(Column)), Acc) end,
							#{},
							lists:zip(Columns, lists:seq(0, length(Columns) - 1))).

transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
