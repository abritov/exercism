-module(saddle_points).

-export([saddle_points/1]).

saddle_points([[]]) -> [];

saddle_points([SingleRow]) ->
	Max = lists:last(lists:sort(SingleRow)),
	[{0, Idx} || {Idx, Elem} <- lists:zip(lists:seq(0, length(SingleRow) - 1), SingleRow), Elem =:= Max];

saddle_points([[_SingleColumnElem] | _RestRows] = Matrix) ->
	Column = lists:map(fun ([Elem | _]) -> Elem end, Matrix),
	[Min | _] = lists:sort(Column),
	[{Idx, 0} || {Idx, Elem} <- lists:zip(lists:seq(0, length(Column) - 1), Column), Elem =:= Min];

saddle_points(M) ->
	Transposed = transpose(M),
	Rows = find_row_max_values(M),
	Columns = find_column_min_values(Transposed),
	
	case sets:to_list(sets:intersection(Rows, Columns)) of
		[Intersection] -> 
			{ok, Pos} = find_indexes(Intersection, M),
			lists:filter(fun (P) -> is_saddle_point(P, M, Transposed) end, Pos);
		_ ->
			[]
	end.

is_saddle_point({RowIndex, ColumnIndex}, Matrix, Transposed) ->
	Row = lists:nth(RowIndex + 1, Matrix),
	Column = lists:nth(ColumnIndex + 1, Transposed),
	Value = lists:nth(ColumnIndex + 1, Row),
	lists:max(Row) == Value andalso lists:min(Column) == Value.

find_indexes(Elem, Matrix) ->
    case [ {R, C} || 
            {R, Row} <- lists:zip(lists:seq(0, length(Matrix) - 1), Matrix),
            {C, V} <- lists:zip(lists:seq(0, length(Row) - 1), Row),
            V =:= Elem ] of
        [] -> not_found;
        Pos -> {ok, Pos}
    end.

find_row_max_values(Matrix) ->
	lists:foldl(fun (Row, Acc) -> sets:add_element(lists:max(Row), Acc) end,
							sets:new(),
							Matrix).

find_column_min_values(TransposedMatrix) ->
	lists:foldl(fun (Column, Acc) -> sets:add_element(lists:min(Column), Acc) end,
							sets:new(),
							TransposedMatrix).

transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
