-module(minesweeper).

-export([annotate/1, find_mines/1, annotate_field/1, inc_cell/2, annotate_mine/3, get_field_size/1]).


annotate([]) -> [];
annotate([""]) -> [""];

annotate(Field) ->
  Mines = find_mines(Field),
  Size = get_field_size(Field),
  if
    length(Mines) == Size ->
      Field;
    true ->
      format_field(lists:foldl(fun (Cell, Acc) ->
                      Neighbours = get_neighbour_cells(Cell, Mines),
                      annotate_mine(Cell, Neighbours, Acc)
                  end,
                  annotate_field(Field), Mines))
  end.

annotate_mine(Mine, Neighbours, AnnotatedField) ->
  Cells = lists:foldl(fun (Cell, Acc) ->
                          lists:keyreplace(Cell, 1, Acc, {Cell, inc_cell(Cell, AnnotatedField)})
                      end,
                      AnnotatedField, Neighbours),
  lists:sort([{Mine, $*} | Cells]).

annotate_field(Field) ->
  Rows = length(Field),
  Cols = length(hd(Field)),
  [{{I, J}, get_cell(I, J, Field)} ||
   I <- lists:seq(1, Rows),
   J <- lists:seq(1, Cols),
   get_cell(I, J, Field) =/= $*].

get_neighbour_cells({Row, Col}, Mines) ->
  Right = {Row, Col + 1},
  Left = {Row, Col - 1},
  Up = {Row - 1, Col},
  Down = {Row + 1, Col},
  UpperLeft = {Row - 1, Col - 1},
  UpperRight = {Row - 1, Col + 1},
  BottomLeft = {Row + 1, Col - 1},
  BottomRight = {Row + 1, Col + 1},
  Result = [Right, Left, Up, Down, UpperLeft, UpperRight, BottomLeft, BottomRight],
  lists:filter(fun (Cell) -> not lists:member(Cell, Mines) end, Result).

format_field(AnnotatedField) ->
  format_field(AnnotatedField, []).

format_field([], Result) -> lists:reverse(Result);

format_field([Row | _] = AnnotatedField, Result) ->
  {{CurrentRowIdx, _ColIdx}, _Value} = Row,
  {Cols, Rest} = lists:partition(fun
                           ({{RowIdx, _}, _Val}) -> CurrentRowIdx =:= RowIdx
                         end, AnnotatedField),
  RowToAdd = lists:map(fun ({_, Value}) -> Value end, Cols),
  format_field(Rest, [RowToAdd | Result]).

inc_cell(Cell, AnnotatedField) ->
  case lists:keyfind(Cell, 1, AnnotatedField) of
    {_, Val} ->  
      inc_cell(Val);
    _ ->
      Cell
  end.

inc_cell($*) -> $*;
inc_cell(32) -> $1;
inc_cell(Cell) ->
  Cell + 1.

get_field_size(Field) ->
  length(Field) * length(hd(Field)).

find_mines(Field) ->
  Rows = length(Field),
  Cols = length(hd(Field)),
  [{I, J} ||
   I <- lists:seq(1, Rows),
   J <- lists:seq(1, Cols),
   get_cell(I, J, Field) == $*].

get_cell(I, J, Field) ->
  lists:nth(J, lists:nth(I, Field)).

