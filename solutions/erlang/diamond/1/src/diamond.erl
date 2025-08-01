-module(diamond).

-export([rows/1, print_row/3]).

-define(PAD_CHAR, " ").

print_row(Letter, Size, 0) ->
  % first row
  string:pad([Letter], Size, both, ?PAD_CHAR);

print_row(Letter, Size, Index) when Size div 2 == Index ->
  % middle row
  Padding = string:copies(?PAD_CHAR, Size - 2),
  lists:concat([[Letter], Padding, [Letter]]);

print_row(Letter, Size, Index) when Size == Index + 1 ->
  % last row
  string:pad([Letter], Size, both, ?PAD_CHAR);

print_row(Letter, Size, Index) when Index > 0,
                                    Index < Size ->
  BorderSize = abs((Size div 2) - Index),
  CenterSize = abs(Size - (BorderSize * 2 + 2)),
  Center = string:copies(?PAD_CHAR, CenterSize),
  Border = string:copies(?PAD_CHAR, BorderSize),
  lists:concat([Border, [Letter], Center, [Letter], Border]);

print_row(_Letter, _Size, _Index) ->
  throw(badarg).


rows([Letter]) ->
  Rows = Letter - $A,
  GridSize = Rows * 2 + 1,
  Top = lists:map(fun ({I, Char}) -> lists:flatten(diamond:print_row(Char, GridSize, I)) end, [{X, $A + X} || X <- lists:seq(0, Rows)]),
  Bottom = lists:map(fun ({I, Char}) -> lists:flatten(diamond:print_row(Char, GridSize, I)) end, [{X, $A + X} || X <- lists:seq(Rows - 1, 0, -1)]),
  lists:concat([Top, Bottom]).
