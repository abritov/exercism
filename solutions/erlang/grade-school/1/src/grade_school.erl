-module(grade_school).

-export([add/3, get/2, get/1, new/0]).


add(Name, Grade, School) ->
  Names = grade_school:get(School),
  case lists:member(Name, Names) of
    true ->
      School;
    false ->
      maps:update_with(Grade, fun (Pupils) -> sets:add_element(Name, Pupils) end, sets:add_element(Name, sets:new()), School)
  end.

get(Grade, School) -> sets:to_list(maps:get(Grade, School, sets:new())).

get(S) ->
  case maps:size(S) of
    0 ->
      [];
    _Else ->
      case lists:flatmap(fun sets:to_list/1, maps:values(S)) of
        [] ->
          [];
        Some ->
          Some
      end
  end.

new() ->
  maps:new().
