-module(simple_linked_list).

-export([cons/2, count/1, empty/0, from_native_list/1,
     head/1, reverse/1, tail/1, to_native_list/1]).

empty() -> nil.

cons(Value, nil) ->
  #{value => Value, next => nil};

cons(ValueToAdd, #{next := Next, value := Value}) ->
  #{value => Value, next => cons(ValueToAdd, Next)};
cons(Value, #{next := nil}) ->
  Value.

tail(#{next := List}) -> List.

head(#{next := nil, value := Value}) ->
  Value;
head(#{next := List}) ->
  head(List).

reverse(_List) -> undefined.

count(nil) -> 0;
count(#{next := List}) ->
    count(List, 1).

count(nil, Length) -> Length;
count(#{next := List}, Length) ->
  count(List, Length + 1).

to_native_list(_List) -> undefined.

from_native_list(_NativeList) -> undefined.
