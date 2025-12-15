-module(simple_linked_list).

-export([cons/2, count/1, empty/0, from_native_list/1,
     head/1, reverse/1, tail/1, to_native_list/1]).

empty() -> {}.

cons(E, L) -> {E, L}.

head({H, _}) -> H.

tail({_, T}) -> T.

reverse(L) -> reverse(L, {}).

reverse({}, Acc) -> Acc;
reverse({H, T}, Acc) -> reverse(T, cons(H, Acc)).

count({}) -> 0;
count({_, T}) -> 1+count(T).

to_native_list({}) -> [];
to_native_list({H, T}) -> [H|to_native_list(T)].

from_native_list([]) -> {};
from_native_list([H|T]) -> cons(H, from_native_list(T)).
