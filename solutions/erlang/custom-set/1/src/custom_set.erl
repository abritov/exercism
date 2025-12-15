-module(custom_set).

-export([add/2, contains/2, difference/2, disjoint/2, empty/1, equal/2, from_list/1, intersection/2, subset/2,
	 union/2]).


add(Elem, Set) ->
	case lists:member(Elem, Set) of
		false ->
			[Elem | Set];
		true ->
			Set
	end.

contains(Elem, Set) -> lists:member(Elem, Set).

difference([], []) -> [];
difference([], _Set2) -> [];
difference(Set1, []) -> Set1;
difference(Set1, Set2) -> lists:filter(fun (Elem) -> not lists:member(Elem, Set2) end, Set1).

disjoint([], []) -> true;
disjoint(Set1, Set2) ->
	lists:all(fun (Elem) -> not lists:member(Elem, Set2) end, Set1) orelse lists:all(fun (Elem) -> not lists:member(Elem, Set1) end, Set2).

empty([]) -> true;
empty(_Set) -> false.

equal(Set, Set) -> true;
equal(Set1, Set2) ->
	lists:sort(Set1) == lists:sort(Set2).

from_list(List) -> List.

intersection([], []) -> [];
intersection([], _Set2) -> [];
intersection(_Set1, []) -> [];

intersection(Set1, Set2) ->
	case disjoint(Set1, Set2) of
		true ->
			[];
		false ->
			lists:foldl(fun (Elem, Acc) -> case lists:member(Elem, Set2) of
																			 true ->
																				 [Elem | Acc];
																			 _ ->
																				 Acc
																		 end
									end, [], Set1) 
	end.

subset([], []) -> true;
subset([], _Set2) -> true;
subset(_Set1, []) -> false;
subset(Set, Set) -> true;
subset(Set1, Set2) -> string:str(Set2, Set1) /= 0.

union([], []) -> [];
union([], Set2) -> Set2;
union(Set1, []) -> Set1;
union(Set1, Set2) ->
	lists:uniq(lists:concat([Set1, Set2])).
