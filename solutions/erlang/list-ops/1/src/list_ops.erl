-module(list_ops).

-export([append/2, concat/1, filter/2, length/1, map/2, foldl/3, foldr/3,
	 reverse/1]).

append([], []) -> [];
append([], List) -> List;
append([_ | _] = List1, List2) ->
	Last = lists:last(List1),
	Rest = lists:droplast(List1),
	append(Rest, [Last | List2]).

concat([]) -> [];
concat([A | Rest]) when is_list(A) ->
	concat(A, concat(Rest));
concat(Result) -> Result.

concat(A, Result) ->
	append(A, Result).

filter(Function, List) ->
	filter(Function, List, []).

filter(_, [], Result) -> Result;
filter(Function, [First | Rest], Result) ->
	case Function(First) of
		true ->
			filter(Function, Rest, Result ++ [First]);
		false ->
			filter(Function, Rest, Result)
	end.

length(List) ->
	length(List, 0).

length([], Result) -> Result;
length([_ | Rest], Result) -> length(Rest, Result + 1).

map(Function, List) ->
	map(Function, List, []).

map(_, [], Result) -> Result;
map(Function, [First | Rest], Result) ->
	map(Function, Rest, Result ++ [Function(First)]).

foldl(_F, Result, []) -> Result;
foldl(F, Start, [First | Rest]) ->
	foldl(F, F(First, Start), Rest).

foldr(_Function, Start, []) -> Start;
foldr(F, Start, List) ->
	foldr(F, F(lists:last(List), Start), lists:droplast(List)).

reverse(List) -> reverse(List, []).

reverse([], Result) -> Result;
reverse([First | Rest], Result) ->
	reverse(Rest, [First | Result]).
