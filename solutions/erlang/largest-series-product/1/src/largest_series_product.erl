-module(largest_series_product).

-export([largest_product/2]).

largest_product(D, S) ->
  Spans = lists:map(fun (Span) -> lists:map(fun ch2int/1, Span) end, spans(D, S, [])),
  lists:max(lists:map(fun product/1, Spans)).

spans([_ | Rest] = Str, Len, Result) when length(Str) >= Len ->
  Span = lists:sublist(Str, 1, Len),
  spans(Rest, Len, [Span | Result]);

spans(_, _, Result) -> Result.

product(List) ->
  lists:foldl(fun (N, Acc) -> Acc * N end, 1, List).

ch2int(C) -> C - $0.
