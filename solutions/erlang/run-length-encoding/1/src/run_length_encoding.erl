-module(run_length_encoding).

-export([decode/1, encode/1]).


decode(Str) ->
  decode(Str, []).

decode([], Result) ->
  Result;

decode([Ch | Rest], Result) when Ch == 32 ->
  io:format("decode whitespace~n"),
  decode(Rest, Result ++ [Ch]);

decode(Str, Result) ->
  case string:to_integer(Str) of
    {error, no_integer} ->
      io:format("no_integer ~p~n", [Str]),
      [Ch | Rest] = Str,
      decode(Rest, Result ++ [Ch]);
    {Len, [Ch | Rest]} ->
      io:format("decode ~p into ~p~n", [Str, string:copies([Ch], Len)]),
      decode(Rest, Result ++ string:copies([Ch], Len))
  end.

encode([]) -> [];

encode([First | Rest]) ->
  encode(Rest, First, 1, []).

encode([Ch | Rest], Current, Length, Acc) when Ch == 32, Ch == Current, Acc == [] orelse Rest == [] ->
  io:format("encode space(~p) \"~s\", Current ~p, Length ~p, Acc ~p~n", [Rest, [Ch], Current, Length, Acc]),
  encode(Rest, Current, Length + 1, Acc);

encode([Ch | Rest], Current, Length, [_ | _] = Acc) when Ch == 32, Ch == Current ->
  io:format("encode space(~p) \"~s\", Current ~p, Length ~p, Acc ~p~n", [Rest, [Ch], Current, Length, Acc]),
  encode(Rest, Current, Length, Acc ++ [Ch]);

encode([Ch | Rest], Current, Length, Acc) when Ch == Current ->
  io:format("encode Ch \"~s\", Current ~p, Length ~p, Acc ~p~n", [[Ch], Current, Length, Acc]),
  encode(Rest, Current, Length + 1, Acc);

encode([Ch | Rest], Current, 1, Acc) ->
  io:format("encode single char ~s, Current ~p, ~p~n", [[Ch], Current, Acc]),
  encode(Rest, Ch, 1, Acc ++ [Current]);

encode([Ch | Rest], Current, Length, Acc) ->
  encode(Rest, Ch, 1, Acc ++ integer_to_list(Length) ++ [Current]);

encode([], Current, Length, Acc) when Length > 1 ->
  Acc ++ integer_to_list(Length) ++ [Current];

encode([], Current, 1, Acc) ->
  Acc ++ [Current].
