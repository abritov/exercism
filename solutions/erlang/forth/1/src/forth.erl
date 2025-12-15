-module(forth).

-export([evaluate/1, evaluate/3, process_binding/2]).


evaluate(Expr) ->
  evaluate(lexemes(Expr), #{}, []).

evaluate([], _Bindings, Stack) -> Stack;

evaluate([[] | Rest], B, Stack) ->
  io:format("evaluate skip empty token~n", []),
  evaluate(Rest, B, Stack);

evaluate([[$:, _ | _] = Binding | Rest] = Expr, B, Stack) ->
  io:format("evaluate binding ~p Stack ~p Expr ~p Rest ~p~n", [Binding, Stack, Expr, Rest]),
  [":", Name | RestBinding] = string:lexemes(Binding, " "),
  case string:to_integer(Name) of 
    {_Int, []} ->
      erlang:error("can't use number as binding name");
    {error, no_integer} ->  
      case lists:last(RestBinding) of
        ";" ->
          Args = lists:droplast(RestBinding),
          Expanded = lists:map(fun (Tok) ->
                                   case maps:find(Tok, B) of
                                     {ok, NewVal} when is_list(NewVal) ->
                                       [" " | lists:join(" ", NewVal)] ++ [" "];
                                     {ok, NewVal} when is_binary(NewVal) ->
                                       binary_to_list(NewVal);
                                     {ok, NewVal} ->
                                       NewVal;
                                     error ->
                                       Tok
                                   end
                               end, Args),
          io:format("put binding ~p ~p into ~p ~n", [Name, Expanded, B]),
          evaluate(lexemes(Rest), maps:put(string:to_lower(Name), Expanded, B), Stack);
        _ ->
          {error, missing_terminator}
      end
  end;

evaluate([32 | Rest] = _Expr, B, Stack) ->
  evaluate(Rest, B, Stack);

% forth:evaluate([": foo 97 97 ;", "foo 65"]).
evaluate([Tok | Rest] = Expr, B, Stack) ->
  io:format("evaluate token ~p Stack ~p Expr ~p~n", [Tok, Stack, Expr]),
  case process_binding(Tok, B) of
    {expr, 0, [Int]} when is_integer(Int) ->
      io:format("token is integer ~p~n", [Int]),
      evaluate(Rest, B, Stack ++ [Int]);
    {expr, 0, Some} ->
      % Tok contains no variables
      case string:to_integer(Some) of
        {Int, []} ->
          io:format("token parsed into integer ~p~n", [Int]),
          evaluate(Rest, B, Stack ++ [Int]);
        {error, no_integer} ->
          case hd(Some) of
            "+" ->
              NewStack = process_bin_op($+, Stack),
              evaluate(Rest, B, NewStack);
            "-" ->
              NewStack = process_bin_op($-, Stack),
              evaluate(Rest, B, NewStack);
            "*" ->
              NewStack = process_bin_op($*, Stack),
              evaluate(Rest, B, NewStack);
            "/" ->
              NewStack = process_bin_op($/, Stack),
              evaluate(Rest, B, NewStack);
            "drop" = Op ->
              NewStack = process_bin_op(Op, Stack),
              evaluate(Rest, B, NewStack);
            "swap" = Op ->
              NewStack = process_bin_op(Op, Stack),
              evaluate(Rest, B, NewStack);
            "over" = Op ->
              NewStack = process_bin_op(Op, Stack),
              evaluate(Rest, B, NewStack);
            "dup" = Op ->
              NewStack = process_bin_op(Op, Stack),
              evaluate(Rest, B, NewStack)
          end;
        {Int, Some} ->
          io:format("number ~p ~p~n", [Int, Some]),
          {error, invalid_integer}
      end;
    {expr, Length, Tokens} ->
      io:format("Tok variable was expanded as it includes variables, Tokens ~p Length ~p~n", [lists:flatten(Tokens), Length]),
      NewStack = evaluate(lexemes(lists:flatten(Tokens)), B, Stack),
      io:format("Continue to evaluate original expression ~p~n", [Rest]),
      evaluate(Rest, B, NewStack)
    end.

process_binding([":", Name | Rest] = Expr, Bindings) when is_list(Expr) ->
  case lists:last(Rest) of
    ";" ->
      Args = lists:droplast(Rest),
      {binding, maps:put(Name, Args, Bindings)};
    _ ->
      {error, missing_terminator}
  end;
process_binding(X, _Bindings) when is_integer(X) ->
  % io:format("process_binding integer ~p~n", [length(X)]),
  {expr, 0, [X]};
process_binding(Expr, Bindings) ->
  io:format("process_binding Expr ~p Bindings ~p ~n", [Expr, Bindings]),
  % Result = maps:fold(fun (K, V, Acc) when is_list(V) ->
  %                          string:replace(Acc, K, V);
  %                        (K, V, Acc) ->
  %                          string:replace(Acc, K, V)
  %                    end, Expr, Bindings),
  {Result, Length} = lists:mapfoldl(fun (Tok, Length) ->
                         case maps:find(Tok, Bindings) of
                           {ok, NewVal} when is_list(NewVal) ->
                             {[" " | lists:join(" ", NewVal)] ++ [" "], Length + length(NewVal)};
                           % {ok, NewVal} when is_integer(NewVal) ->
                           %   {[NewVal], Length + 1};
                           {ok, NewVal} when is_binary(NewVal) ->
                             {binary_to_list(NewVal), Length + 1};
                           {ok, NewVal} ->
                             {NewVal, Length + length(NewVal)};
                           error ->
                             {Tok, Length}
                         end
                     end, 0, string:lexemes(string:to_lower(Expr), " ")),
  % Length = length(maps:to_list(Bindings)),
  {expr, Length, Result}.

process_bin_op("drop", Stack) ->
  lists:droplast(Stack);

process_bin_op("swap", Stack) ->
  {A, B, Stack1} = get_stack_args(Stack),
  Stack1 ++ [A, B];

process_bin_op("over", Stack) ->
  {A, B, Stack1} = get_stack_args(Stack),
  Stack1 ++ [B, A, B];

process_bin_op("dup", Stack) ->
  A = lists:last(Stack),
  Stack ++ [A];

process_bin_op($+, Stack) ->
  {A, B, Stack1} = get_stack_args(Stack),
  Stack1 ++ [A + B];

process_bin_op($-, Stack) ->
  {A, B, Stack1} = get_stack_args(Stack),
  Stack1 ++ [B - A];
    
process_bin_op($*, Stack) ->
  {A, B, Stack1} = get_stack_args(Stack),
  Stack1 ++ [A * B];

process_bin_op($/, Stack) ->
  {A, B, Stack1} = get_stack_args(Stack),
  Stack1 ++ [B div A].

get_stack_args(Stack) ->
  io:format("Stack ~p~n", [Stack]),
  A = lists:last(Stack),
  Stack1 = lists:droplast(Stack),
  io:format("A ~p Stack ~p~n", [A, Stack1]),
  B = lists:last(Stack1),
  io:format("B ~p Stack ~p~n", [B, lists:droplast(Stack1)]),
  {A, B, lists:droplast(Stack1)}.

lexemes(Expr) ->
  case string:find(Expr, ":") of
    nomatch ->
      string:lexemes(Expr, " ");
    _Some ->
      Expr
  end.

