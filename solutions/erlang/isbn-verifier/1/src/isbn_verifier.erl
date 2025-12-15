-module(isbn_verifier).

-export([is_valid/1]).


is_valid(Str) ->
  {Digits, {I, Acc}} = Result = lists:mapfoldl(fun (Char, {I, Acc}) when Char >= $0, Char =< $9 ->
                                                   {Char - $0, {I - 1, Acc + (Char - $0) * I}};
                                                   ($X, {I, Acc}) when I == 1 ->
                                                   {10, {0, Acc + 10}};
                                                   (Char, Acc) ->
                                                   {Char, Acc}
                                               end, {10, 0}, Str),
  case length(lists:filter(fun (Ch) -> Ch /= $- end, Digits)) of
    10 ->
      case {I, Acc} of
        {0, Sum} ->
          (Sum rem 11) == 0;
        _ ->
          false
      end;
    _ ->
      false
  end.
