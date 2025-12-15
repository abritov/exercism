-module(all_your_base).

-export([rebase/3, to_binary/3]).


rebase(Digits, Src, _OutputBase) -> upcast(Digits, length(Digits) - 1, Src, 0).

upcast([], _, _, Result) -> Result;

upcast([Digit | Rest], I, Src, Result) when Digit == 0 ->
  upcast(Rest, I - 1, Src, Result);

upcast([Digit | Rest], I, Src, Result) when Digit == 1 ->
  upcast(Rest, I - 1, Src, [trunc(math:pow(Src, I)) | Result]).

to_binary([], _, Result) -> Result;

to_binary([Digit | Rest], I, Result) when Digit == 0 ->
  to_binary(Rest, I - 1, Result);

to_binary([Digit | Rest], I, Result) when Digit == 1 ->
  to_binary(Rest, I - 1, trunc(math:pow(2, I)) + Result).
