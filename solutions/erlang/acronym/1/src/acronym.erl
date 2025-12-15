-module(acronym).

-export([abbreviate/1]).


abbreviate(Phrase) ->
  Lexemes = string:lexemes(Phrase, " -_"),
  lists:map(fun ([FirstLetter | _Rest]) -> string:to_upper(FirstLetter) end, Lexemes).
