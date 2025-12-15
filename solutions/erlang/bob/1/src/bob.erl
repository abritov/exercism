-module(bob).
-export([response/1, is_question/1, is_yell/1]).

is_uppercase(Char) when Char >= $A, Char =< $Z -> true;
is_uppercase(_) -> false.

% is_question(Req) ->
%   [LastLetter | _Rest] = string:reverse(Req),
%   LastLetter == $?.

is_question(Req) ->
  case re:run(Req, "\\?$") of
    {match, _} ->
      true;
    nomatch ->
      false
  end.

is_yell([]) ->
  false;
is_yell(Req) ->
  lists:all(fun is_uppercase/1, Req).

response(Req) ->
  Req1 = re:replace(Req, "[^A-Za-z1-9?]", "", [global, {return, list}]),
  IsYell = is_yell(re:replace(Req, "[^A-Za-z]", "", [global, {return, list}])),
  IsQuestion = is_question(Req1),
  IsSilence = string:is_empty(Req1),
  if
    IsSilence ->
      "Fine. Be that way!";
    IsQuestion and IsYell ->
      "Calm down, I know what I'm doing!";
    IsQuestion ->
      "Sure.";
    IsYell ->
      "Whoa, chill out!";
    true ->
      "Whatever."
  end.
