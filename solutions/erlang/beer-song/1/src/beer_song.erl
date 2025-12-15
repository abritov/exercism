-module(beer_song).
-export([verse/1, sing/1, sing/2]).

verse(0) ->
  "No more bottles of beer on the wall, no more bottles of beer.\n" ++
  "Go to the store and buy some more, 99 bottles of beer on the wall.\n";
verse(1) ->
  "1 bottle of beer on the wall, 1 bottle of beer.\n" ++
  "Take it down and pass it around, no more bottles of beer on the wall.\n";
verse(2) ->
  "2 bottles of beer on the wall, 2 bottles of beer.\n" ++
  "Take one down and pass it around, 1 bottle of beer on the wall.\n";
verse(N) when N > 2 ->
  io_lib:format("~B bottles of beer on the wall, ~B bottles of beer.\n" ++
               "Take one down and pass it around, ~B bottles of beer on the wall.\n", 
               [N, N, N-1]).

sing(From) ->
  sing(From, 0).

sing(From, To) when From >= To ->
  Verses = [verse(N) || N <- lists:seq(From, To, -1)],
  string:join(Verses, "\n") ++ "\n".
