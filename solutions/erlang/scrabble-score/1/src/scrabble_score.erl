-module(scrabble_score).

-export([score/1]).


score(Word) -> lists:foldl(fun (Letter, Sum) -> points(Letter) + Sum end, 0, string:to_upper(Word)).

points($A) -> 1;
points($E) -> 1;
points($I) -> 1;
points($O) -> 1;
points($U) -> 1;
points($L) -> 1;
points($N) -> 1;
points($R) -> 1;
points($S) -> 1;
points($T) -> 1;
points($D) -> 2;
points($G) -> 2;
points($B) -> 3;
points($C) -> 3;
points($M) -> 3;
points($P) -> 3;
points($F) -> 4;
points($H) -> 4;
points($V) -> 4;
points($W) -> 4;
points($Y) -> 4;
points($K) -> 5;
points($J) -> 8;
points($X) -> 8;
points($Q) -> 10;
points($Z) -> 10.
