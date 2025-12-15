-module(rna_transcription).

-export([to_rna/1]).


convert($G) -> $C;
convert($C) -> $G;
convert($T) -> $A;
convert($A) -> $U.

to_rna(String) ->
    lists:map(fun convert/1, String).
