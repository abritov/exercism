-module(protein_translation).

-export([proteins/1, c2a/1]).


proteins([]) -> {ok, []};

proteins(Strand) ->
  Protein = lists:takewhile(fun (El) -> El /= stop end, lists:map(fun c2a/1, split(Strand))),
  case lists:member({error, badarg}, Protein) of
    true ->
      {error, badarg};
    false ->
      {ok, Protein}
  end.

c2a("AUG") -> methionine;
c2a("UUU") -> phenylalanine;
c2a("UUC") -> phenylalanine;
c2a("UUA") -> leucine;
c2a("UUG") -> leucine;
c2a("UCU") -> serine;
c2a("UCC") -> serine;
c2a("UCA") -> serine;
c2a("UCG") -> serine;
c2a("UAU") -> tyrosine;
c2a("UAC") -> tyrosine;
c2a("UGU") -> cysteine;
c2a("UGC") -> cysteine;
c2a("UGG") -> tryptophan;
c2a("UAA") -> stop;
c2a("UAG") -> stop;
c2a("UGA") -> stop;
c2a(_) -> {error, badarg}.

split(String) ->
    [lists:sublist(String, I, 3) || I <- lists:seq(1, length(String), 3)].
