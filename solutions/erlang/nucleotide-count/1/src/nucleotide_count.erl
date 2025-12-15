-module(nucleotide_count).

-export([count/2, nucleotide_counts/1]).


count([], _Nucleotide) -> 0;
count(Strand, Nucleotide) ->
  Freqs = get_frequences(Strand),
  maps:get(Nucleotide, Freqs).

nucleotide_counts(Strand) ->
  AllowedKeys = identity(),
  case lists:any(fun (S) -> not maps:is_key([S], AllowedKeys) end, Strand) of
    true ->
      erlang:error("not allowed");
    false ->
      Freqs = get_frequences(Strand),
      maps:to_list(Freqs)
  end.

inc(N) -> N + 1.

identity() ->
  #{"A" => 0, "C" => 0, "G" => 0, "T" => 0}.

get_frequences(List) ->
  lists:foldl(fun (Elem, Acc) when is_map_key([Elem], Acc) ->
                  maps:update_with([Elem], fun inc/1, Acc)
              end, identity(), List).
