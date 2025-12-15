-module(anagram).

-export([find_anagrams/2]).


find_anagrams(Subject, Candidates) ->
  FilterFn = anagram_filter(Subject),
  % lists:foldl(fun (Word, Acc) -> sets:subtract(SubjectSet, sets:from_list(Word)) end, [], Candidates).
  lists:filter(fun (Word) -> FilterFn(Word) end, Candidates).

anagram_filter(Subject) ->
  SubjectLower = string:to_lower(Subject),
  SubjectSet = sets:from_list(SubjectLower),
  SubjectFrequences = get_frequences(SubjectLower),
  fun (Word) ->
      WordLower = string:to_lower(Word),
      WordFrequences = get_frequences(WordLower),
      SubjectLower =/= WordLower andalso sets:is_subset(SubjectSet, sets:from_list(WordLower)) andalso SubjectFrequences == WordFrequences end.

inc(N) -> N + 1.

get_frequences(List) ->
  lists:foldl(fun (Elem, Acc) when is_map_key(Elem, Acc) -> maps:update_with(Elem, fun inc/1, Acc);
                  (Elem, Acc) -> maps:put(Elem, 1, Acc) end, #{}, List).
