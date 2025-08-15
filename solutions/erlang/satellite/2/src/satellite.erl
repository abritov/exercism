-module(satellite).

-export([tree_from_traversals/2]).

% satellite:tree_from_traversals(["a", "i", "x", "f", "r"], ["i", "a", "f", "x", "r"]).
%   a
%  / \
% i   x
%    / \
%   f   r
tree_from_traversals(Pre, In) when length(Pre) == length(In) ->
  case {lists:uniq(Pre), lists:uniq(In)} of
    {P, I} when length(P) < length(Pre) orelse length(I) < length(In) ->
      erlang:error("duplicate items");
    _ ->
      build(Pre, In)
  end.

build([], []) -> #{};

build([H|T], I) ->
  {LI, [H|RI]} = lists:splitwith(fun(X) -> X /= H end, I),
  {LP, RP} = lists:split(length(LI), T),
  #{l => build(LP, LI), r => build(RP, RI), v => H}.
