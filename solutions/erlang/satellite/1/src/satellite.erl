-module(satellite).

-export([tree_from_traversals/2]).


tree_from_traversals([], []) -> #{};

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
      tree_from_traversals(Pre, In, #{})
  end.

tree_from_traversals([Root | Pre], In, Tree) ->
  io:format("Root ~p, In ~p, Tree ~p~n", [Root, In, Tree]),
  {Left, RestIn} = lists:splitwith(fun (El) -> El /= Root end, In),
  io:format("Left ~p, RestIn ~p~n", [Left, RestIn]),
  #{l => in_order(Root, In), r => tree_from_traversals(Pre, RestIn, Tree), v => Root};

tree_from_traversals([], _, Tree) -> Tree.

% pre_order([First, L | Rest], Tree) ->
%   #{l => L, r => pre_order(Rest, #{}), v => First};
% 
% pre_order([], Tree) -> Tree.

in_order(Root, [Left | _]) when Root /= Left ->
  io:format("in_order Root ~p, Left ~p~n", [Root, Left]),
  #{l => #{}, r => #{}, v => Left}.
  
