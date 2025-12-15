-module(secret_handshake).

-export([commands/1]).


% commands(N) -> get_flags(0, integer_to_list(N, 2), []).

commands(Num) ->
  Mappings = [{1, "wink"}, {2, "double blink"}, {4, "close your eyes"}, {8, "jump"}],
  lists:foldl(fun ({Mask, Action}, Acc) when Num band 16 == 16 ->
                  if Num band Mask == Mask ->
                       [Action | Acc];
                     true ->
                       Acc
                  end;
                  ({Mask, Action}, Acc) ->
                  if Num band Mask == Mask ->
                       Acc ++ [Action];
                     true ->
                       Acc
                  end
              end, [], Mappings).
