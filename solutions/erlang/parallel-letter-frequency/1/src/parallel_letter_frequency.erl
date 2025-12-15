-module(parallel_letter_frequency).

-export([dict/1]).

dict(Strings) ->
  Tasks = [async(fun () -> get_frequences(X) end) || X <- Strings],
  Freqs = [await(X, 5000) || X <- Tasks],
  lists:foldl(fun (Val, Acc) -> dict:merge(fun(_Key, Val1, Val2) -> Val1 + Val2 end, Val, Acc) end, dict:new(), Freqs).

async(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() -> 
        Result = try Fun() catch E:R -> {error, {E,R}} end,
        Parent ! {Ref, Result}
    end),
    {Pid, Ref}.

await({Pid, Ref}, Timeout) ->
    receive
        {Ref, Result} -> 
            Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.

inc(N) -> N + 1.

get_frequences(List) ->
    lists:foldl(fun(Elem, Acc) ->
                    case dict:is_key(Elem, Acc) of
                        true -> dict:update(Elem, fun inc/1, Acc);
                        false -> dict:store(Elem, 1, Acc)
                    end
                end, dict:new(), List).
