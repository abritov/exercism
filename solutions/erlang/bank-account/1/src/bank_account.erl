-module(bank_account).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([balance/1, charge/2, close/1, create/0, deposit/2, withdraw/2]).

balance(Pid) ->
  case erlang:is_process_alive(Pid) of
    true ->
      gen_server:call(Pid, balance);
    _ ->
      {error, account_closed}
  end.

charge(Pid, Amount) ->
  gen_server:call(Pid, {charge, Amount}).

close(Pid) ->
  gen_server:call(Pid, stop).

create() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  Pid.

deposit(Pid, Amount) ->
  gen_server:call(Pid, {deposit, Amount}).

withdraw(Pid, Amount) ->
  gen_server:call(Pid, {withdraw, Amount}).

init([]) ->
  {ok, 0}.

handle_call(balance, _From, Amount) ->
  {reply, Amount, Amount};

handle_call({charge, Charge}, _From, Amount) when Charge < 0 ->
  {reply, 0, Amount};
handle_call({charge, Charge}, _From, Amount) when Charge > Amount ->
  {reply, 0, Amount};

handle_call({charge, Charge}, _From, Amount) ->
  {reply, Charge, Amount - Charge};

handle_call({deposit, Add}, _From, Amount) when Add =< 0->
  {reply, 0, Amount};
handle_call({deposit, Add}, _From, Amount) ->
  NewAmount = Amount + Add,
  {reply, NewAmount, NewAmount};

handle_call({withdraw, Sub}, _From, Amount) when Sub =< 0 ->
  {reply, 0, Amount};
handle_call({withdraw, Sub}, _From, Amount) when Sub >= Amount ->
  {reply, Amount, 0};
handle_call({withdraw, Sub}, _From, Amount) ->
  {reply, Sub, Amount - Sub};

handle_call(stop, _From, Amount) ->
  {stop, normal, Amount, 0}.

handle_cast(_Request, Count) ->
  {noreply, Count}.
