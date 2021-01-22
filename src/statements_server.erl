-module(statements_server).
-include("data.hrl").
-include("transactions_events.hrl").

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3,
         start/1]).

start(_) ->
    {_, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

-spec process_transaction(#transaction_event{}) -> ok.
process_transaction(#transaction_event{transaction_id = TxId, timestamp = Timestamp, from_acc_nr = FromAcc, to_acc_nr = ToAcc, amount = Amount, from_account_resulting_balance = FromBalance, to_account_resulting_balance = ToBalance}) -> 
    database:put_transaction(#transaction{id=TxId, timestamp=Timestamp,from_acc_nr=FromAcc, to_acc_nr=ToAcc, amount=Amount}),   
    FromAccount = database:get_account(FromAcc),
    database:put_account(#account{account_number=FromAcc, firstname = FromAccount#account.firstname, surname = FromAccount#account.surname, amount = FromBalance}),
    ToAccount = database:get_account(ToAcc),
    database:put_account(#account{account_number=ToAcc, firstname = ToAccount#account.firstname, surname = ToAccount#account.surname, amount = ToBalance}),
    ok.

init(_) -> 
    TxPid = global:whereis_name(transaction_service),
    gen_server:cast(TxPid, #transaction_event_subscription{from_id = 0, subscriber_pid = self()}),
    {ok, state}. 

handle_cast(Message, _) -> {noreply, process_transaction(Message)}.

handle_call(_, _From, _) -> 
    Reply = ok,
    NewState = ok,
    {reply, Reply, NewState}.
