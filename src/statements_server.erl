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

wait_for_global_name(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            lager:info("retrying finding global name ~p", [Name]),
            timer:sleep(1000),
            wait_for_global_name(Name);
        Pid ->
            lager:info("found global name ~p", [Pid]),
            {ok, Pid}
    end.

init(_) -> 
    {ok, TxPid} = wait_for_global_name(transaction_service),
    lager:info("sending message to transactions service at ~p", [TxPid]),
    gen_server:cast(TxPid, #transaction_event_subscription{from_id = 0, subscriber_pid = self()}),
    {ok, state}. 

handle_cast(Message, _) -> 
    lager:info("received message from transactions service ~p", [Message]),
    {noreply, process_transaction(Message)}.

handle_call(_, _From, _) -> 
    Reply = ok,
    NewState = ok,
    {reply, Reply, NewState}.
