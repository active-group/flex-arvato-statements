-module(account_poller).
-export([poll_process/0, poll_loop/0]).

-include("data.hrl").
-record(get, {fromAccountId :: number()}).
-record(accountCreated,
    {
        firstname :: binary(),
        surname :: binary(),
        account_number :: integer(),
        amount :: number()
    }).

poll_loop() ->         
    poll_accounts(),
    timer:sleep(1000),
    poll_loop().

poll_process() ->
    spawn(?MODULE, poll_loop, []).

poll_accounts() ->
    AccountPid = global:whereis_name(account_service),
    if
        AccountPid == undefined ->
            lager:info("No Account Service found :(");
        true ->
            lager:info("Calling account service at ~p", [AccountPid]),
            AccountCreatedList = gen_server:call(AccountPid, #get{ fromAccountId = database:get_latest_account_number()+1 }),
            lager:info("Got account list from account service ~p", [AccountCreatedList]),
            parseAndSaveAccounts(AccountCreatedList)
    end.

parseAndSaveAccounts([]) -> ok;
parseAndSaveAccounts([AccountCreated | Rest]) ->
    % LastTx = lists:last(database:get_all_transactions(AccountCreated#accountCreated.account_number)),
    % if
    %     LastTx == undefined -> Amount = AccountCreated#accountCreated.amount;
    %     true -> 
    %         if
    %             LastTx#transaction.from_acc_nr == AccountCreated#accountCreated.account_number -> Amount = LastTx#transaction.from
    % end.
    database:put_account(#account{account_number = AccountCreated#accountCreated.account_number, firstname=AccountCreated#accountCreated.firstname, surname = AccountCreated#accountCreated.surname, amount=AccountCreated#accountCreated.amount}),
    parseAndSaveAccounts(Rest).
