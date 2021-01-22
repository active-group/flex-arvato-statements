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
    AccountCreatedList = AccountPid ! #get{fromAccountId = 0},
    parseAndSaveAccounts(AccountCreatedList).

parseAndSaveAccounts([]) -> ok;
parseAndSaveAccounts([AccountCreated | Rest]) ->
    database:put_account(#account{account_number = AccountCreated#accountCreated.account_number, firstname=AccountCreated#accountCreated.firstname, surname = AccountCreated#accountCreated.surname, amount=AccountCreated#accountCreated.amount}),
    parseAndSaveAccounts(Rest).
