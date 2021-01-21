-module(business_logic_statement_test).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").

setup() ->
    database:init_database().

cleanup(_) -> 
    database:init_database().

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun test_get_transactions/1, fun test_get_account/1]
     }}.

test_get_transactions(_) ->
    fun() ->
        Transaction1 = #transaction{id = 14, timestamp = {1610,547469,326863}, from_acc_nr = 1, to_acc_nr = 2, amount = 120 },
            database:put_transaction(Transaction1),
        Transaction2 = #transaction{id = 17, timestamp = {1610,547469,326863}, from_acc_nr = 3, to_acc_nr = 1, amount = 50 },
            database:put_transaction(Transaction2),
        Transactions = business_logic:get_transactions(1),
        ?assertEqual(Transactions, [Transaction1, Transaction2])
    end.

test_get_account(_) -> 
    fun() ->
        Account = #account{account_number = 65, person_id = 98, amount = 1423 },
        database:put_account(Account),
        {ok, BLAccount} = business_logic:get_account(65),
        ?assertEqual(Account, BLAccount)
    end.






