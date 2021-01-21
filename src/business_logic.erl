%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([get_account/1, sort_tx/1, get_transactions/1 ]).


-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNr) -> database:get_account(AccountNr).

-spec get_transactions(unique_id()) -> list(#transaction{}).
get_transactions(Id) ->
     database:get_all_transactions(Id).

sort_tx(Txs) ->
    lists:sort(fun(Tx1, Tx2) -> Tx2#transaction.id < Tx1#transaction.id end, Txs).
