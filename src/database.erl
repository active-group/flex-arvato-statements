%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0,
         put_account/1, get_account/1, get_all_accounts/0,         
         put_transaction/1, get_transaction/1, get_all_transactions/0, get_all_transactions/1, 
         unique_account_number/0,unique_tx_id/0,
         atomically/1, get_latest_account_number/0]).

%% id-table for atomic id increment
-record(table_id, {table_name :: mnesia:table(), last_id :: non_neg_integer()}).

%% destroy tables in case they already existed
destroy_tables() ->
    mnesia:delete_table(transaction),
    mnesia:del_table_copy(transaction, node()),
    mnesia:delete_table(account),
    mnesia:del_table_copy(account, node()),
    mnesia:delete_table(table_id),
    mnesia:del_table_copy(table_id, node()).

create_tables() ->
    mnesia:create_table(transaction, [{attributes, [id, timestamp, from_acc_nr, to_acc_nr, amount]}]),
    mnesia:create_table(account, [{attributes, [account_number, firstname, surname, amount]}]),
    mnesia:create_table(table_id, [{record_name, table_id}, {attributes, record_info(fields, table_id)}]).

clear_tables() ->
    mnesia:clear_table(account),
    mnesia:clear_table(account),
    mnesia:clear_table(table_id).

init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    destroy_tables(),
    create_tables(),
    ok = mnesia:wait_for_tables([transaction, account, table_id], 5000),
    mnesia:transaction(fun clear_tables/0),
    put_account(#account{account_number = 1, firstname = <<"Max">>, surname = <<"Meier">>, amount = 1000}),
    put_account(#account{account_number = 2, firstname = <<"Martin">>, surname = <<"Müller">>, amount = 1000}),
    put_transaction(#transaction{id = 1, timestamp = {1610,547469,326863}, from_acc_nr = 1, to_acc_nr = 2, amount = 100 }),
    put_transaction(#transaction{id = 2, timestamp = {1610,547469,326863}, from_acc_nr = 2, to_acc_nr = 1, amount = 20 }),
    put_transaction(#transaction{id = 3, timestamp = {1610,547469,326863}, from_acc_nr = 1, to_acc_nr = 2, amount = 450 }),
    ok.

write(Table, Tuple) ->
    Fun = fun() -> ok = mnesia:write(Table, erlang:insert_element(1, Tuple, Table), write) end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.

-spec read_one(mnesia:table(), unique_id(), fun((tuple()) -> Obj)) -> {ok, Obj} | {error, not_found | more_than_one}.
read_one(Table, Id, Deserialize) ->
    Fun = fun() -> mnesia:read(Table, Id) end,
    {atomic, Res} = mnesia:transaction(Fun),
    case Res of
        [Tuple] -> {ok, Deserialize(erlang:delete_element(1, Tuple))};
        []  -> {error, not_found};
        [_ | _] -> {error, more_than_one}
    end.

-spec read_all(mnesia:table(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
    Fun = fun() -> mnesia:select(Table,[{'_',[],['$_']}]) end,
    {atomic, Res} = mnesia:transaction(Fun),
    lists:map(fun (Tuple) -> Deserialize(erlang:delete_element(1, Tuple)) end, Res).

-spec put_account(#account{}) -> ok.
put_account(#account{account_number = AccountNumber, firstname = Firstname, surname = Surname, amount = Amount}) ->
    write(account, {AccountNumber, Firstname, Surname, Amount}).

deserialize_account({AccountNumber, Firstname, Surname, Amount}) ->
    #account{account_number = AccountNumber, firstname = Firstname, surname = Surname, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber, fun deserialize_account/1).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() -> read_all(account, fun deserialize_account/1).

-spec get_latest_account_number() -> number().
get_latest_account_number() ->
     LastAccount = lists:max(get_all_accounts()),
     LastAccount#account.account_number.

-spec put_transaction(#transaction{}) -> ok.
put_transaction(#transaction{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}) ->
    write(transaction, {Id, Timestamp, FromAccNr, ToAccNr, Amount}).

deserialize_transaction({Id, Timestamp, FromAccNr, ToAccNr, Amount}) ->
    #transaction{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}.

-spec get_transaction(unique_id()) -> {ok, #transaction{}} | {error, any()}.
get_transaction(Id) ->
    read_one(transaction, Id, fun deserialize_transaction/1).

-spec get_all_transactions() -> list(#transaction{}).
get_all_transactions() -> read_all(transaction, fun deserialize_transaction/1).

-spec get_all_transactions(account_number()) -> list(#transaction{}).
get_all_transactions(AccountNr) ->
    Fun = fun() ->
            mnesia:select(transaction,
                           [{'$1',
                            [{'orelse',
                                {'==', {element, #transaction.from_acc_nr, '$1'}, AccountNr},
                                {'==', {element, #transaction.to_acc_nr, '$1'}, AccountNr}}],
                            ['$_']}]) 
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    lists:map(fun (Tuple) -> deserialize_transaction(erlang:delete_element(1, Tuple)) end, Res).

-spec unique_account_number() -> unique_id().
unique_account_number() -> mnesia:dirty_update_counter(table_id, account, 1).

-spec unique_tx_id() -> unique_id().
unique_tx_id() -> mnesia:dirty_update_counter(table_id, transaction, 1).

-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Fun) ->
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
