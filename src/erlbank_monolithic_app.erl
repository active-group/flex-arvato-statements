%%%-------------------------------------------------------------------
%% @doc erlbank_monolithic public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_monolithic_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/bank-statements/request", web_frontend, request}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8000}],
                                 #{env => #{dispatch => Dispatch}}).

keep_pinging(Node) ->
    case net_adm:ping(Node) of
        pong -> lager:info("successfully connected with node ~p", [Node]),
                ok;
        _ -> lager:info("re-pinging node ~p", [Node]),
             timer:sleep(1000),
             keep_pinging(Node)
    end.

start(_StartType, _StartArgs) ->
    database:init_database(),
    lager:start(),
    start_cowboy(),

    AccountNode = list_to_atom(os:getenv("ACCOUNT_NODE")),
    TransactionNode = list_to_atom(os:getenv("TRANSACTION_NODE")),
    keep_pinging(AccountNode),
    keep_pinging(TransactionNode),

    lager:info("starting statements processes"),
    account_poller:poll_process(),
    statements_server:start(ok),
    erlbank_monolithic_sup:start_link().

stop(_State) ->
    ok.

