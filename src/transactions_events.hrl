% Service name: transaction_service

% einfacher
-record(transaction_event, {
  transaction_id :: unique_id(),  % fortlaufend
  timestamp :: erlang:timestamp(),  % der transaction
  from_acc_nr :: account_number(),
  to_acc_nr :: account_number(),
  amount :: money(),
  from_account_resulting_balance :: money(),
  to_account_resulting_balance :: money()
}).

% subscription event
-record(transaction_event_subscription, {
  from_id :: unique_id(),
  subscriber_pid :: pid()
}).

%%% evtl. flexibler
%%-record(account_transaction_event,
%%{
%%  id :: unique_id(),
%%  timestamp :: erlang:timestamp(),
%%  acc_nr :: account_number(),
%%  amount :: money(),  % can be positive or negative
%%  account_resulting_balance :: money()
%%  }).