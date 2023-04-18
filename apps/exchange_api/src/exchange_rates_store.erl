-module(exchange_rates_store).

-behaviour(gen_server).

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([get_rates/0]).

-include("include/exchange_rates.hrl").

-define(API_URL, "https://api.privatbank.ua/p24api/pubinfo?json&exchange&coursid=5").

-spec get_rates() -> [#exchange_rate{}].
get_rates() ->
  gen_server:call(?MODULE, get_rates).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  ets:new(exchange_rates, [set, named_table, protected, {keypos, #exchange_rate.id}]),
  ets:insert(exchange_rates,
             [#exchange_rate{id = 1,
                             ccy = "USD",
                             base_ccy = "UAH",
                             buy = "27.60000",
                             sale = "28.02000"},
              #exchange_rate{id = 2,
                             ccy = "EUR",
                             base_ccy = "UAH",
                             buy = "32.40000",
                             sale = "33.01000"}]),
  {ok, []}.

handle_call(get_rates, _From, State) ->
  Rates = ets:tab2list(exchange_rates),
  {reply, Rates, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
