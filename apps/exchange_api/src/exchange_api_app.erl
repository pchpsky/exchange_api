%%%-------------------------------------------------------------------
%% @doc exchange_api public API
%% @end
%%%-------------------------------------------------------------------

-module(exchange_api_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/exchange_rates", exchange_rates_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    exchange_api_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener).

%% internal functions
