-module(exchange_rates_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  Req2 =
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello World!">>, Req),
  {ok, Req2, State}.
