-module(exchange_rates_handler).

-include_lib("xmerl/include/xmerl.hrl").

-export([init/2, allowed_methods/2, content_types_provided/2]).
-export([to_xml/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"xml">>, []}, to_xml}], Req, State}.

to_xml(Req, State) ->
  Data =
    [{exchangerates,
      [{row,
        [{exchangerate,
          [{ccy, "USD"}, {base_ccy, "UAH"}, {buy, "27.60000"}, {sale, "28.02000"}],
          []}]},
      {row,
       [{exchangerate,
         [{ccy, "EUR"}, {base_ccy, "UAH"}, {buy, "32.40000"}, {sale, "33.01000"}],
         []}]}]}],
  Doc = xmerl:export_simple(Data, xmerl_xml),
  {Doc, Req, State}.
