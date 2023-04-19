-module(exchange_rates_handler).

-export([init/2, allowed_methods/2, content_types_provided/2]).
-export([to_xml/2]).

-include("include/exchange_rates.hrl").

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"xml">>, []}, to_xml}], Req, State}.

to_xml(Req, State) ->
  case exchange_rates_store:get_rates() of
    {ok, Rates} ->
      Rows = lists:map(fun make_rate_row/1, Rates),
      Resp = make_xml([{exchangerates, Rows}]),
      {Resp, Req, State};
    {error, _} ->
      Resp = make_xml([{error, [{message, "Internal server error"}], []}]),
      {stop, cowboy_req:reply(500, #{}, Resp, Req), State}
  end.

make_xml(Content) ->
  xmerl:export_simple(Content, xmerl_xml).

make_rate_row(Rate) ->
  #exchange_rate{ccy = Ccy,
                 base_ccy = BaseCcy,
                 buy = Buy,
                 sale = Sale} =
    Rate,
  {row,
   [{exchangerate,
     [{ccy, Ccy}, {base_ccy, BaseCcy}, {buy, Buy}, {sale, Sale}],
     []}]}.
