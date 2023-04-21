-module(exchange_rates_store).

-behaviour(gen_server).

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([get_rates/0]).

-include("include/exchange_rates.hrl").

-define(API_URL, "https://api.privatbank.ua/p24api/pubinfo?json&exchange&coursid=5").
-define(EXPIRY, 60).

-spec get_rates() -> {ok, [#exchange_rate{}]} | {error, term()}.
get_rates() ->
  gen_server:call(?MODULE, get_rates).

start_link() ->
  start_link(#{}).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

init(Options) ->
  Expiry = maps:get(expiry, Options, ?EXPIRY),
  ets:new(exchange_rates, [set, named_table, protected, {keypos, #exchange_rate.id}]),
  {ok, #{expiry => Expiry}}.

handle_call(get_rates, _From, State) ->
  case ets:tab2list(exchange_rates) of
    [] ->
      io:format("Rates are empty, requesting~n", []),
      case request_rates() of
        {ok, Rates} ->
          ets:insert(exchange_rates, Rates),
          drop_rates_after(expiry(State)),
          {reply, {ok, Rates}, State};
        {error, _} ->
          io:format("Error requesting rates, returning~n", []),
          {reply, {error, upstream_error}, State}
      end;
    Rates ->
      io:format("Rates are not empty, returning~n", []),
      {reply, {ok, Rates}, State}
  end;
handle_call(_Message, _From, State) ->
  {reply, {error, unknown_message}, State}.

handle_cast(drop_rates, State) ->
  io:format("Dropping rates~n", []),
  ets:delete_all_objects(exchange_rates),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

request_rates() ->
  case hackney:get(?API_URL, [], <<>>, [with_body]) of
    {ok, 200, _Headers, Body} ->
      exchange_rates_from_json(Body);
    {ok, _Status, _Headers, Body} ->
      {error, Body}
  end.

exchange_rates_from_json(Json) ->
  case jsx:decode(Json) of
    Rates when is_list(Rates) ->
      MapRate =
        fun({I, Rate}) ->
           #{<<"ccy">> := Ccy,
             <<"base_ccy">> := BaseCcy,
             <<"buy">> := Buy,
             <<"sale">> := Sale} =
             Rate,
           #exchange_rate{id = I,
                          ccy = Ccy,
                          base_ccy = BaseCcy,
                          buy = Buy,
                          sale = Sale}
        end,
      {ok, lists:map(MapRate, lists:enumerate(1, Rates))};
    Resp ->
      io:format("Unexpected response: ~p~n", [Resp]),
      {error, upstream_error}
  end.

drop_rates_after(Seconds) ->
  timer:apply_after(Seconds * 1000, gen_server, cast, [?MODULE, drop_rates]).

expiry(State) ->
  maps:get(expiry, State).
