%%%-------------------------------------------------------------------
%% @doc exchange_api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(exchange_api_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags =
    #{strategy => one_for_one,
      intensity => 5,
      period => 60},
  ExchangeRatesStore =
    #{id => exchange_rates_store, start => {exchange_rates_store, start_link, []}},
  {ok, {SupFlags, [ExchangeRatesStore]}}.

%% internal functions
