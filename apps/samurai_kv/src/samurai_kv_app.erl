%%%-------------------------------------------------------------------
%% @doc samurai_kv public API
%% @end
%%%-------------------------------------------------------------------

-module(samurai_kv_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    create_ets_tabs(),
    samurai_kv_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

create_ets_tabs() ->
    ets:new(subscriptions, [set, named_table,public]),
    ets:new(storage,[set, named_table, public, {write_concurrency, true},{read_concurrency, true}]).