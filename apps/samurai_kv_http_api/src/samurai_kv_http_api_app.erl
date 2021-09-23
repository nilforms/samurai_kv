%%%-------------------------------------------------------------------
%% @doc samurai_kv_http_api public API
%% @end
%%%-------------------------------------------------------------------

-module(samurai_kv_http_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port),
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/test-api", rest_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}),
    samurai_kv_http_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
