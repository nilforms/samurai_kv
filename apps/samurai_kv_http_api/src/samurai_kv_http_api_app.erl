% Copyright 2021, Shaikhrozy Zaidullin  <shaykhrozy@gmail.com>.

% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at

%     http://www.apache.org/licenses/LICENSE-2.0

% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

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
		{'_', [{"/", samurai_kv_http_api_rest_urlenc_handler, []},
			{"/cache", samurai_kv_http_api_rest_urlenc_handler, []},
			{"/cache_json", samurai_kv_http_api_rest_json_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(samurai_http, 
								[{port, Port}], 
								#{env => #{dispatch => Dispatch}, 
								protocols => [http|http2]}),
    samurai_kv_http_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
