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