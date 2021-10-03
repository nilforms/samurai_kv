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
%% @doc samurai_kv top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(samurai_kv_sup).

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
    Env = application:get_all_env(samurai_kv),
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                #{id => samurai_kv_storage_sup,       % mandatory
                    start => {samurai_kv_storage_sup, start_link, []},      % mandatory
                    restart => temporary,
                    shutdown => infinity,
                    type => supervisor,
                    modules => []},
                    #{id => samurai_kv_pool_worker,       % mandatory
                    start => {samurai_kv_pool_worker, start_link, [Env]},      % mandatory
                    restart => permanent,   % optional
                     shutdown => 5000, % optional
                     type => worker,       % optional
                     modules => []}  % optiona  
                    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions