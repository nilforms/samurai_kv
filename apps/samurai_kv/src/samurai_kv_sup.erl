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

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(Args) -> Return when
	Args       :: term(),
	SupFlags   :: supervisor:sup_flags(),
	ChildSpecs :: [supervisor:child_spec()],
	Return     :: ignore | {ok, {SupFlags, ChildSpecs}}.
init([]) ->
	Env = application:get_all_env(samurai_kv),
	SupFlags = #{strategy => one_for_one,
				 intensity => 0,
				 period => 1},
	ChildSpecs = [
					#{id => samurai_kv_storage_sup,       
					 start => {samurai_kv_storage_sup, start_link, []},      
					 restart => temporary,
					 shutdown => infinity,
					 type => supervisor,
					 modules => []},
					#{id => samurai_kv_pool_worker,       
					 start => {samurai_kv_pool_worker, start_link, [Env]},     
					 restart => permanent,  
					 shutdown => 5000, 
					 type => worker,    
					 modules => []}   
					],
	{ok, {SupFlags, ChildSpecs}}.