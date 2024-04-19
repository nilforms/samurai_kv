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

-module(samurai_kv_storage_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
        start_worker/0,
        stop_worker/1
        ]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_worker()-> Return when
	Return :: supervisor:startchild_ret() | supervisor:startchild_err().
start_worker() ->
	Env = application:get_all_env(samurai_kv),
	supervisor:start_child(?MODULE, [Env]).

-spec stop_worker(Child) -> Return when
	Child  :: supervisor:child(),
	Return :: ok | {error, Error},
	Error  :: not_found | simple_one_for_one.
stop_worker(Child) ->
	supervisor:terminate_child(?MODULE, Child).

-spec init(Args) -> Return when
	Args       :: term(),
	SupFlags   :: supervisor:sup_flags(),
	ChildSpecs :: [supervisor:child_spec()],
	Return     :: ignore | {ok, {SupFlags, ChildSpecs}}.
init([]) ->
	SupFlags = #{strategy => simple_one_for_one,
				 intensity => 10,
				 period => 10},
	ChildSpecs = [
				  #{id => samurai_kv_storage_worker,      
				    start => {samurai_kv_storage_worker, start_link, []},
					restart => temporary, 
					shutdown => 5000, 
					type => worker,       
					modules => []}
				 ],
	{ok, {SupFlags, ChildSpecs}}.