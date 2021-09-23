-module(samurai_kv_storage_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([start_worker/0,
        stop_worker/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker() ->
    Env = application:get_all_env(samurai_kv),
    supervisor:start_child(?MODULE, [Env]).
  
  %%-spec stop_handler(pid()) -> ok | {error, not_found}.
stop_worker(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
  
  
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [#{id => samurai_kv_storage_worker,       % mandatory
                 start => {samurai_kv_storage_worker, start_link, []},      % mandatory
                 restart => temporary,   % optional
                 shutdown => 5000, % optional
                 type => worker,       % optional
                 modules => []}],
    {ok, {SupFlags, ChildSpecs}}.