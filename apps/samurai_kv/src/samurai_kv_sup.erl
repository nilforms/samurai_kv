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