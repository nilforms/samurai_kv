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
%% @doc samurai_kv storage pool worker.
%% @end
%%%-------------------------------------------------------------------

-module(samurai_kv_pool_worker).

-include("samurai_kv.hrl").

-behaviour(gen_server).

%% Gen Server callbacks
-export([start_link/1]).
-export([
		init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2
		]).
%% API functions
-export([
		add/2,
		update/2,
		delete/1,
		get/1,
		get_all/0
		]).

-define(SERVER, ?MODULE).

%% State of the Pool dispatcher
-type state() :: #{limit => non_neg_integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% API
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add(Key, Value) -> Result when
    Key    :: binary(),
    Value  :: binary(),
    Result :: {ok, added}  | {error, Error},
    Error  :: already_exists | overload | oversize.
add(Key, Value) ->
	procedure(?FUNCTION_NAME, [Key, Value]).   

-spec update(Key, Value) -> Result when
	Key    :: binary(),
	Value  :: binary(),
	Result :: {ok, updated} | {error, Error},
	Error  :: overload | 'key not found'.
update(Key, Value) ->
	procedure(?FUNCTION_NAME, [Key, Value]).   

-spec delete(Key) -> Result when
	Key    :: binary(),
	Result :: {ok, deleted} | {error, overload}.
delete(Key) ->
	procedure(?FUNCTION_NAME, [Key]).

-spec get(Key) -> Result when
	Key    :: binary(),
	Result :: {ok, db_record()} | {error, Error},
	Error  :: no_key | overload.
get(Key) ->
	procedure(?FUNCTION_NAME, [Key]).

-spec get_all() -> Result when
	Result :: [db_record()] 
			| {result, storage_empty} 
			| {error, overload}.
get_all() ->
	procedure(?FUNCTION_NAME, []).

-spec start_link(Args) -> StartRet when
	Args     :: term(),
	StartRet :: gen_server:start_ret().
start_link(Args) ->
	gen_server:start_link({local,?SERVER}, ?MODULE, Args,[]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Gen Server Callbacks
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Args) -> Return when
    Args   :: term(),
    Return :: {ok, state()}.
init(Args) ->
	Limit = proplists:get_value(num_connections, Args),
	{ok, #{limit => Limit}}.

-spec handle_call(Request, From, State) -> Return when
	Request :: term(),
	From    :: gen_server:from(),
	State   :: state(),
	Return  :: {reply, term(), state()}.
handle_call(get_worker, _From, #{limit := 0} = State) ->
	{reply, {error, overload}, State};
handle_call(get_worker, _From, #{limit := Limit} = State) ->
	Reply = samurai_kv_storage_sup:start_worker(),
	{reply, Reply, State#{limit => Limit - 1}};
handle_call(Request, From, State) ->
	logger:info("Request ~p  from ~p received", [Request, From]),
	{reply, ok, State}.

-spec handle_cast(Msg, State) -> Return when
	Msg    :: term(),
	State  :: state(),
	Return :: {noreply, state()}.
handle_cast(Msg, State) ->
	logger:info("Request ~p received", [Msg]),
	{noreply, State}.

-spec handle_info(Info, State) -> Return when
	Info   :: term(),
	State  :: state(),
	Return :: {noreply, state()}.
handle_info({'DOWN', _Mref, process, _W, normal}, #{limit := Limit} = State) ->
	{noreply, State#{limit => Limit + 1}};
handle_info({'DOWN', Mref, process, W, _Reason}, #{limit := Limit} = State) ->
	erlang:demonitor(Mref),
	samurai_kv_storage_sup:stop_worker(W),
	{noreply, State#{limit => Limit + 1}};
handle_info(Info, State) ->
	logger:info("Info ~p received", [Info]),
	{noreply, State}.

-spec terminate(Reason, State) -> ok when
	Reason :: term(),
	State  :: state().
terminate(Reason, _State) ->
	logger:info("Process ~p terminated with reason ~p", [?MODULE, Reason]),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Internal Functions
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------------------------------------------------------------------
-spec procedure(Method, Args) -> Result when
	Method :: any(),
	Args   :: [binary()],
	Result :: {ok, term()} | {error, term()}.
%% @doc
%% Performs DB procedure corresponding to certain method
%% 
%% @end
%%%-------------------------------------------------------------------
procedure(Method, Args) ->
	Request = case Args of
		[] -> 
			Method;
		ArgList ->
			list_to_tuple([Method] ++ ArgList)
		end,
	case gen_server:call(?SERVER, get_worker) of
		{ok, W} ->
			gen_server:call(W, Request);
		Msg ->
			Msg
	end.