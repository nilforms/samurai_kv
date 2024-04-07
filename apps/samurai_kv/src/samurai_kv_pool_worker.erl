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

-behaviour(gen_server).

-export([start_link/1]).

-export([
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2
        ]).
-export([
         add/2,
         update/2,
         delete/1,
         get/1,
         get_all/0
        ]).

-define(SERVER, ?MODULE).

add(Key, Value) when is_binary(Value) ->
    procedure(?FUNCTION_NAME, [Key, Value]);   
add(_Key, _Value) ->
    {error, format}.

update(Key, Value) when is_binary(Value) ->
    procedure(?FUNCTION_NAME, [Key, Value]);   
update(_Key, _Value) ->
    {error, format}.

delete(Key) ->
    procedure(?FUNCTION_NAME, [Key]).

get(Key) ->
    procedure(?FUNCTION_NAME, [Key]).
get_all() ->
    procedure(?FUNCTION_NAME, []).

start_link(Args) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, Args,[]). 

init(Args) ->
    Limit = proplists:get_value(num_connections, Args),
    {ok, #{limit => Limit}}.

handle_call(get_worker, _From, #{limit := 0} = State) ->
    {reply, {error, overload}, State};
handle_call(get_worker, _From, #{limit := Limit} = State) ->
    Reply = samurai_kv_storage_sup:start_worker(),
    {reply, Reply, State#{limit => Limit - 1}};
handle_call(Request, From, State) ->
    logger:info("Request ~p  from ~p received", [Request, From]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:info("Request ~p received", [Msg]),
    {noreply, State}.

handle_info({'DOWN', _Mref, process, _W, normal}, #{limit := Limit} = State) ->
    {noreply, State#{limit => Limit + 1}};
handle_info({'DOWN', Mref, process, W, _Reason}, #{limit := Limit} = State) ->
    erlang:demonitor(Mref),
    samurai_kv_storage_sup:stop_worker(W),
    {noreply, State#{limit => Limit + 1}};
  
handle_info(Info, State) ->
    logger:info("Info ~p received", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    logger:info("Process ~p terminated with reason ~p", [?MODULE, Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec procedure(Method, Args) -> Result when
    Method :: function(),
    Args   :: tuple(),
    Result :: {ok, term()} | {error, term()}.
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