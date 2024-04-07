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
%% @doc samurai_kv storage worker.
%% @end
%%%-------------------------------------------------------------------

-module(samurai_kv_storage_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([
        init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2,
        handle_continue/2, 
        terminate/2
        ]).

-define(SERVER, ?MODULE).

-type state() :: #{max_keys => non_neg_integer()}.

start_link(Args) ->
    gen_server:start_link(?SERVER, Args,[]). 

-spec init(Args) -> Result when
    Args   :: list(),
    Result :: {ok, state()}.
init(Args) ->
    MaxKeys = proplists:get_value(max_keys, Args),
    {ok, #{max_keys => MaxKeys}}.

handle_call({add, _Key, _Value} = Request, From, #{max_keys := MaxKeys} = State) ->
    case ets:info(storage, size) < MaxKeys of
        true ->
            {noreply, State, {continue, {From, Request}}};
        false ->
            {stop, normal, {error, oversize}, State}
    end;
handle_call({update, Key, Value}, _From, State) ->
    ets:update_element(storage, Key, [{2, Value}]),
    {stop, normal, updated, State};
handle_call({delete, Key}, _From, State) ->
    ets:delete(storage, Key),
    {stop, normal, {ok, deleted}, State};
handle_call({get, Key}, _From, State) ->
    Reply = case ets:lookup(storage, Key) of
                [] ->
                    no_key;
                Item ->
                    {ok, Item}
            end,
    {stop, normal, Reply, State};
handle_call(get_all, _From, State) ->
    Reply = case ets:tab2list(storage) of
        [] -> {result, storage_empty};
        List -> List
    end,
    {stop, normal, Reply, State};
handle_call(Request, From, State) ->
    logger:info("Request ~p  from ~p received", [Request, From]),
    {noreply, State}.

handle_cast(Msg, State) ->
    logger:info("Request ~p received", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    logger:info("Info ~p received", [Info]),
    {noreply, State}.

handle_continue({From, {add, Key, Value}}, State) ->
    Reply = case ets:insert_new(storage, {Key, Value}) of
                true -> 
                    added;                     
                false ->
                    already_exists
            end,
    gen_server:reply(From, Reply),
    {stop, normal, State};
handle_continue(_Continue, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    logger:info("Process ~p terminated with reason ~p", [?MODULE, Reason]),
    ok.