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

-include("samurai_kv.hrl").

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

%% State of the storage worker
-type state() :: #{max_keys => non_neg_integer()}.

start_link(Args) ->
    gen_server:start_link(?SERVER, Args,[]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Gen Server Callbacks
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Args) -> Return when
    Args   :: term(),
    Return :: {ok, state()}.
init(Args) ->
    MaxKeys = proplists:get_value(max_keys, Args),
    {ok, #{max_keys => MaxKeys}}.

-spec handle_call(Request, From, State) -> Return when
    Request :: term(),
    From    :: gen_server:from(),
    State   :: state(),
    Return  :: {reply, term(), state()}
            | {noreply, state(),{continue, term()}}
            | {stop, term(), term(), state()}.
handle_call({add, _Key, _Value} = Request, From, #{max_keys := MaxKeys} = State) ->
    case ets:info(?storage_table, size) < MaxKeys of
        true ->
            {noreply, State, {continue, {From, Request}}};
        false ->
            {stop, normal, #{error => <<"storage size limit exceeded">>}, State}
    end;
handle_call({update, Key, Value}, _From, State) ->
    Reply = case ets:update_element(?storage_table, Key, [{2, Value}]) of
                true  -> #{key => Key, message => <<"key updated">>};
                false -> #{key => Key, error => <<"key not found">>}
            end,
    {stop, normal, Reply, State};
handle_call({delete, Key}, _From, State) ->
    ets:delete(?storage_table, Key),
    {stop, normal, #{key => Key, message => <<"key deleted">>}, State};
handle_call({get, Key}, _From, State) ->
    Reply = case ets:lookup(?storage_table, Key) of
                [] ->
                    #{key => Key, error => <<"key not found">>};
                [{Key, Value}] ->
                    #{key => Key, value => Value}
            end,
    {stop, normal, Reply, State};
handle_call(get_all, _From, State) ->
    Reply = case ets:tab2list(?storage_table) of
        [] -> #{message => <<"storage empty">>};
        List -> [#{key => K, value => V} || {K, V} <- List]
    end,
    {stop, normal, Reply, State};
handle_call(Request, From, State) ->
    logger:info("Request ~p  from ~p received", [Request, From]),
    {noreply, State}.

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
handle_info(Info, State) ->
    logger:info("Info ~p received", [Info]),
    {noreply, State}.

-spec handle_continue(Continue, State) -> Return when
    Continue   :: term(),
    State      :: state(),
    Return     :: {noreply, state()} | {stop, term(), state()}.
handle_continue({From, {add, Key, Value}}, State) ->
    Reply = case ets:insert_new(?storage_table, {Key, Value}) of
                true -> 
                    #{key => Key, message => <<"key added">>};                     
                false ->
                    #{key => Key, error => <<"key already exists">>}
            end,
    gen_server:reply(From, Reply),
    {stop, normal, State};
handle_continue(_Continue, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State  :: state().
terminate(Reason, _State) ->
    logger:info("Process ~p terminated with reason ~p", [?MODULE, Reason]),
    ok.