%%%-------------------------------------------------------------------
%% @doc samurai_kv storage worker.
%% @end
%%%-------------------------------------------------------------------

-module(samurai_kv_pool_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2]).
-export([insert/3,
         delete/2,
         connect/1,
         disconnect/1,
         get/2
]).

-define(SERVER, ?MODULE).

-record(state, {limit}).


connect(Client) ->
    gen_server:call(?SERVER,{connect, Client}).
disconnect(Client) ->
    gen_server:call(?SERVER, {disconnect, Client}).
insert(Client, Key, Value) when is_binary(Value) ->
    case ets:lookup(subscriptions, Client) of
        [] -> {error, <<"Client not connected">>};
        [{_, Worker}] ->
            gen_server:call(Worker,{insert, Key, Value})
    end;
insert(_, _, _) ->
    {error, <<"Wrong value format">>}.

delete(Client, Key) ->
    case ets:lookup(subscriptions, Client) of
        [] -> {error, <<"Client not connected">>};
        [{_, Worker}] ->
            gen_server:call(Worker,{delete, Key})
    end.
get(Client, Key) ->
    case ets:lookup(subscriptions, Client) of
        [] -> {error, <<"Client not connected">>};
        [{_, Worker}] ->
            gen_server:call(Worker, {get, Key})
    end.

start_link(Args) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, Args,[]). 

init(Args) ->
    Limit = case ets:info(subscriptions, size) of
        0 -> proplists:get_value(num_connections, Args);
        Num -> Num
    end,
    
    {ok,#state{limit = Limit}}.
handle_call({connect, _}, _From, #state{limit = 0} = State) ->
    {reply,{error, <<"Number of Clients exceeded">>}, State};

handle_call({connect, Client}, _From, #state{limit = Limit} = State) ->
    {Reply, NewLimit} = case ets:lookup(subscriptions, Client) of 
                            [] ->
                                {ok, W} = samurai_kv_storage_sup:start_worker(),
                                ets:insert(subscriptions, {Client, W}),
                                {{ok, <<"Client connected">>}, Limit-1};
                            _->
                                {{error, <<"Client already connected">>}, Limit}
                        end,
    {reply,Reply, State#state{limit = NewLimit}};
handle_call({disconnect, Client}, _From, #state{limit = Limit}=State) ->
    {Reply, NewState}  = case ets:lookup(subscriptions,Client) of 
        [] -> { {error, <<"Empty connection">>}, State};
        [{Client, W}] ->
            samurai_kv_storage_sup:stop_worker(W),
            ets:delete(subscriptions, Client),
            {{ok, <<"Client disconnected">>}, State#state{limit = Limit +1}}
    end, 
    {reply,Reply, NewState};
handle_call(Request, _From, State) ->
    logger:info("Request ~p  from ~p received", [Request,_From]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:info("Request ~p received", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    logger:info("Info ~p received", [Info]),
    {noreply, State}.

terminate(Reason, _) ->
    logger:info("Process ~p terminated with reason ~p", [?MODULE, Reason]),
    ok.