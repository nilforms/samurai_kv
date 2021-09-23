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


connect(Peer) ->
    gen_server:call(?SERVER,{connect, Peer}).
disconnect(Peer) ->
    gen_server:call(?SERVER, {disconnect, Peer}).
insert(Peer, Key, Value) ->
    case ets:lookup(subscriptions, Peer) of
        [] -> {error, <<"Peer not connected">>};
        [{_, Worker}] ->
            gen_server:call(Worker,{insert, Key, Value})
    end.

delete(Peer, Key) ->
    case ets:lookup(subscriptions, Peer) of
        [] -> {error, <<"Peer not connected">>};
        [{_, Worker}] ->
            gen_server:call(Worker,{delete, Key})
    end.
get(Peer, Key) ->
    case ets:lookup(subscriptions, Peer) of
        [] -> {error, <<"Peer not connected">>};
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
    {reply,{error, <<"Number of peers exceeded">>}, State};

handle_call({connect, Peer}, _From, #state{limit = Limit} = State) ->
    {ok, W} = samurai_kv_storage_sup:start_worker(),
    ets:insert(subscriptions, {Peer, W}),
    {reply,{ok, <<"Peer connected">>}, State#state{limit = Limit -1}};
handle_call({disconnect, Peer}, _From, #state{limit = Limit}=State) ->
    {Reply, NewState}  = case ets:lookup(subscriptions,Peer) of 
        [] -> { {error, <<"Empty connection">>}, State};
        [{Peer, W}] ->
            samurai_kv_storage_sup:stop_worker(W),
            ets:delete(subscriptions, Peer),
            {{ok, <<"Peer disconnected">>}, State#state{limit = Limit +1}}
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