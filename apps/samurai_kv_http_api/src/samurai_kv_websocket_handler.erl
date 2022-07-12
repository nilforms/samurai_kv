-module(samurai_kv_websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% we receive State from cowbow_router:compile (see yaptool_app.erl)
init(#{peer := Peer} = Req, _Opts) ->
	io:format("hello req in ~p with Peer: ~p and initial State: ~p  ~n", [self(), Peer, _Opts]),
	Opts = #{idle_timeout => 1000, peer => Peer},
    samurai_kv:connect(Peer),
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	io:format("websocket init in pid ~p with State ~p  ~n", [self(), State]),

	erlang:start_timer(1000, self(), <<"{\"alarm\":\"bazarm\"}">>),
	{[], State}.

websocket_handle(_Data, State) ->
    io:format("~p ~n", [_Data]),
	{[], State}.

websocket_info({timeout, _Ref, Msg}, #{peer := Peer} =State) ->
	erlang:start_timer(5000, self(), samurai_kv:get_all(Peer)),	
	{[{text, Msg}], State};

websocket_info(_Info, State) ->
	io:format("222 hello received in ~p Info ~p  ~n", [self(), _Info]),
	{reply, {text, <<"Hello!">>}, State}.

terminate(timeout,_, State) ->
    case maps:get(peer, State) of
        undefined -> ok;
        Peer -> samurai_kv:disconnect(Peer)
    end,
ok;

%% WS close codes
%% https://tools.ietf.org/html/rfc6455#section-7.4.1
%%
%% 1001
%%
%% 1001 indicates that an endpoint is "going away", such as a server
%% going down or a browser having navigated away from a page.

%% termination due to browser closed
terminate({error, closed}, _PartialReq, _State) ->
    case maps:get(peer,_State) of
        undefined -> ok;
        Peer -> samurai_kv:disconnect(Peer)
    end,
	logger:info("~p: terminate because browser closed", [?MODULE]),
	ok;

%% also when close browser
terminate({remote, 1001, <<>>}, _PartialReq, _State) ->
	case maps:get(peer,_State) of
        undefined -> ok;
        Peer -> samurai_kv:disconnect(Peer)
    end,
    logger:info("~p: terminate because browser closed", [?MODULE]),
	ok;

terminate(Reason, PartialReq, State) ->
    case maps:get(peer, State) of
        undefined -> ok;
        Peer -> samurai_kv:disconnect(Peer)
    end,
	logger:info("~p terminate with Reason : ~p, PartialReq: ~p and State : ~p", [?MODULE, Reason, PartialReq, State]),
	ok.      %% optional
