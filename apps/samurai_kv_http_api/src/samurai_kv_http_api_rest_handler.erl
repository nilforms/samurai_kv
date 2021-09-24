-module(samurai_kv_http_api_rest_handler).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom sword callbacks.
-export([katana/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, katana},
		{{<<"application">>, <<"x-ld-json">>, []}, odachi}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, katana},
		{{<<"application">>, <<"x-ld-json">>, []}, odachi}
	], Req, State}.

% Katana callback to process simple HTTP requests
katana(#{method := <<"POST">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
    samurai_kv:connect(Peer),
	{ok, Body, Req2} = cowboy_req:read_body(Req),
	Reply = try 
				{[{Key, Value}]} = jiffy:decode(Body),
				samurai_kv:insert(Peer, Key, Value)
			catch
				_:_:_ ->
					{error, <<"Wrong data have been provided">>}
			end,
	io:format("Reply ~p ~n", [Reply]),
	samurai_kv:disconnect(Peer),
	Resp = make_response(Reply, Req2),
	{stop, Resp, State};
katana(#{method :=<<"GET">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
    samurai_kv:connect(Peer),
	{ok, Body, Req2} = cowboy_req:read_body(Req),
	Reply = try 
				{[{<<"key">>, Key}]} = jiffy:decode(Body),
				samurai_kv:get(Peer, Key)
			catch
				_:_:_ ->
					{error, <<"Wrong data have been provided">>}
			end,
	io:format("Reply ~p ~n", [Reply]),
	samurai_kv:disconnect(Peer),
	Resp = make_response(Reply, Req2),
	{stop, Resp, State};
katana(Req, State)->
	Resp = make_response([], Req),
	{stop, Resp, State}.


%% Internal functions

make_response({ok, _} = Reply, Req) ->
	Rsp = cowboy_req:set_resp_body(jiffy:encode({[Reply]}),Req),
	cowboy_req:reply(200, Rsp);
make_response({error, _} = Reply, Req) ->
	Rsp = cowboy_req:set_resp_body(jiffy:encode({[Reply]}),Req),
	cowboy_req:reply(300, Rsp);
make_response(_, Req) ->
	cowboy_req:reply(400, Req).
