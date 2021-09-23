-module(rest_handler).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([katana/2]).
% -export([paste_html/2]).
% -export([paste_text/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, katana},
		{{<<"application">>, <<"x-ld-json">>, []}, katana}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, katana},
		{{<<"application">>, <<"x-ld-json">>, []}, katana}
	], Req, State}.


katana(Req, State) ->
    io:format("Req ~p ~n", [Req]),
	Peer = cowboy_req:peer(Req),
    Conn = samurai_kv:connect(Peer),
    io:format("~p ~n", [Conn]),
    case cowboy_req:method(Req) of
		<<"POST">> ->
            % Insert = samurai_kv:insert(Peer, Key, Value)
            samurai_kv:disconnect(Peer),
			{<<"{\"ok\":\"rest\"}">>, <<"">>, State};
		_ ->
            samurai_kv:disconnect(Peer),
			{jiffy:encode({[Conn]}), Req, State}
	end.
