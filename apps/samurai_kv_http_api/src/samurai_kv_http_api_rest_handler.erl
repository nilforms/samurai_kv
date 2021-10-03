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
-module(samurai_kv_http_api_rest_handler).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([known_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom sword callbacks.
-export([to_sword/2]).
-export([delete_resource/2]).


init(Req, Opts) ->
	[Option | _] = Opts,
	{cowboy_rest, Req, #{opt => Option}}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.
known_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>,
	<<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, to_sword},
		{{<<"application">>, <<"json">>, []}, delete_resource}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, to_sword}
	], Req, State}.

to_sword(Req, #{opt := on_demand} = State) ->
	katana(Req, State);
to_sword(Req, #{opt := stream} = State) ->
	odachi(Req, State);
to_sword(Req, State) ->
	io:format("State: ~p ~n", [State]),
	Resp = make_response([], Req),
	{stop, Resp, State}.

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

%% TODO: Create the stream response handler
%% which will push all KV pairs to clients in stream mode 

odachi(Req, State)->
	Resp = make_stream_response([], Req),
	{stop, Resp, State}.


delete_resource(#{method :=<<"DELETE">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
	samurai_kv:connect(Peer),
	{ok, Body, Req2} = cowboy_req:read_body(Req),
	Reply = try 
				{[{<<"key">>, Key}]} = jiffy:decode(Body),
				samurai_kv:delete(Peer, Key)
			catch
				_:_:_ ->
					{error, <<"Wrong data have been provided">>}
			end,
	samurai_kv:disconnect(Peer),
	Resp = make_response(Reply, Req2),
	{stop, Resp, State};
delete_resource(Req, State)->
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

make_stream_response({ok, Reply}, Req) ->
	Rsp = cowboy_req:set_resp_body(jiffy:encode({Reply}),Req),
	% cowboy_req:push(<<"">>, #{}, Rsp);
	cowboy_req:cast({inform, 200, Rsp}, Req);
make_stream_response({error, _} = Reply, Req) ->
	Rsp = cowboy_req:set_resp_body(jiffy:encode({[Reply]}),Req),
	cowboy_req:reply(300, #{},Rsp);
make_stream_response(_, Req) ->
	io:format("Haha"),
	cowboy_req:reply(400, #{},Req).
