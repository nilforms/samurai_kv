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
-module(samurai_kv_http_api_rest_urlenc_handler).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([known_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom sword callbacks.
-export([odachi/2]).
-export([delete_resource/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.
known_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>,
	<<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, odachi},
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, delete_resource}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, odachi}
	], Req, State}.

% odachi callback to process simple HTTP requests
odachi(#{method := <<"POST">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
    samurai_kv:connect(Peer),
	Resp = try 
				{ok, [{<<"key">>,Key},{<<"value">>,Value}],Req2} = cowboy_req:read_urlencoded_body(Req),
				Reply = samurai_kv:insert(Peer, Key, Value),
				make_response(Reply, Req2)
			catch
				_:_:_ ->
					make_response({error, <<"Wrong data format">>}, Req)
			end,
	samurai_kv:disconnect(Peer),
	{stop, Resp, State};
odachi(#{method :=<<"GET">>, path := <<"/cache">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
    samurai_kv:connect(Peer),
	Resp = try 
				{ok, [{<<"key">>,Key}],Req2} = cowboy_req:read_urlencoded_body(Req),
				Reply = samurai_kv:get(Peer, Key),
				make_response(Reply, Req2)
			catch
				_:_:_ ->
					make_response({error, <<"Wrong data format">>}, Req)
			end,
	samurai_kv:disconnect(Peer),
	{stop, Resp, State};
odachi(#{method :=<<"GET">>, path := <<"/">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
    samurai_kv:connect(Peer),
	Reply = samurai_kv:get_all(Peer),
	Resp = make_response(Reply, Req),
	samurai_kv:disconnect(Peer),
	{stop, Resp, State};
odachi(Req, State)->
	Resp = make_response([], Req),
	{stop, Resp, State}.



delete_resource(#{method :=<<"DELETE">>} = Req, State) ->
	Peer = cowboy_req:peer(Req),
	samurai_kv:connect(Peer),
	Resp = try 
				{ok, [{<<"key">>,Key}],Req2} = cowboy_req:read_urlencoded_body(Req), 
				Reply =samurai_kv:delete(Peer, Key),
				make_response(Reply, Req2)
			catch
				_:_:_ ->
					make_response({error, <<"Wrong data format">>}, Req)
			end,
	{stop, Resp, State};
delete_resource(Req, State)->
	Resp = make_response([], Req),
	{stop, Resp, State}.


%% Internal functions

make_response({ok, Answer}, Req) when Answer =:= <<"Key added">>;
									  Answer =:= <<"Key changed">>;
									  Answer =:= <<"Key deleted">>->
	cowboy_req:reply(204, Req);
make_response({ok, Answer}, Req) ->
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, "application/json", Req),
	Resp = cowboy_req:set_resp_body(to_json(Answer),Req2),
	cowboy_req:reply(200, Resp);
make_response({error, Answer}, Req) when Answer =:= <<"Client not connected">>;
									   	 Answer =:= <<"Number of Clients exceeded">>;
										 Answer =:=  <<"Key doesn't exist">>->
	cowboy_req:reply(404, Req);
make_response({error, _}, Req) ->
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, "application/json", Req),
	Resp = cowboy_req:set_resp_body(<<"{\"error\":\"Bad Request\"}">>,
									Req2),
	cowboy_req:reply(400, Resp);
make_response(_, Req) ->
	cowboy_req:reply(500, Req).

to_json(Any) when is_binary(Any) -> jiffy:encode(Any);
to_json(Any) when is_tuple(Any) -> jiffy:encode({[Any]});
to_json(Any) when is_list(Any) -> jiffy:encode({Any});
to_json(_) ->  <<"">>.
