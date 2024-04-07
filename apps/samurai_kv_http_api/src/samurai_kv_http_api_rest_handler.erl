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
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([valid_entity_length/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

%% Custom callbacks.
-export([url_to_samurai/2]).
-export([samurai_to_json/2]).

init(Req, _Opts) ->
	{ok, Length} = application:get_env(samurai_kv_http_api, max_val_len),
	{cowboy_rest, Req, #{max_len => Length}}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, url_to_samurai}
	 ], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, samurai_to_json}
	], Req, State}.
valid_entity_length(Req, #{max_len := Length} = State) ->
	Result = cowboy_req:body_length(Req) < Length,
	{Result, Req, State}.


resource_exists(#{method   := <<"GET">>, 
				  bindings := #{key := Key}} = Req, State) ->
	case samurai_kv:get(Key) of
		{ok, Item} ->
			{true, Req, State#{item => Item}};
		{error, overload} = Rsp ->
			respond_service_unavailable(Rsp, Req, State);
		_Rsp ->
			{false, Req, State}
	end;
resource_exists(#{method := <<"DELETE">>} = Req, State)  ->
	case cowboy_req:binding(key, Req) of
		undefined ->
			{false, Req, State};
		Key ->
			{true, Req, State#{key_to_delete => Key}}
	end;
resource_exists(Req, State) ->
	{true, Req, State}.

delete_resource(Req, #{key_to_delete := Key} = State) ->
	samurai_kv:delete(Key),
	{true, Req, State}.
	
url_to_samurai(#{method := <<"POST">>} = Req, State) ->
	case cowboy_req:read_urlencoded_body(Req) of
		{ok, [{<<"key">>, Key},{<<"value">>, Value}], Req2} ->
			case samurai_kv:add(Key, Value) of 
				added ->
					{{created, <<$/, Key/binary>>}, Req2, State};
				already_exists ->
					respond_not_modified(Req, State);
				Rsp when Rsp =:= {error, overload};
						 Rsp =:= {error, oversize} ->
					respond_service_unavailable(Rsp, Req, State);
				_Rsp ->
					{false, Req2, State}
				end;
		_ ->
			{false, Req, State}
	end;
url_to_samurai(#{method := <<"PUT">>} = Req, State) ->
	case cowboy_req:read_urlencoded_body(Req) of
		{ok, [{<<"key">>, Key},{<<"value">>, Value}], Req2} ->
			case samurai_kv:update(Key, Value) of 
				updated ->
					{true, Req2, State};
				{error, overload} = Rsp ->
					respond_service_unavailable(Rsp, Req, State);
				_Rsp ->
					{false, Req2, State}
				end;
		_ ->
			{false, Req, State}
	end;
url_to_samurai(Req, State)->
	{false, Req, State}.
samurai_to_json(Req, #{item := Item} = State) ->
	{to_json(Item), Req, State};
samurai_to_json(Req, State) ->
	Reply = samurai_kv:get_all(),
	{to_json(Reply), Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% INTERNAL FUNCTIONS
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% In fact dirty hacks, but it would look much uglier
respond_service_unavailable(Error, Req, State) ->
	respond_non_standard_error(503, Error, Req, State).

respond_not_modified(Req, State) ->
	respond_non_standard_error(304, <<>>, Req, State).

respond_non_standard_error(Status, Error, Req, State) ->
	Req2 = cowboy_req:set_resp_body(to_json(Error), Req),
	Resp = cowboy_req:reply(Status, Req2),
	{stop, Resp, State}.

-spec to_json(Term) -> Result when
	Term   :: binary() | tuple() | list(),
	Result :: binary().
to_json(Term) when is_binary(Term) ->
	Term;
to_json(Term) when is_tuple(Term) -> 
	jiffy:encode({[Term]});
to_json(Term) when is_list(Term) ->
	case lists:all(fun is_tuple/1, Term) of
		true ->
			jiffy:encode({Term});
		false ->
			<<>>
	end;
to_json(_Term) ->
	<<>>.
