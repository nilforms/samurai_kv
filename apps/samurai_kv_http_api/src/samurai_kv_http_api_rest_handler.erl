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
%%%-------------------------------------------------------------------
%% @doc 
%% samurai_kv rest hadler.
%% @end
%%%-------------------------------------------------------------------

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

-type state() :: #{max_len => non_neg_integer()}. 
-type json()  :: binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Cowboy REST Callbacks
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-spec init(Req, Opts) -> Result when
	Req    :: cowboy_req:req(),
	Opts   :: any,
	State  :: state(),
	Result :: {cowboy_rest, Req, State}.
init(Req, _Opts) ->
	{ok, Length} = application:get_env(samurai_kv_http_api, max_val_len),
	{cowboy_rest, Req, #{max_len => Length}}.

-spec allowed_methods(Req, State) -> Result when 
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {[binary()], Req, State}.
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

-spec content_types_accepted(Req, State) -> Return when 
	Req    	   :: cowboy_req:req(),
	State  	   :: state(),
	Result     :: [{binary() | ParsedMime, ProvideCallback :: atom()}],
	ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
	Params     :: [{Key :: binary(), Value :: binary()}],
	Return     :: {Result, Req, State}.
content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, url_to_samurai}
	 ], Req, State}.
	
-spec content_types_provided(Req, State) -> Return  when 
	Req    	   :: cowboy_req:req(),
	State  	   :: state(),
	Result     :: [{binary() | ParsedMime, ProvideCallback :: atom()}],
	ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params},
	Params     :: [{Key :: binary(), Value :: binary()}],
	Return     :: {Result, Req, State}.
content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, samurai_to_json}
	], Req, State}.

-spec valid_entity_length(Req, State) -> Return when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Return :: {boolean(), Req, State}.
valid_entity_length(Req, #{max_len := Length} = State) ->
	Result = cowboy_req:body_length(Req) < Length,
	{Result, Req, State}.

-spec resource_exists(Req, State) -> Result when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {boolean(), Req, State}.
resource_exists(#{method := Method, bindings := #{key := Key}} = Req, State) when Method == <<"GET">>;
																				  Method == <<"DELETE">> ->
	case samurai_kv:get(Key) of
		{ok, Record} ->
			{true, Req, State#{record => Record}};
		{error, overload} = Rsp ->
			respond_service_unavailable(Rsp, Req, State);
		{error, no_key} ->
			{false, Req, State}
	end;
resource_exists(#{method := Method} = Req, State) ->
	case cowboy_req:binding(key, Req) of
		undefined when Method =:= <<"DELETE">> ->
			{false, Req, State};
		_ ->
			{true, Req, State}
	end.

-spec delete_resource(Req, State) -> Result when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {boolean(), Req, State}.
delete_resource(#{bindings := #{key := Key}} = Req, State) ->
	samurai_kv:delete(Key),
	{true, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Custom Callbacks
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-spec url_to_samurai(Req, State) -> Result when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {boolean(), Req, State} | {stop, Reply, State},
	Reply  :: cowboy_req:req().
url_to_samurai(#{method := <<"POST">>} = Req, State) ->
	case cowboy_req:read_urlencoded_body(Req) of
		{ok, [{<<"key">>, Key},{<<"value">>, Value}], Req2} ->
			case samurai_kv:add(Key, Value) of 
				{ok, added} ->
					{{created, <<$/, Key/binary>>}, Req2, State};
				{error, already_exists} ->
					respond_not_modified(Req, State);
				Rsp when Rsp =:= {error, overload};
						 Rsp =:= {error, oversize} ->
					respond_service_unavailable(Rsp, Req, State)
				end;
		_ ->
			{false, Req, State}
	end;
url_to_samurai(#{method := <<"PUT">>, bindings := #{key := Key}} = Req, State) ->
	case cowboy_req:read_urlencoded_body(Req) of
		{ok, [{<<"value">>, Value}], Req2} ->
			case samurai_kv:update(Key, Value) of 
				{ok, updated} ->
					{true, Req2, State};
				{error, 'key not found'} = Rsp ->
					respond_not_found(Rsp, Req, State);
				{error, overload} = Rsp ->
					respond_service_unavailable(Rsp, Req, State)
				end;
		_ ->
			{false, Req, State}
	end;
url_to_samurai(Req, State)->
	{false, Req, State}.

-spec samurai_to_json(Req, State) -> Result when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {json(), Req, State}.
samurai_to_json(Req, #{record := Record} = State) ->
	{to_json(Record), Req, State};
samurai_to_json(Req, State) ->
	Reply = samurai_kv:get_all(),
	{to_json(Reply), Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%% 
%%% INTERNAL FUNCTIONS
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% In fact custom reponses below are dirty hacks, 
%% but without them everything would look much uglier

-spec respond_service_unavailable(Error, Req, State) -> Result when
	Error  :: {error, term()},
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {stop, Reply, State},
	Reply  :: cowboy_req:req().
respond_service_unavailable(Error, Req, State) ->
	respond_custom_error(503, Error, Req, State).

-spec respond_not_found(Error, Req, State) -> Result when
	Error  :: {error, term()},
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {stop, Reply, State},
	Reply  :: cowboy_req:req().
respond_not_found(Error, Req, State) ->
	respond_custom_error(404, Error, Req, State).

-spec respond_not_modified(Req, State) -> Result when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {stop, Reply, State},
	Reply  :: cowboy_req:req().
respond_not_modified(Req, State) ->
	respond_custom_error(304, <<>>, Req, State).
%%%-------------------------------------------------------------------
-spec respond_custom_error(Status, Error, Req, State) -> Result when
	Status :: cowboy:http_status(),
	Error  :: term(),
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {stop, Reply, State},
	Reply  :: cowboy_req:req().
%% @doc 
%% Creates non-standard erroneous response for an accept callback.
%% 
%% @end
%%%-------------------------------------------------------------------
respond_custom_error(Status, Error, Req, State) ->
	Resp = cowboy_req:reply(Status, #{}, to_json(Error), Req),
	{stop, Resp, State}.

%%%-------------------------------------------------------------------
-spec to_json(Term) -> Result when
	Term   :: binary() | tuple() | list(),
	Result :: json().

%% @doc 
%% Ensures safe conversion of term into JSON.
%% 
%% @end
%%%-------------------------------------------------------------------
to_json(Term) when is_tuple(Term) -> 
	to_json([Term]);
to_json(Term) when is_list(Term) ->
	case lists:all(fun is_tuple/1, Term) of
		true ->
			jiffy:encode({Term});
		false ->
			<<>>
	end;
to_json(_Term) ->
	<<>>.
