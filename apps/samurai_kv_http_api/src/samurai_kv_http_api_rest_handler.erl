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
		{{<<"application">>, <<"json">>, '*'}, samurai_to_json},
		{{<<"application">>, <<"problem+json">>, '*'}, url_to_samurai},
		{{<<"application">>, <<"problem+json">>, '*'}, resource_exists}
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
	Rsp = samurai_kv:get(Key),
	case Rsp of
		#{value := _Val} ->
			{true, Req, State#{record => Rsp}};
		#{error := <<"too many connections">>} ->
			respond_service_unavailable(Rsp, Req, State);
		#{error := _Error} ->
			{false, Req#{resp_body => to_json(Rsp)}, State}
	end;
resource_exists(#{method := Method} = Req, State) ->
	case cowboy_req:binding(key, Req) of
		undefined when Method =:= <<"DELETE">> ->
			RespBody = to_json(#{error => "key not porvided"}),
			{false, Req#{resp_body => RespBody}, State};
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
			Rsp = samurai_kv:add(Key, Value),
			case Rsp of 
				#{message := <<"key added">>} ->
					{{created, <<>>}, Req2#{resp_body => to_json(Rsp)}, State};
				#{error := <<"key already exists">>} ->
					respond_not_modified(Req, State);
				#{error := _Error} ->
					respond_service_unavailable(Rsp, Req, State)
				end;
		_ ->
			RespBody = to_json(#{error => "wrong body format"}),
			{false, Req#{resp_body => RespBody}, State}
	end;
url_to_samurai(#{method := <<"PUT">>, bindings := #{key := Key}} = Req, State) ->
	case cowboy_req:read_urlencoded_body(Req) of
		{ok, [{<<"value">>, Value}], Req2} ->
			Rsp = samurai_kv:update(Key, Value),
			case Rsp of 
				#{message := <<"key updated">>} ->
					{true, Req2#{resp_body => to_json(Rsp)}, State};
				#{error := <<"key not found">>} ->
					respond_not_found(Rsp, Req, State);
				#{error := _Error} ->
					respond_service_unavailable(Rsp, Req, State)
				end;
		_ ->
			RespBody = to_json(#{error => "wrong body format"}),
			{false, Req#{resp_body => RespBody}, State}
	end;
url_to_samurai(Req, State)->
	respond_im_teapot(#{error => "you're a teapot ^_^"}, Req, State).

-spec samurai_to_json(Req, State) -> Result when
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {json(), Req, State}.
samurai_to_json(Req, #{record := Record} = State) ->
	{to_json(Record), Req, State};
samurai_to_json(Req, State) ->
	Rsp = samurai_kv:get_all(),
	Reply = case Rsp of
		#{message := _Message}->
			Rsp;
		Records ->
			Records
		end,
	{to_json(Reply), Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%% 
%%% INTERNAL FUNCTIONS
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% In fact custom reponses below are dirty hacks, 
%% but without them everything would look much uglier

-spec respond_service_unavailable(Error, Req, State) -> Result when
	Error  :: map(),
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {stop, Reply, State},
	Reply  :: cowboy_req:req().
respond_service_unavailable(Error, Req, State) ->
	respond_custom_error(503, Error, Req, State).

-spec respond_im_teapot(Error, Req, State) -> Result when
	Error  :: map(),
	Req    :: cowboy_req:req(),
	State  :: state(),
	Result :: {stop, Reply, State},
	Reply  :: cowboy_req:req().
respond_im_teapot(Error, Req, State) ->
	respond_custom_error(418, Error, Req, State).
-spec respond_not_found(Error, Req, State) -> Result when
	Error  :: map(),
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
	Resp = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/problem+json">>}, to_json(Error), Req),
	{stop, Resp, State}.

%%%-------------------------------------------------------------------
-spec to_json(Term) -> Result when
	Term   :: term(),
	Result :: json().

%% @doc 
%% Ensures safe conversion of term into JSON.
%% 
%% @end
%%%-------------------------------------------------------------------
to_json(Term) when is_tuple(Term) -> 
	jiffy:encode({[Term]});
to_json(Term) when is_map(Term) ->
	jiffy:encode(Term);
to_json(Term) when is_list(Term) ->
	TermToEncode = case lists:all(fun is_tuple/1, Term) of
						true ->
							{Term};
						false ->
							Term
				end,
	jiffy:encode(TermToEncode);
to_json(_Term) ->
	<<>>.
