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

%%%-------------------------------------------------------------------
%% @doc samurai_kv public API
%% @end
%%%-------------------------------------------------------------------


-module(samurai_kv_app).

-include("samurai_kv.hrl").

-behaviour(application).

-export([
         start/2, 
         stop/1
         ]).

-spec start(StartType, StartArgs) -> Return when
	StartType :: application:start_type(), 
	StartArgs :: term(),
	Return    ::   {'ok', pid()} 
                 | {'ok', pid(), term()} 
                 | {'error', term()}.
 start(_StartType, _StartArgs) ->
    create_ets_tabs(),
    samurai_kv_sup:start_link().

-spec stop(State) -> ok when
	State :: term().
stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%% 
%%% INTERNAL FUNCTIONS
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%%%-------------------------------------------------------------------
-spec create_ets_tabs() -> ets:table().
%% @doc 
%% Creates ETS table needed for storage work.
%% 
%% @end
%%%-------------------------------------------------------------------
create_ets_tabs() ->
    ets:new(?storage_table,[set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]).