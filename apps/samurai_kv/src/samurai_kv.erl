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


-module(samurai_kv).

-include("samurai_kv.hrl").

-export([
         add/2,
         update/2,
         delete/1,
         get/1,
         get_all/0
        ]).

%%%-------------------------------------------------------------------
-spec add(Key, Value) -> Result when
    Key    :: binary(),
    Value  :: binary(),
    Result :: {ok, added}  | {error, Error},
    Error  :: already_exists | overload | oversize.
%% @doc
%% Adds new record to storage.
%% Returns error when:
%%  - key already exists
%%  - storage workers pool is overlaoded
%%  - storage exceeds size limits 
%%
%% @end
%%%-------------------------------------------------------------------
add(Key, Value) ->
	samurai_kv_pool_worker:add(Key, Value).

%%%-------------------------------------------------------------------
-spec update(Key, Value) -> Result when
	Key    :: binary(),
	Value  :: binary(),
	Result :: {ok, updated} | {error, Error},
	Error  :: overload | 'key not found'.
%% @doc
%% Updates existing record in storage.
%% Returns error when:
%%  - key does not exist
%%  - storage workers pool is overlaoded 
%%
%% @end
%%%-------------------------------------------------------------------
update(Key, Value) ->
	samurai_kv_pool_worker:update(Key, Value).

%%%-------------------------------------------------------------------
-spec delete(Key) -> Result when
	Key    :: binary(),
	Result :: {ok, deleted} | {error, overload}.
%% @doc
%% Removes record correpoding to input key from storage.
%% Does not return error, if key has not been found.
%% Returns error when:
%%  - storage exceeds size limits 
%%
%% @end
%%%-------------------------------------------------------------------
delete(Key) ->
	samurai_kv_pool_worker:delete(Key).

%%%-------------------------------------------------------------------
-spec get(Key) -> Result when
	Key    :: binary(),
	Result :: {ok, db_record()} | {error, Error},
	Error  :: no_key | overload.
%% @doc
%% Retrieves record correpoding to input key from storage.
%% Returns error when:
%%  - key does not exist
%%  - storage workers pool is overlaoded 
%%
%% @end
%%%-------------------------------------------------------------------
get(Key) ->
	samurai_kv_pool_worker:get(Key).

%%%-------------------------------------------------------------------
-spec get_all() -> Result when
	Result :: [db_record()] 
			| {result, storage_empty} 
			| {error, overload}.
%% @doc
%% Retrieves all exisitng recors from the storage.
%% Returns error when:
%%  - storage workers pool is overlaoded
%%
%% @end
%%%-------------------------------------------------------------------
get_all() ->
	samurai_kv_pool_worker:get_all().