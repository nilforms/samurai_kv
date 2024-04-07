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

-export([
         add/2,
         update/2,
         delete/1,
         get/1,
         get_all/0
        ]).

add(Key, Value) ->
    samurai_kv_pool_worker:add(Key, Value).

update(Key, Value) ->
    samurai_kv_pool_worker:update(Key, Value).

delete(Key) ->
    samurai_kv_pool_worker:delete(Key).

get(Key) ->
    samurai_kv_pool_worker:get(Key).

get_all() ->
    samurai_kv_pool_worker:get_all().