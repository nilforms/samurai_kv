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

-export([insert/3,
         delete/2,
         connect/1,
         disconnect/1,
         get/2,
         get_all/1
]).

connect(Client) ->
    samurai_kv_pool_worker:connect(Client).
disconnect(Client) ->
    samurai_kv_pool_worker:disconnect(Client).
insert(Client, Key, Value) ->
    samurai_kv_pool_worker:insert(Client, Key, Value).

delete(Client, Key) ->
    samurai_kv_pool_worker:delete(Client, Key).
get(Client, Key) ->
    samurai_kv_pool_worker:get(Client, Key).
get_all(Client) ->
    samurai_kv_pool_worker:get_all(Client).