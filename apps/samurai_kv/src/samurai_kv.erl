-module(samurai_kv).

-export([insert/3,
         delete/2,
         connect/1,
         disconnect/1,
         get/2
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