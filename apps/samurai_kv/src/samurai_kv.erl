-module(samurai_kv).

-export([insert/3,
         delete/2,
         connect/1,
         disconnect/1,
         get/2
]).

connect(Peer) ->
    samurai_kv_pool_worker:connect(Peer).
disconnect(Peer) ->
    samurai_kv_pool_worker:disconnect(Peer).
insert(Peer, Key, Value) ->
    samurai_kv_pool_worker:insert(Peer, Key, Value).

delete(Peer, Key) ->
    samurai_kv_pool_worker:delete(Peer, Key).
get(Peer, Key) ->
    samurai_kv_pool_worker:get(Peer, Key).