samurai_kv
=====

Simple in-memory key-value storage engine written in erlang. The storage uses Erlang Term Storage as a base, and supports multiple
councrrent access from defferent clients.

# Build

In order to build the application run the following command:

``` 
$ rebar3 as prod release
```
# Start of the application

An example of the application startup is as follows:

```
MAX_KEYS=1000 MAX_VAL_LEN=10240 NUM_CONNECTIONS=1000 HTTP_PORT=8181 path-to-samurai-kv/samurai_kv start
```
where 
    
    MAX_KEYS - is maximal possible number of keys (also limited by ets table size)
    
    MAX_VAL_LEN - is maximal possible value byte size
    
    NUM_CONNECTIONS - is an upper limit of clients, which can connect to the storage concurrently
    
    HTTP_PORT - is a listening port of HTTP reference point

# Accessing storage

## Erlang API

The functions of the storage access and management are presented in samurai_kv.erl module:
```
connect(Client) - connects the Client to storage
insert(Client, Key, Value) - adds new Key-Value pair to storage or updates Value of exisitng key, if Client is connected
delete(Client, Key) - removes the Key from storage, if Client is connected
get(Client, Key) - returns Key from storage if Key exists, and if Client is connected
get_all(Client) - returns all Key-Value pairs for Client
disconnect(Client) - disconnects Client from storage 
```

## HTTP API

HTTP API allows insert and get spesific keys from storage in on-demand manner. The client is automatically connected to storage
via peer (ip-address and port), and automatically disconnected after access procedure evaluation. 

The key-value pair may be stored as follows:

```
curl -d '{"nam":"linux"}' -N -H "Content-Type:application/json" -X POST  -i  http://localhost:8181/test-api/
```
    where {"nam":"linux"} - is an exmaple JSON with key-value pair to be stored.

Access to value of specific key may be got as follows

```
curl -s -N -d '{"key":"nam"}' -H "Content-Type:application/json" -X GET  -i  http://localhost:8181/test-api
```
    where "nam" is an example name of key to be accessed. Remember, that keyword "key" is mandatory when accessing 
    specific key.

To remove a specific key from strorage do the following:

```
curl -s -N -d '{"key":"nam"}' -H "Content-Type:application/json" -X DELETE  -i  http://localhost:8181/test-api
``` 
    where "nam" is an example name of key to be accessed. Remember, that keyword "key" is mandatory when accessing 
    specific key.
