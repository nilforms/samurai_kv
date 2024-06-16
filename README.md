samurai_kv
=====

Simple in-memory key-value storage engine written in Erlang. The storage uses Erlang Term Storage as a backend, and supports
concurrent access from different clients.

# Requirements
Currently tested on the following environment

- OS: Ubuntu 20.04
- OTP: 26
- rebar3: 3.23.0

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
    
MAX_KEYS - is a maximal possible number of keys (also limited by ets table size)

MAX_VAL_LEN - is a maximal possible value byte size

NUM_CONNECTIONS - is an upper limit of clients, which can connect to the storage concurrently

HTTP_PORT - is a listening port of HTTP reference point

# Example use

You can use the following commands to access the storage from linux command line:

* adding the KV pair:

```
curl -i --data-urlencode "key=foo" --data-urlencode "value=bar" -H "Accept: application/json" -X POST localhost:8181/api/keys/
```

* updating the KV-pair

```
curl -i --data-urlencode  --data-urlencode "value=bar" -H "Accept: application/json" -X PUT localhost:8181/api/keys/foo

``` 

* retrieval of specific KV-pair

```
curl -i -X GET localhost:8181/api/keys/foo
```

* retrieval of all KV-pairs

```
curl -i -X GET localhost:8181/api/keys/
```

* remove a KV-pair

```
curl -i -X DELETE localhost:8181/api/keys/foo
``` 