samurai_kv
=====

Simple in-memory key-value database engine written in erlang

Build

```
$ rebar3 as prod release
````

```
MAX_KEYS=1000 MAX_VAL_LEN=10240 NUM_CONNECTIONS=1000 HTTP_PORT=8181 path-to-samurai-kv/samurai_kv start
```