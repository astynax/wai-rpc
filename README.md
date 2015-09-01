# WAI RPC

This project shows how to implement a simple WebApp (RPC-server) with WAI.

## Building:

```shell
$ stack build
```

## Running

```shell
$ stack exec wai-rpc
Serving (hit Ctrl+C to stop)...
```

## API

```shell
$ curl http://localhost:8000
Available functions:
reverse
upper
$ curl http://localhost:8000/upper
returns string with each character in upper case
$ curl http://localhost:8000/upper?hello%20world
HELLO WORLD
```

