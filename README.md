# Erlbank Monolithic

Erlbank Legacy System

## Build

```
$ rebar3 compile
```


## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8000/

## Standalone Docker setup

```
cd .devcontainer
docker build -t erlang-flex .
```

Then start a shell in the the container with `run-docker.sh` or:

```
docker run --rm -p 8000:8000 -v PWD:/monolithic -w /monolithic -i -t erlang-flex bash
```

Replace `PWD` by the full path of the `monolithic` directory.

