# Building and setup instruction

The documents demonstrates building and setup instructions.
>**Disclaimer:** All commands should be executed in the folder that contains the project.

## Building instruction

To build, the system uses rebar3 and OTP 24.3.4.

To build the system, execute the following commands in the command line:
```erlang 
rebar3 do compile
```

## Setup instruction

To start the server, double click the 'run_server.sh' file.

To start the client, double click the 'run_client.sh' file.

>**Disclaimer:** In order for the app to work properly, the server and the client **must**  be running on separate terminals. The server should be executed first.

## Test run instruction

To run tests, execute the following:
```erlang
rebar3 do clean, compile, eunit --dir=test, cover --verbose
```