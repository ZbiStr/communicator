-module(communicator_tests).
-include_lib("eunit/include/eunit.hrl").


all_test_() ->
    {
        foreach,
        fun start_system/0,
        fun stop_system/1, 
        [
        ]
    }.

% SETUP

start_system() ->
    {ok, _} = communicator:start_link().

stop_system(_) ->
    ok = communicator:stop().

% TESTCASES