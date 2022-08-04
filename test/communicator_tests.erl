-module(communicator_tests).
-include_lib("eunit/include/eunit.hrl").

-define(NAME1, user1).

start_test() ->
  ?assertMatch({ok, _PID}, communicator:start_link()).

login_test() ->
  ok = communicator:login(?NAME1, address),
  already_exists = communicator:login(?NAME1, address).

logout_test() ->
  ok = communicator:logout(?NAME1),
  does_not_exist = communicator:logout(?NAME1).
stop_test() ->
  ?assertMatch(ok, communicator:stop()).
