-module(communicator_tests).
-include_lib("eunit/include/eunit.hrl").

-define(NAME1, user1).


login_test() ->
  {ok, _Pid} = communicator:start_link(),
  ok = communicator:login(?NAME1, address),
  already_exists = communicator:login(?NAME1, address),
  stopped = communicator:stop().

logout_test() ->
  {ok, _Pid} = communicator:start_link(),
  ok = communicator:login(?NAME1, address),
  ok = communicator:logout(?NAME1),
  do_not_exist = communicator:logout(?NAME1),
  stopped = communicator:stop().

    
