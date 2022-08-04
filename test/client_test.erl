-module(client_test).
-include_lib("eunit/include/eunit.hrl").

-define(USERNAME, "Username").
start_link_test() ->
	{ok, _CPID} = client:start_link(?USERNAME).

stop_test() ->
	?assertMatch(ok, client:stop(?USERNAME)).
