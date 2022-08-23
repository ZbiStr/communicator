-module(communicator_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, erlangpol).
-define(NAME, "name1").
-define(PASSWORD, "pass").
-define(ADDRESS, address).


all_test_() ->
    {
        foreach,
        fun start_system/0,
        fun stop_system/1, 
        [
            fun login_without_pass/0,
            fun set_password/0,
            fun login_with_password/0
        ]
    }.

% SETUP

start_system() ->
    {ok, _} = communicator:start_link().

stop_system(_) ->
    ok = communicator:stop().

% TESTCASES

login_without_pass() ->
    CodedName = code_to_7_bits(?NAME),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    already_exists = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {logout, CodedName}).

set_password() -> 
    CodedName = code_to_7_bits(?NAME),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    CodedPass = code_to_7_bits(?PASSWORD),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {password, CodedName, CodedPass}).

login_with_password() ->
    CodedName = code_to_7_bits(?NAME),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    CodedPass = code_to_7_bits(?PASSWORD),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {password, CodedName, CodedPass}),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {logout, CodedName}),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, CodedPass}).











get_node(Name) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

code_to_7_bits(Input) ->
	Bit = <<  <<(A-32)>> || A <- Input>>,
	<< <<Code>> || <<_A:1,Code:7>> <= Bit>>.

decode_from_7_bits(Input) ->
	Bit = << <<0:1,Code:7>> || <<Code>> <= Input>>,
	[(A+32) || <<A:8>> <= Bit].
