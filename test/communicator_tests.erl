-module(communicator_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, erlangpol).
-define(NAME1, "name1").
-define(NAME2, "name2").
-define(NAME3, "name3").
-define(PASSWORD, "password").
-define(BADPASSWORD, "badpassword").
-define(TIME, "12").
-define(MESSAGE, "message").
-define(ADDRESS, address).


all_test_() ->
    {
        foreach,
        fun start_system/0,
        fun stop_system/1, 
        [
            fun login_without_pass/0,
            fun set_password/0,
            fun login_with_password/0,
            fun send_message/0,
            fun find_user/0,
            fun show_active_users/0,
            fun find_password/0,
            fun user_history/0
        ]
    }.

% SETUP

start_system() ->
    {ok, _} = communicator:start_link().

stop_system(_) ->
    ok = communicator:stop().

% TESTCASES

login_without_pass() ->
    CodedName = code_to_7_bits(?NAME1),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    %already_exists = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {logout, CodedName}).

set_password() -> 
    CodedName = code_to_7_bits(?NAME1),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    CodedPass = code_to_7_bits(?PASSWORD),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {password, CodedName, CodedPass}).

login_with_password() ->
    CodedName = code_to_7_bits(?NAME1),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    CodedPass = code_to_7_bits(?PASSWORD),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {password, CodedName, CodedPass}),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {logout, CodedName}),
    ok = gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, CodedPass}).

send_message() ->
    CodedFrom = code_to_7_bits(?NAME1),
    gen_server:call({communicator, get_node(?SERVER)}, {login, CodedFrom, undefined, undefined}),
    CodedTo = code_to_7_bits(?NAME2),
    gen_server:call({communicator, get_node(?SERVER)}, {login, CodedTo, undefined, undefined}),
    CodedTime = code_to_7_bits(?TIME),
	CodedMessage =code_to_7_bits(?MESSAGE),
    gen_server:cast({communicator, get_node(?SERVER)},{send_message, all, CodedTime, CodedFrom, CodedMessage}),
    gen_server:cast({communicator, get_node(?SERVER)},{send_message, CodedTo, CodedTime, CodedFrom, CodedMessage}).

show_active_users() ->
    gen_server:call({communicator, get_node(?SERVER)}, show_active_users).

find_password() ->
    CodedName = code_to_7_bits(?NAME1),
    gen_server:call({communicator, get_node(?SERVER)},{find_password, CodedName}).

find_user() ->
    CodedName = code_to_7_bits(?NAME1),
    gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    gen_server:call({communicator, get_node(?SERVER)}, {find_user, CodedName}).

user_history() ->
    CodedName = code_to_7_bits(?NAME1),
    gen_server:call({communicator, get_node(?SERVER)}, {login, CodedName, undefined, undefined}),
    gen_server:call({communicator, get_node(?SERVER)},{history, CodedName}).

get_node(Name) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

code_to_7_bits(Input) ->
	Bit = <<  <<(A-32)>> || A <- Input>>,
	<< <<Code>> || <<_A:1,Code:7>> <= Bit>>.
