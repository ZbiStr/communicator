-module(communicator_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, erlangpol).
-define(NAME1, "name1").
-define(NAME2, "name2").
-define(PASSWORD, "password").
-define(BADPASSWORD, "badpassword").
-define(TIME, "12").
-define(MESSAGE, "message").
-define(MSGID, msg_id).
-define(ADDRESS1, address1).
-define(ADDRESS2, address2).


all_test_() ->
    {
        foreach,
        fun start_system/0,
        fun stop_system/1, 
        [
            fun login_without_pass/0,
            fun login_already_exists/0,
            fun set_password/0,
            fun login_with_correctpass/0,
            fun login_with_wrongpass/0,
            fun send_message/0,
            fun retry/0,
            fun find_user/0,
            fun show_active_users/0,
            fun find_password/0,
            fun confirm_mess_and_user_history/0,
            fun default/0
        ]
    }.

% SETUP

start_system() ->
    {ok, _} = communicator:start_link().

stop_system(_) ->
    ok = communicator:stop().

% TESTCASES
login_without_pass() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:logout(?NAME1).

login_already_exists() ->
	ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
	already_exists = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:logout(?NAME1).

set_password() -> 
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    ok = communicator:logout(?NAME1).

login_with_correctpass() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    ok = communicator:logout(?NAME1),
    ok = communicator:login(?NAME1, ?ADDRESS1, ?PASSWORD),
    ok = communicator:logout(?NAME1).

login_with_wrongpass() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    ok = communicator:logout(?NAME1),
    wrong_password = communicator:login(?NAME1, ?ADDRESS1, ?BADPASSWORD).

send_message() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:login(?NAME2, ?ADDRESS2, undefined),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(5),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),

    ok = communicator:set_password(?NAME2, ?PASSWORD),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(5),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),

    ok = communicator:logout(?NAME2),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:login(?NAME2, ?ADDRESS2, ?PASSWORD),
    timer:sleep(5),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),
    ok = communicator:logout(?NAME1),
    ok = communicator:logout(?NAME2).

retry() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:login(?NAME2, ?ADDRESS2, undefined),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(5),
    {communicator, get_node(?SERVER)} ! {msg_retry, ?MSGID},
    {communicator, get_node(?SERVER)} ! {msg_retry, {?MSGID, ?NAME2}},
    timer:sleep(5),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),
    ok = communicator:logout(?NAME1),
    ok = communicator:logout(?NAME2).
    
show_active_users() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    [?NAME1] = communicator:show_active_users().

find_password() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    undefined = communicator:find_password(?NAME1),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    defined = communicator:find_password(?NAME1).


find_user() ->
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:find_user(?NAME1),
    does_not_exist = communicator:find_user(?NAME2).

confirm_mess_and_user_history() ->
    ok = communicator:clear_whole_table(),
    ok = communicator:login(?NAME1, ?ADDRESS1, undefined),
    ok = communicator:login(?NAME2, ?ADDRESS2, undefined),
    ok = communicator:set_password(?NAME2, ?PASSWORD),
    [] = communicator:user_history(?NAME2),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(5),
    communicator:confirm(?MSGID),
    [{?TIME, ?NAME1, ?MESSAGE}] = communicator:user_history(?NAME2).
default() ->
    ok = gen_server:call({communicator, get_node(?SERVER)}, costam),
    gen_server:cast({communicator, get_node(?SERVER)}, costam),
    {communicator, get_node(?SERVER)} ! "blabla".

get_node(Name) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

