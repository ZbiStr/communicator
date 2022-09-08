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
-define(LOGIN_TIME, "login_time").
-define(LOGOUT_TIME, "logout_time").


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
            fun list_of_users/0,
            fun find_password/0,
            fun confirm_mess_and_user_history/0,
            fun default/0,
            fun automatic_logout/0
        ]
    }.

% SETUP

start_system() ->
    ok = communicator:clear_whole_table(server_status, "server_status"),
    timer:sleep(20),
    {ok, _} = communicator:start_link().

stop_system(_) ->
    ok = communicator:stop().

% TESTCASES
login_without_pass() ->
    {ok,_} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME).

login_already_exists() ->
	{ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
	already_exists = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME).

set_password() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME).

login_with_correctpass() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME),
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, ?PASSWORD, ?LOGIN_TIME),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME).

login_with_wrongpass() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME),
    wrong_password = communicator:login(?NAME1, ?ADDRESS1, ?BADPASSWORD, ?LOGIN_TIME).

automatic_logout() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    timer:sleep(10),
    {communicator, get_node(?SERVER)} ! {afk_time, ?NAME1},
    timer:sleep(10),
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME).

send_message() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    {ok, _} = communicator:login(?NAME2, ?ADDRESS2, undefined, ?LOGIN_TIME),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(10),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),

    ok = communicator:set_password(?NAME2, ?PASSWORD),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(10),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),

    ok = communicator:logout(?NAME2, ?LOGOUT_TIME),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    {ok, _} = communicator:login(?NAME2, ?ADDRESS2, ?PASSWORD, ?LOGIN_TIME),
    timer:sleep(10),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME),
    ok = communicator:logout(?NAME2, ?LOGOUT_TIME).

retry() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    {ok, _} = communicator:login(?NAME2, ?ADDRESS2, undefined, ?LOGIN_TIME),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(10),
    {communicator, get_node(?SERVER)} ! {msg_retry, ?MSGID},
    {communicator, get_node(?SERVER)} ! {msg_retry, {?MSGID, ?NAME2}},
    timer:sleep(10),
    ok = communicator:confirm({?MSGID, ?NAME2}),
    ok = communicator:confirm(?MSGID),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME),
    ok = communicator:logout(?NAME2, ?LOGOUT_TIME).
    
list_of_users() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    [{?NAME1, "undefined", ?LOGIN_TIME, "temp"}] = communicator:list_of_users(),
    ok = communicator:send_message(all, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    [{?NAME1, ?TIME, ?LOGIN_TIME, "temp"}] = communicator:list_of_users(),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    [{?NAME1, ?TIME, ?LOGIN_TIME, "active"}] = communicator:list_of_users(),
    {ok, _} = communicator:login(?NAME2, ?ADDRESS1, undefined, ?LOGIN_TIME),
    [{?NAME1, ?TIME, ?LOGIN_TIME, "active"}, {?NAME2, "undefined", ?LOGIN_TIME, "temp"}] = communicator:list_of_users(),
    ok = communicator:logout(?NAME1, ?LOGOUT_TIME),
    timer:sleep(10),
    [{?NAME1, ?TIME, ?LOGIN_TIME, ?LOGOUT_TIME}, {?NAME2, "undefined", ?LOGIN_TIME, "temp"}] = communicator:list_of_users().



find_password() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    undefined = communicator:find_password(?NAME1),
    ok = communicator:set_password(?NAME1, ?PASSWORD),
    defined = communicator:find_password(?NAME1).

find_user() ->
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    ok = communicator:find_user(?NAME1),
    does_not_exist = communicator:find_user(?NAME2).

confirm_mess_and_user_history() ->
    ok = communicator:clear_whole_table(messages, "messages"),
    {ok, _} = communicator:login(?NAME1, ?ADDRESS1, undefined, ?LOGIN_TIME),
    {ok, _} = communicator:login(?NAME2, ?ADDRESS2, undefined, ?LOGIN_TIME),
    ok = communicator:set_password(?NAME2, ?PASSWORD),
    [] = communicator:user_history(?NAME2),
    ok = communicator:send_message(?NAME2, ?TIME, ?NAME1, ?MESSAGE, ?MSGID),
    timer:sleep(10),
    communicator:confirm(?MSGID),
    [{?TIME, ?NAME1, ?MESSAGE}] = communicator:user_history(?NAME2).
default() ->
    ok = gen_server:call({communicator, get_node(?SERVER)}, costam),
    gen_server:cast({communicator, get_node(?SERVER)}, costam),
    {communicator, get_node(?SERVER)} ! "blabla".

get_node(Name) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

