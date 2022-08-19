-module(client_tests).
-include_lib("eunit/include/eunit.hrl").

% probably put this in some .hrl
-define(SERVER, erlangpol).
-define(CLIENT1, client1).
-define(CLIENT2, client2).

-define(COOKIE, ciasteczko).

-define(NAME1, name1).


all_test_() ->
	{
		foreach,
		fun start_system/0,
		fun stop_system/1, 
		[
			fun login_without_pass/0,
			fun set_password/0,
			fun login_with_wrongpass/0,
			fun login_with_correctpass/0,
			fun login_already_exists/0,
			fun find_user/0,
			fun show_active_users/0,
			fun send_message/0,
			fun logout/0,
			fun help/0,
			fun unknown_command/0
		]
	}.
  
% SETUP

start_system() ->
	% client1's node = master node
	{ok, _} = net_kernel:start(?CLIENT1, #{name_domain => shortnames}),
	erlang:set_cookie(?COOKIE),

	{ok, Host} = inet:gethostname(),
	% this is pretty bad
	SrcPath = "./_build/default/lib/communicator/ebin",
	Args = io_lib:format("-setcookie ~s -pa \"~s\"", [?COOKIE, SrcPath]),
	% separate nodes for client2 and server
	{ok, NodeClient2} = slave:start(Host, ?CLIENT2, Args),
	{ok, NodeServer} = slave:start(Host, ?SERVER, Args),

	{ok, _} = gen_statem:start({local, client}, client, [], []),
	{ok, _} = rpc:call(NodeClient2, gen_statem, start, [{local, client}, client, [], []]),
	{ok, _} = rpc:call(NodeServer, gen_server, start, [{local, communicator}, communicator, [], []]).

stop_system(_) ->
	ok = gen_server:stop({communicator, get_node(?SERVER)}),
	ok = gen_statem:stop({client, get_node(?CLIENT2)}),
	% must be stopped last cause currently it also stops its node (the master node of these tests)
	ok = gen_statem:stop(client).

% TESTCASES

% testing should be done on client1's node (this node)
login_without_pass() ->
	ok = gen_statem:call(client, {login, ?NAME1, undefined}).

set_password() ->
	ok = gen_statem:call(client, {login, ?NAME1, undefined}),
	{ok, _} = gen_statem:call(client, {set_pass, pass}).
	
login_with_wrongpass() ->
	ok = gen_statem:call(client, {login, ?NAME1, undefined}),
	{ok, _} = gen_statem:call(client, {set_pass, pass}),
	ok = gen_statem:call(client, logout),
	wrong_password = gen_statem:call(client, {login, ?NAME1, badpass}).

login_with_correctpass() ->
	ok = gen_statem:call(client, {login, ?NAME1, undefined}),
	{ok, _} = gen_statem:call(client, {set_pass, pass}),
	ok = gen_statem:call(client, logout),
	ok = gen_statem:call(client, {login, ?NAME1, pass}).

login_already_exists() ->
	gen_statem:call({client, get_node(?CLIENT2)}, {login, ?NAME1, undefined}),
	already_exists = gen_statem:call(client, {login, ?NAME1, undefined}).

find_user() ->
	gen_statem:call(client, {login, ?NAME1, undefined}),
	_Data = gen_statem:call(client, get_name).

show_active_users() ->
	gen_statem:call(client, {login, ?NAME1, undefined}),
	_Data = gen_statem:call(client, active_users).

send_message() ->
	gen_statem:call(client, {login, ?NAME1, undefined}),
	{ok, ?NAME1} = gen_statem:call(client, send).

logout() ->
	gen_statem:call(client, {login, ?NAME1, undefined}),
	ok = gen_statem:call(client, logout).

help() ->
	gen_statem:call(client, {login, ?NAME1, undefined}),
	ok = gen_statem:call(client, help).

unknown_command() ->
	unknown = gen_statem:call(client, not_a_command),
	gen_statem:call(client, {login, ?NAME1, undefined}),
	unknown = gen_statem:call(client, not_a_command).

get_node(Name) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(atom_to_list(Name) ++ "@" ++ Host).
