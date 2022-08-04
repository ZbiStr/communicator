-module(client).
-behaviour(gen_statem).

%% API
-export([start/0]).
%% CALLBACKS
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

-define(COOKIE, ciasteczko).


% ================================================================================
% API
% ================================================================================


start() ->
	greet(),
	help_logged_out(),
	parse_logged_out().


% ================================================================================
% CALLBACKS
% ================================================================================


init([Username]) ->
	net_kernel:start(list_to_atom(Username), #{name_domain => shortnames}),
	erlang:set_cookie(local, ?COOKIE),
	{ok, state, [Username]}.

%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
	handle_event_function.

handle_event(enter, _OldState, _State, _Data) ->
	keep_state_and_data;
handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

terminate(_Reason, _State, [Username] = _Data) ->
	communicator:logout(Username),
	net_kernel:stop().


% ================================================================================
% INTERNAL
% ================================================================================


parse_logged_out() ->
	{ok, [Command]} = io:fread("", "~a"),
	case Command of
		help ->
			help_logged_out(),
			parse_logged_out();
		login ->
			Username = login(),
			io:format("You have been successfully logged in~n"),
			help_logged_in(),
			parse_logged_in(Username);
		exit ->
			ok;
		_ ->
			io:format("Not a viable command.~n"),
			parse_logged_out()
	end.

parse_logged_in(Username) ->
	{ok, [Command]} = io:fread("", "~a"),
	case Command of
		help ->
			help_logged_in(),
			parse_logged_in(Username);
		logout ->
			case logout(Username) of
				does_not_exist ->
					parse_logged_in(Username);
				ok ->
					parse_logged_out()
			end;
		exit ->
			logout(Username);
		_ ->
			io:format("Not a viable command.~n"),
			parse_logged_in(Username)
	end.

login() -> 
	{ok, [Username]} = io:fread("Please input your username: ", "~s"),
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [Username], []),
	case communicator:login(Username, {?MODULE, client_node(Username)}) of
		already_exists ->
			stop(Username),
			io:fwrite("Username already logged on~n"),
			login();
		ok ->
			io:format("Username: ~s~n", [Username]),
			Username
	end.

logout(Username) ->
	Logout = communicator:logout(Username),
	case Logout of
		does_not_exist ->
			io:format("This username doesn't exist~n");
		ok ->
			stop(Username),
			io:format("You have been successfully logged out~n")
	end,
	Logout.

greet() ->
	io:format("
////////////////////////////////////////////
/////    Glad to see you in our app!   /////
////////////////////////////////////////////~n").

help_logged_out() ->
	io:format("You can use the following commands:
login     to log in to the server
help      to view this again
exit      to exit the app~n").

help_logged_in() ->
	io:format("You can use the following commands:
logout    to log out from the server
help      to view this again
exit      to exit the app~n").

client_node(Username) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(Username ++ "@" ++ Host).

stop(Username) ->
	gen_statem:stop({?MODULE, client_node(Username)}).