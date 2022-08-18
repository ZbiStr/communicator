-module(client).
-behaviour(gen_statem).

%% API
-export([start/0]).
%% CALLBACKS
-export([init/1, callback_mode/0, terminate/3, logged_out/3, logged_in/3]).

-define(COOKIE, ciasteczko).

-record(data, {
	username = "" :: string(),
	address  :: atom()
}).


% ================================================================================
% API
% ================================================================================


start() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []),
	greet(),
	help_logged_out(),
	read_commands().


% ================================================================================
% CALLBACKS
% ================================================================================


init([]) ->
	Address = case node() of
        'nonode@nohost' ->
            start_node();
        Node ->
            Node
    end,
	erlang:set_cookie(local, ?COOKIE),
	{ok, logged_out, #data{address = Address}}.

callback_mode() ->
	state_functions.

logged_out({call, From}, {login, Username}, Data) ->
	case login(Username, Data#data.address) of
		already_exists ->
			{keep_state_and_data, {reply, From, already_exists}};
		ok ->
			help_logged_in(),
			{next_state, logged_in, Data#data{username = Username}, {reply, From, ok}}
	end;
logged_out({call, From}, help, _Data) ->
	help_logged_out(),
	{keep_state_and_data, {reply, From, ok}};
logged_out({call, From}, _, _Data) ->
	handle_unknown(From).

logged_in({call, From}, logout, Data) ->
	case logout(Data#data.username) of
		does_not_exist ->
			% that would make no sense but it can stay in for now
			{keep_state_and_data, {reply, From, does_not_exist}};
		ok ->
			help_logged_out(),
			{next_state, logged_out, Data#data{username = ""}, {reply, From, ok}}
	end;
logged_in({call, From}, help, _Data) ->
	help_logged_in(),
	{keep_state_and_data, {reply, From, ok}};
logged_in({call, From}, send, Data) ->
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, active_users, Data) ->
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, {set_pass, Password}, Data) ->
	communicator:set_password(Data#data.username, Password),
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, _, _Data) ->
	handle_unknown(From);
logged_in(cast, {message, From, Message}, _Data) ->
	io:format("From ~s: ~s~n", [From, Message]),                                  
    keep_state_and_data.

terminate(_Reason, _State, Data) ->
	case Data#data.username of
		"" ->
			ok;
		Username ->
			try logout(Username)
			catch
				exit:_ ->
					ok
			end
	end,
	net_kernel:stop().


% ================================================================================
% INTERNAL
% ================================================================================


read_commands() ->
	Input = io:get_line(""),
	[Command, Opts] =
		[list_to_atom(string:trim(Token)) || Token <- string:split(Input ++ " ", " ")],
	case Command of
		exit ->
			gen_statem:stop(?MODULE),
			exit(normal);
		send ->
			case gen_statem:call(?MODULE, send) of
				{ok, Username} ->
					To = atom_to_list(Opts),
					case To of 
						[] ->
							Message = string:trim(io:get_line("Message > "), trailing, [$\n]),
							communicator:send_message(Username, all, Message),
							io:format("You sent a message to all users~n");
						_ ->
							case communicator:find_user(To) of
							  	does_not_exist ->
										io:format("There is no such user!~n");
								ok ->
										Message = string:trim(io:get_line("Message > "), trailing, [$\n]),
										communicator:send_message(Username, To, Message),
										io:format("You sent a message to ~p~n", [To])
							end
					end;
				_ ->
					ok
			end;
		users ->
			case gen_statem:call(?MODULE, active_users) of
				{ok, _Username} ->
					io:format("List of active users: ~p~n", [communicator:show_active_users()]);
				_ ->
					ok
			end;
		login ->
			{ok, [Username]} = io:fread("Please input your username: ", "~s"),
			gen_statem:call(?MODULE, {login, Username});
		set_pass ->
			{ok, [Password]} = io:fread("Please input desired password: ", "~s"),
			gen_statem:call(?MODULE, {set_pass, Password}),
			io:format("Password has been set ~n");
		_ ->
			gen_statem:call(?MODULE, Command)			
	end,
	read_commands().

login(Username, Address) -> 
	Login = communicator:login(Username, {?MODULE, Address}),
	case Login of
		already_exists ->
			% makes no sense in this implementation
			io:format("Username already logged on~n");
		ok ->
			io:format("Succesfully logged in as: ~s~n", [Username])
	end,
	Login.

logout(Username) ->
	Logout = communicator:logout(Username),
	case Logout of
		does_not_exist ->
			io:format("This username doesn't exist~n");
		ok ->
			io:format("You have been successfully logged out~n")
	end,
	Logout.

handle_unknown(From) ->
	io:format("Not a viable command~n"),
	{keep_state_and_data, {reply, From, unknown}}.

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
logout			to log out from the server
send		to send a message to all users
send Username	to send a message to user called Username
users			to show the list of active users
set_pass		to set a new password
help			to view this again
exit			to exit the app~n").

start_node() ->
	% random lowercase letters
	Name = [96 + rand:uniform(26) || _ <- lists:seq(1,9)],
	try net_kernel:start(list_to_atom(Name), #{name_domain => shortnames}) of
		{ok, _} ->
			{ok, Host} = inet:gethostname(),
			list_to_atom(Name ++ "@" ++ Host)
	catch
		error:{already_started, _Pid} ->
			start_node()
	end.