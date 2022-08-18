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
	Username = login(),
	read_commands(Username).

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

logged_out({call, From}, {login, Username, Password}, Data) ->
	case communicator:login(Username, {?MODULE, Data#data.address}, Password) of
		already_exists ->
			{keep_state_and_data, {reply, From, already_exists}};
		wrong_password ->
			{keep_state_and_data, {reply, From, wrong_password}};
		ok ->
			{next_state, logged_in, Data#data{username = Username}, {reply, From, ok}}
	end;
logged_out({call, From}, _, _Data) ->
	handle_unknown(From).

logged_in({call, From}, logout, Data) ->
	case communicator:logout(Data#data.username) of
		ok ->
			{next_state, logged_out, Data#data{username = ""}, {reply, From, ok}};
		_ ->
			% that would make no sense but it can stay in for now
			{keep_state_and_data, {reply, From, does_not_exist}}
	end;
logged_in({call, From}, get_name, Data) ->
	{keep_state_and_data, {reply, From, Data#data.username}};
logged_in({call, From}, help, _Data) ->
	help(),
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

handle_unknown(From) ->
	io:format("Not a viable command~n"),
	{keep_state_and_data, {reply, From, unknown}}.

terminate(_Reason, _State, Data) ->
	case Data#data.username of
		"" ->
			ok;
		Username ->
			try communicator:logout(Username)
			catch
				exit:_ ->
					ok
			end
	end,
	net_kernel:stop().


% ================================================================================
% INTERNAL
% ================================================================================


read_commands(Username) ->
	Prompt = "@" ++ Username ++ "> ",
	Input = io:get_line(Prompt),
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
					end,
					read_commands(Username);
				_ ->
					ok
			end;
		users ->
			case gen_statem:call(?MODULE, active_users) of
				{ok, _Username} ->
					io:format("List of active users: ~p~n", [communicator:show_active_users()]);
				_ ->
					ok
			end,
			read_commands(Username);
		set_pass ->
			{ok, [Password]} = io:fread("Please input desired password: ", "~s"),
			gen_statem:call(?MODULE, {set_pass, Password}),
			io:format("Password has been set ~n"),
			read_commands(Username);
		logout ->
			logout(),
			greet(),
			NewName = login(),
			read_commands(NewName);
		_ ->
			gen_statem:call(?MODULE, Command),
			read_commands(Username)
	end.

login() ->
	{ok, [Username]} = io:fread("Please input your username: ", "~s"),
	Findpass = communicator:find_password(Username),
	case Findpass of
		undefined ->
			Reply = gen_statem:call(?MODULE, {login, Username, undefined}),
			case Reply of
				already_exists ->
					io:format("Username already logged on~n"),
					login();
				ok ->
					io:format("Connected to server~nFor avaiable commands type ~chelp~c~n", [$",$"]),
					Username
			end;
		_ ->
			io:format("This user is password protected~n"),
			{ok, [Inputpass]} = io:fread("Please input your password: ", "~s"),
			Reply = gen_statem:call(?MODULE, {login, Username, Inputpass}),
			case Reply of
				already_exists ->
					io:format("Username already logged on~n"),
					login();
				wrong_password ->
					io:format("Wrong password, try again~n"),
					login();
				ok ->
					io:format("Connected to server~nFor avaiable commands type ~chelp~c~n", [$",$"]),
					Username
			end

	end.
logout() ->
	Reply = gen_statem:call(?MODULE, logout),
	case Reply of
		ok ->
			io:format("You have been successfully logged out~n");
		_ ->
			io:format("Something went wrong~n")
	end.

greet() ->
	io:format("~nWelcome to communicator erlang~n").

help() ->
	io:format("You can use the following commands:
logout			to log out from the server
send			to send a message to all users
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