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
logged_in({call, From}, {send, To, Message}, Data) ->
	{{Y,M,D},{H,S,_MS}} = calendar:local_time(),
	Time =  integer_to_list(Y) ++ "/" ++ "0" ++ integer_to_list(M) ++ "/" ++ 
			integer_to_list(D) ++ " " ++ integer_to_list(H) ++ ":" ++ 
			integer_to_list(S),
	case To of 
		[] ->
			communicator:send_message(all, Time, Data#data.username, Message),
			{keep_state_and_data, {reply, From, all}};
		_ ->
			case communicator:find_user(To) of
				does_not_exist ->
					{keep_state_and_data, {reply, From, does_not_exist}};
				ok ->
					communicator:send_message(To, Time, Data#data.username, Message),
					{keep_state_and_data, {reply, From, private}}
			end
	end;
logged_in({call, From}, active_users, Data) ->
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, {set_pass, Password}, Data) ->
	communicator:set_password(Data#data.username, Password),
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, history, Data) ->
	{keep_state_and_data, {reply, From, communicator:user_history(Data#data.username)}};
logged_in({call, From}, _, _Data) ->
	handle_unknown(From);
logged_in(cast, {message, CodedTime, CodedFrom, CodedMessage}, _Data) ->
	From = decode_from_7_bits(CodedFrom),
	Message = decode_from_7_bits(CodedMessage),
	Time = decode_from_7_bits(CodedTime),
	io:format("~s - ~s: ~s~n", [Time, From, Message]),
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
	PromptReadCommands = "@" ++ Username ++ "> ",
	PromptMessage = "Message> ",
	Input = read(PromptReadCommands),
	[Command, Opts] =
		[list_to_atom(string:trim(Token)) || Token <- string:split(Input ++ " ", " ")],
	case Command of
		exit ->
			gen_statem:stop(?MODULE),
			exit(normal);
		send ->
			To = atom_to_list(Opts),
			Message = read(PromptMessage),
			Status = gen_statem:call(?MODULE, {send, To, Message}),
			case Status of 
				all ->
						io:format("You sent a message to all users~n");
				does_not_exist ->
						io:format("There is no such user!~n");
				private ->
						io:format("You sent a message to ~p~n", [To])
			end,
			read_commands(Username);
		users ->
			case gen_statem:call(?MODULE, active_users) of
				{ok, _Username} ->
					io:format("List of active users: ~p~n", [communicator:show_active_users()]);
				_ ->
					ok
			end,
			read_commands(Username);
		set_pass ->
			PromptSetPass = "Please input desired password: ",
			Password = read(PromptSetPass),
			gen_statem:call(?MODULE, {set_pass, Password}),
			io:format("Password has been set ~n"),
			read_commands(Username);
		history ->
			case communicator:find_password(Username) of
				undefined -> io:format("You have access to messagess history only from registered account.~n");
				_ -> 
					History = gen_statem:call(?MODULE, history),
					case History of
						[] -> io:format("Your history is empty.~n");
						_ -> 
							[io:format("~s - ~s: ~s~n", [Time, From, Message])
							|| {Time, From, Message} <- History]
					end
			end,
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
	Prompt = "Please input your username: ",
	Username = read(Prompt),
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
			PromptP = "Please input your password: ",
			Inputpass = read(PromptP),
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
history			to see your message history (only for registered users)
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

read(Prompt) ->
	% 32 to 126
	Input = string:trim(io:get_line(Prompt), trailing, [$\n]),
	Check = [32 || _<- Input],
	Output = [check(Y) || Y <- Input],
	case Output of
		Check ->
			Input;
		_ ->
			io:format("~s~n", [Output]),
			io:format("Wrong character at indicated position~n"),
			io:format("Try again~n"),
			read(Prompt)
		end.

check(Y) ->
	if
		Y >= 32 andalso Y =< 126 ->
			32;
		true ->
			94
	end .

code_to_7_bits(Input) ->
	Bit = <<  <<(A-32)>> || A <- Input>>,
	<< <<Code>> || <<_A:1,Code:7>> <= Bit>>.

decode_from_7_bits(Input) ->
	Bit = << <<0:1,Code:7>> || <<Code>> <= Input>>,
	[(A+32) || <<A:8>> <= Bit].