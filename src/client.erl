-module(client).
-behaviour(gen_statem).

%% API
-export([start/0, start/1, receive_confirmation_from_server/1, receive_message/4, logout/1]).
%% CALLBACKS
-export([init/1, callback_mode/0, terminate/3, logged_out/3, logged_in/3]).

-define(DIVIDER, "~n;~n").
-define(MSG_DELIVERY_TIME, 5000).
-define(ACTIVE_TIME, 5000).

-record(data, {
	username = "" :: string(),
	outbox :: list(),
	active_timer_ref = undefined
}).
-record(msg_sent, {
	msg_ref, 
	timer_ref, 
	msg
}).


% ================================================================================
% API
% ================================================================================

start(Port) ->
	tcp_client:start_link({127,0,0,1}, Port, [list, {active, true}]),
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []),
	greet(),
	Username = login(),
	read_commands(Username). 

start() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []),
	greet(),
	Username = login(),
	read_commands(Username).

receive_message(Time, From, Message, MsgId) ->
	gen_statem:cast(?MODULE, {message, Time, From, Message, MsgId}).

receive_confirmation_from_server(MsgId) ->
	gen_statem:cast(?MODULE, {confirmation_from_server, MsgId}).


% ================================================================================
% CALLBACKS
% ================================================================================

init(_Args) ->
	{ok, logged_out, #data{outbox = []}}.

callback_mode() ->
	state_functions.

logged_out({call, From}, {login, Username, Password}, Data) ->
	Reply = tcp_client:login([Username, Password]),
	case Reply of
		ok ->
			{ok, TimerRef} = timer:send_after(?ACTIVE_TIME, i_am_active),
			io:format("Connected to server~nFor avaiable commands type ~chelp~c~n", [$",$"]),
			{next_state, logged_in, Data#data{username = Username, active_timer_ref = TimerRef}, {reply, From, Reply}};
		Reply ->
			{keep_state_and_data, {reply, From, Reply}}
	end;
logged_out({call, From}, {find_password, Username}, _Data) ->
	FindPass = tcp_client:find_password([Username]),
	{keep_state_and_data, {reply, From, FindPass}};
logged_out({call, From}, _, _Data) ->
	handle_unknown(From);
logged_out(info, {reply, Reply}, _Data) ->
	io:format("Received unknown request: ~p~n", [Reply]),
	keep_state_and_data.

logged_in({call, From}, {logout, Username}, Data) ->
	tcp_client:logout([Username]),
	timer:cancel(Data#data.active_timer_ref),
	{next_state, logged_out, Data#data{username = "", active_timer_ref = undefined}, {reply, From, ok}};
logged_in({call, From}, get_name, Data) ->
	{keep_state_and_data, {reply, From, Data#data.username}};
logged_in({call, From}, help, _Data) ->
	help(),
	{keep_state_and_data, {reply, From, ok}};
logged_in({call, From}, {send, To, Message}, Data) ->
	{{Y,M,D},{H,Min,S}} = calendar:local_time(),
	Year = integer_to_list(Y),
    TempTime = [ "00" ++ integer_to_list(X) || X <- [M, D, H, Min, S]],
    [Month,Day,Hour,Minute,Second] = [lists:sublist(X, lists:flatlength(X) - 1, 2) || X <- TempTime],
    Time =  Year ++ "/" ++ Month ++ "/" ++ Day ++ " " ++ Hour ++ ":" ++ Minute ++ ":" ++ Second,
	case To of 
		[] ->
			MsgId = make_ref(),
			{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_timeout, "0", MsgId}),
			MsgSent = #msg_sent{msg_ref = MsgId, timer_ref = TimerRef, msg = {Time, To, Message}},
			NewOutbox = Data#data.outbox ++ [MsgSent],
			NewData = Data#data{outbox = NewOutbox},
			tcp_client:send_message(["0", "all", Time, Data#data.username, Message, ref_to_list(MsgId)]),
			{keep_state, NewData, {reply, From, all}};
		_ ->
			Reply = tcp_client:find_user([To]),
			case Reply of
				does_not_exist ->
					{keep_state_and_data, {reply, From, does_not_exist}};
				ok ->
					MsgId = make_ref(),
					{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_timeout, "1", MsgId}),
					MsgSent = #msg_sent{msg_ref = MsgId, timer_ref = TimerRef, msg = {Time, To, Message}},
					NewOutbox = Data#data.outbox ++ [MsgSent],
					NewData = Data#data{outbox = NewOutbox},
					tcp_client:send_message(["1", To, Time, Data#data.username, Message, ref_to_list(MsgId)]),
					{keep_state, NewData, {reply, From, private}}
			end
	end;
logged_in({call, From}, list_of_users, _Data) ->
	ListOfUsers = tcp_client:list_of_users(),
	{keep_state_and_data, {reply, From, ListOfUsers}};
logged_in({call, From}, {set_pass, Password}, Data) ->
	tcp_client:set_password([Data#data.username, Password]),
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, history, Data) ->
	case tcp_client:find_password([Data#data.username]) of
		undefined ->
			{keep_state_and_data, {reply, From, not_registered}};
		defined ->
			History = tcp_client:user_history([Data#data.username]),
			{keep_state_and_data, {reply, From, History}}
	end;
logged_in({call, From}, _, _Data) ->
	handle_unknown(From);

%%confirmation from server that message was received 
logged_in(cast, {confirmation_from_server, MsgId}, Data) ->
	{MsgSent, NewOutBox} = take_msg_by_ref(MsgId, Data#data.outbox),
	timer:cancel(MsgSent#msg_sent.timer_ref),
	{keep_state, Data#data{outbox = NewOutBox}};

%%when server doesn't confirm in the time defined in the macro MSG_DELIVERY_TIMER
logged_in(info, {msg_timeout, IsPrivate, MsgId}, Data) ->
	{Message, Outbox1} = take_msg_by_ref(MsgId, Data#data.outbox),
	{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_timeout, IsPrivate, MsgId}),
	NewMsg = Message#msg_sent{timer_ref = TimerRef}, 
	{Time, To, Message_txt} = Message#msg_sent.msg,
	NewOutbox = Outbox1 ++ [NewMsg],
	NewData = Data#data{outbox = NewOutbox},
	case IsPrivate of
		"0" ->
			tcp_client:send_message(["0", "all", Time, Data#data.username, Message_txt, ref_to_list(MsgId)]);
		"1" ->
			tcp_client:send_message(["1", To, Time, Data#data.username, Message_txt, ref_to_list(MsgId)])
	end,
	{keep_state, NewData};
logged_in(cast, {message, Time, From, Message, {MsgId, To}}, _Data) ->
	io:format("~s - ~s: ~s~n", [Time, From, Message]),
	tcp_client:confirmation_from_client([ref_to_list(MsgId), To]),
	keep_state_and_data;
logged_in(info, i_am_active, Data) ->
	{ok, TimerRef} = timer:send_after(?ACTIVE_TIME, i_am_active),
	tcp_client:confirm_activity([Data#data.username]),
	{keep_state, Data#data{active_timer_ref = TimerRef}};
logged_in(EventType, EventContent, Data) ->
	io:format("Received unknown request: ~p, {~p}, ~p", [EventType, EventContent, Data]),
	keep_state_and_data.

handle_unknown(From) ->
	io:format("Not a viable command~n"),
	{keep_state_and_data, {reply, From, unknown}}.

terminate(_Reason, _State, Data) ->
	case Data#data.username of
		"" ->
			ok;
		Username ->
			tcp_client:logout([Username])
	end,
	% tcp_client:cast("close"),
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
				all->
						io:format("You sent a message to all users~n");
				does_not_exist ->
						io:format("There is no such user!~n");
				private ->
						io:format("You sent a message to ~p~n", [To])
			end,
			read_commands(Username);
		users ->
			ListOfUsers = gen_statem:call(?MODULE, list_of_users),
			[io:format("~s: last message sent at: ~s, last login time: ~s, last logout time: ~s~n", 
					[
						Name, 
						LastMsgTime, 
						LastLoginTime,
						LastLogoutTime])
					|| {Name, LastMsgTime, LastLoginTime, LastLogoutTime} <- ListOfUsers],
			read_commands(Username);
		set_pass ->
			PromptSetPass = "Please input desired password: ",
			Password = read(PromptSetPass),
			gen_statem:call(?MODULE, {set_pass, Password}),
			io:format("Password has been set ~n"),
			read_commands(Username);
		history ->
			History = gen_statem:call(?MODULE, history),
			case History of
				not_registered -> io:format("Only registered users have access to messagess history.~n");
				empty -> io:format("Your history is empty.~n");
				_ -> 
					[io:format("~s - ~s: ~s~n", [Time, 
						From, 
						Message])
					|| {Time, From, Message} <- History]
			end,
			read_commands(Username);
		logout ->
			logout(Username),
			greet(),
			NewName = login(),
			read_commands(NewName);
		help ->
			gen_statem:call(?MODULE, help),
			read_commands(Username);
		_ ->
			io:format("Not a viable command~n"),
			read_commands(Username)
	end.

login() ->
	Prompt = "Please input your username: ",
	Username = read(Prompt),
	Inputpass = get_pass(Username),
	Reply = gen_statem:call(?MODULE, {login, Username, Inputpass}),
	case Reply of
		max_reached ->
			io:format("Maximum number of logged in clients reached~n"),
			login();
		already_exists ->
			io:format("Username already logged on~n"),
			login();
		wrong_password ->
			io:format("Wrong password, try again~n"),
			login();
		ok ->
			Username
	end.
get_pass(Username) ->
	Findpass = gen_statem:call(?MODULE, {find_password, Username}),
	case Findpass of
		undefined ->
			"0" ++ ?DIVIDER ++ "undefined";
		defined ->
			io:format("This user is password protected~n"),
			PromptP = "Please input your password: ",
			"1" ++ ?DIVIDER ++ read(PromptP)
	end.
logout(Username) ->
	Reply = gen_statem:call(?MODULE, {logout, Username}),
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
send Username		to send a message to user called Username
users			to show the list of active users
set_pass		to set a new password
history			to see your message history (only for registered users)
help			to view this again
exit			to exit the app~n").

read(Prompt) ->
	% 32 to 126
	Input = string:trim(io:get_line(Prompt), trailing, [$\n]),
	Check = [32 || _<- Input],
	EmptyPrompt = [32 || _<- Prompt],
	Output = [check(Y) || Y <- Input],
	case Output of
		Check ->
			Input;
		_ ->
			io:format("~s~n", [EmptyPrompt ++ Output]),
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

%code_to_7_bits(Input) ->
%	Bit = <<  <<(A-32)>> || A <- Input>>,
%	<< <<Code>> || <<_A:1,Code:7>> <= Bit>>.

decode_from_7_bits(Input) ->
	Bit = << <<0:1,Code:7>> || <<Code>> <= Input>>,
	[(A+32) || <<A:8>> <= Bit].

take_msg_by_ref(MsgId, Outbox) ->
	take_msg_by_ref(MsgId, Outbox, []).
take_msg_by_ref(_MsgId, [], _Acc) ->
	not_found;
%%matched
take_msg_by_ref(MsgId, [SentMsg | Tl], Acc) when SentMsg#msg_sent.msg_ref == MsgId ->
	{SentMsg, Acc ++ Tl};
%% no match, test next item
take_msg_by_ref(MsgId, [H | Tl], Acc) ->
	take_msg_by_ref(MsgId, Tl, Acc ++ [H]).