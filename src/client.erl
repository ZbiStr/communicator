-module(client).
-behaviour(gen_statem).

%% API
-export([start/0, start/1]).
%% CALLBACKS
-export([init/1, callback_mode/0, terminate/3, logged_out/3, logged_in/3]).

-define(COOKIE, ciasteczko).
-define(MSG_DELIVERY_TIME, 5000).
-define(ACTIVE_TIME, 1000000).

-record(data, {
	username = "" :: string(),
	address  :: atom(), 
	outbox :: list(),
	is_buffered = false,
	buffer :: list()
}).
-record(msg_sent, {
	msg_ref,
	timer_ref,
	msg
}).
-record(prompt, {
	en :: string(),
	pl :: string()
}).


% ================================================================================
% API
% ================================================================================
start() ->
	start(en).

start(Lang) ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []),
	greet(Lang),
	Username = login(Lang),
	read_commands(Lang, Username).

% ================================================================================
% CALLBACKS
% ================================================================================


init([]) ->
	Address = 
	case node() of
		'nonode@nohost' ->
			start_node();
		Node ->
			Node
	end,
	erlang:set_cookie(local, ?COOKIE),
	{ok, logged_out, #data{address = Address, outbox = [], buffer = []}}.

callback_mode() ->
	state_functions.

logged_out({call, From}, {login, Username, Password}, Data) ->
	case communicator:login(Username, {?MODULE, Data#data.address}, Password) of
		{ok, ServerName} ->
			{ok, _TimerRef} = timer:send_after(?ACTIVE_TIME, i_am_active),
			{next_state, logged_in, Data#data{username = Username}, {reply, From, {ok, ServerName}}};
		Reply ->
			{keep_state_and_data, {reply, From, Reply}}
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
logged_in({call, From}, {send, To, Message}, Data) ->
	Time = get_time(),
	case To of 
		[] ->
			MsgId = make_ref(),
			{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_retry, MsgId}),
			MsgSent = #msg_sent{msg_ref = MsgId, timer_ref = TimerRef, msg = {Time, To, Message}},
			NewOutbox = Data#data.outbox ++ [MsgSent],
			NewData = Data#data{outbox = NewOutbox},
			communicator:send_message(all, Time, Data#data.username, Message, MsgId),
			{keep_state, NewData, {reply, From, all}};
		_ ->
			case communicator:find_user(To) of
				does_not_exist ->
					{keep_state_and_data, {reply, From, does_not_exist}};
				ok ->
					MsgId = make_ref(),
					{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_retry, MsgId}),
					MsgSent = #msg_sent{msg_ref = MsgId, timer_ref = TimerRef, msg = {Time, To, Message}},
					NewOutbox = Data#data.outbox ++ [MsgSent],
					NewData = Data#data{outbox = NewOutbox},
					communicator:send_message(To, Time, Data#data.username, Message, MsgId),
					{keep_state, NewData, {reply, From, private}}
			end
	end;
logged_in({call, From}, list_of_users, _Data) ->
	ListOfUsers = communicator:list_of_users(),
	{keep_state_and_data, {reply, From, ListOfUsers}};
logged_in({call, From}, {set_pass, Password}, Data) ->
	communicator:set_password(Data#data.username, Password),
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, history, Data) ->
	case communicator:find_password(Data#data.username) of
		undefined ->
			{keep_state_and_data, {reply, From, not_registered}};
		_ ->
			{keep_state_and_data, {reply, From, communicator:user_history(Data#data.username)}}
	end;
logged_in({call, From}, _, _Data) ->
	handle_unknown(From);

%%confirmation from server that message was received 
logged_in(cast, {msg_confirm_from_server, MsgId}, Data) ->
	{MsgSent, NewOutBox} = take_msg_by_ref(MsgId, Data#data.outbox),
	timer:cancel(MsgSent#msg_sent.timer_ref),
	{keep_state, Data#data{outbox = NewOutBox}};

%%when server doesn't confirm in the time defined in the macro MSG_DELIVERY_TIMER
logged_in(timeout, {msg_retry, MsgRef}, Data) ->
	{Message, Outbox1} = take_msg_by_ref(MsgRef, Data#data.outbox),
	{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_retry, MsgRef}),
	NewMsg = Message#msg_sent{timer_ref = TimerRef}, 
	{Time, To, Message_txt} = Message#msg_sent.msg,
	NewOutbox = Outbox1 ++ [NewMsg],
	NewData = Data#data{outbox = NewOutbox},
	communicator:send_message(To, Time, Data#data.username, Message_txt, MsgRef),
	{keep_state, NewData};
logged_in(timeout, i_am_active, Data) ->
	{ok, _TimerRef} = timer:send_after(?ACTIVE_TIME, i_am_active),
	communicator:confirm_activity(Data#data.username),
	keep_state_and_data;
logged_in(cast, {message, CodedTime, CodedFrom, CodedMessage, MsgId}, Data) ->
	From = decode_from_7_bits(CodedFrom),
	Message = decode_from_7_bits(CodedMessage),
	Time = decode_from_7_bits(CodedTime),
	communicator:confirm(MsgId),
	case Data#data.is_buffered of
		false ->
			io:format("~s - ~s: ~s~n", [Time, From, Message]),
			keep_state_and_data;
		true ->
			NewBuffer = Data#data.buffer ++ [[Time, From, Message]],
			{keep_state, Data#data{buffer = NewBuffer}}
		end;
logged_in(cast, {custom_server_message, CodedFrom, CodedMessage}, _Data) ->
	From = decode_from_7_bits(CodedFrom),
	Message = decode_from_7_bits(CodedMessage),
	io:format("!!!~s: ~s!!!~n", [From, Message]),
	keep_state_and_data;
logged_in(cast, start_buffer, Data) ->
	{keep_state, Data#data{is_buffered = true}};
logged_in(cast, stop_buffer, Data) ->
	[io:format("~s - ~s: ~s~n", X) || X <- Data#data.buffer],
	{keep_state, Data#data{is_buffered = false, buffer = []}};
logged_in(EventType, EventContent, Data) ->
	io:format("Received unknown request: ~p, ~p, ~p", [EventType, EventContent, Data]),
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


read_commands(Lang, Username) ->
	PromptReadCommands = "@" ++ Username ++ "> ",
	PromptMessage = read_prompt(Lang, message),
	Input = read(Lang, PromptReadCommands),
	[Command, Opts] =
		[list_to_atom(string:trim(Token)) || Token <- string:split(Input ++ " ", " ")],
	if
		Command == exit ->
			gen_statem:stop(?MODULE),
			exit(normal);
		Command == send orelse Command == s ->
			start_buffer(),
			To = atom_to_list(Opts),
			Message = read(Lang, PromptMessage),
			Status = gen_statem:call(?MODULE, {send, To, Message}),
			case Status of 
				all->
					PromptAll = read_prompt(Lang, all),
					io:format(PromptAll);
				does_not_exist ->
					PromptNotExist = read_prompt(Lang, does_not_exist),
					io:format(PromptNotExist);
				private ->
					PromptPrivate = read_prompt(Lang, private),
					io:format(PromptPrivate, [To])
			end,
			stop_buffer(),
			read_commands(Lang, Username);
		Command == users orelse Command == us ->
			ListOfUsers = gen_statem:call(?MODULE, list_of_users),
			PromptUsers = read_prompt(Lang, users),
			[io:format(PromptUsers, [Name, LastMessage, LastLogin, LastLogout])
			|| {Name, LastMessage, LastLogin, LastLogout} <- ListOfUsers],
			read_commands(Lang, Username);
		Command == set_pass orelse Command == sp ->
			PromptSetPass = read_prompt(Lang, set_pass),
			Password = read(Lang, PromptSetPass),
			gen_statem:call(?MODULE, {set_pass, Password}),
			PromptPassSet = read_prompt(Lang, pass_set),
			io:format(PromptPassSet),
			read_commands(Lang, Username);
		Command == history orelse Command == his ->
			History = gen_statem:call(?MODULE, history),
			case History of
				not_registered ->
					PromptHistoryNR = read_prompt(Lang, history_not_registered),
					io:format(PromptHistoryNR);
				[] -> 
					PromptHistoryEmpty = read_prompt(Lang, empty_history),
					io:format(PromptHistoryEmpty);
				_ -> 
					[io:format("~s - ~s: ~s~n", [Time, 
						From, 
						Message])
					|| {Time, From, Message} <- History]
			end,
			read_commands(Lang, Username);
		Command == logout orelse Command == lg ->
			logout(Lang),
			greet(Lang),
			NewName = login(Lang),
			read_commands(Lang, NewName);
		Command == help ->
			help(Lang),
			read_commands(Lang, Username);
		true ->
			gen_statem:call(?MODULE, Command),
			read_commands(Lang, Username)
	end.

login(Lang) ->
	Prompt = read_prompt(Lang, login),
	Username = read(Lang, Prompt),
	InputPass = get_pass(Lang, Username),
	Reply = gen_statem:call(?MODULE, {login, Username, InputPass}),
	case Reply of
		max_reached ->
			PromptMax = read_prompt(Lang, max_reached),
			io:format(PromptMax),
			login(Lang);
		already_exists ->
			PromptAlredy = read_prompt(Lang, already_exists),
			io:format(PromptAlredy),
			login(Lang);
		wrong_password ->
			PromptWrong = read_prompt(Lang, wrong_password),
			io:format(PromptWrong),
			login(Lang);
		{ok, ServerName} ->
			PromptServer = read_prompt(Lang, server_name),
			io:format(PromptServer, [ServerName, $",$"]),
			Username
	end.
get_pass(Lang, Username) ->
	IsPassword = communicator:find_password(Username),
	case IsPassword of
		undefined ->
			undefined;
		_ ->
			PromptIsPass = read_prompt(Lang, is_password),
			io:format(PromptIsPass),
			PromptPass = read_prompt(Lang, password_prompt),
			read(Lang, PromptPass)

	end.
logout(Lang) ->
	Reply = gen_statem:call(?MODULE, logout),
	case Reply of
		ok ->
			PromptLogout = read_prompt(Lang, ok_logout),
			io:format(PromptLogout);
		_ ->
			PromptNotOkLogout = read_prompt(Lang, not_ok_logout),
			io:format(PromptNotOkLogout)
	end.

greet(Lang) ->
	Prompt = read_prompt(Lang, greet),
	io:format(Prompt).

help(Lang) ->
	Start = read_prompt(Lang, help),
	Logout = read_prompt(Lang, help_logout),
	SendAll = read_prompt(Lang, help_send),
	SendUser = read_prompt(Lang, help_send_username),
	Users = read_prompt(Lang, help_users),
	SetPass = read_prompt(Lang, help_set_pass),
	History = read_prompt(Lang, help_history),
	Help = read_prompt(Lang, help_help),
	Exit = read_prompt(Lang, help_exit),
	io:format(Start ++
	"logout (lg)		" ++ Logout ++
	"send (s)			" ++ SendAll ++
	"send (s) Username	" ++ SendUser ++
	"users (us)			" ++ Users ++
	"set_pass (sp)		" ++ SetPass ++
	"history (his)		" ++ History ++
	"help				" ++ Help ++
	"exit				" ++ Exit).

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

read(Lang, Prompt) ->
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
			PromptReadWrong = read_prompt(Lang, read_wrong),
			io:format(PromptReadWrong),
			read(Lang, Prompt)
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

get_time() ->
	{{Y,M,D},{H,Min,S}} = calendar:local_time(),
	Year = integer_to_list(Y),
	TempTime = [ "00" ++ integer_to_list(X) || X <- [M, D, H, Min, S]],
	[Month,Day,Hour,Minute,Second] = [lists:sublist(X, lists:flatlength(X) - 1, 2) || X <- TempTime],
	Year ++ "/" ++ Month ++ "/" ++ Day ++ " " ++ Hour ++ ":" ++ Minute ++ ":" ++ Second.

start_buffer() ->
	gen_statem:cast(?MODULE, start_buffer).
stop_buffer() ->
	gen_statem:cast(?MODULE, stop_buffer).

read_prompt(Lang, Id) ->
	{ok, Table} = dets:open_file(prompts, [{file, "prompts"}, {type, set}]),
	[{Id, Prompt}] = dets:lookup(Table, Id),
	dets:close(Table),
	case Lang of
		en ->
			Prompt#prompt.en;
		pl ->
			Prompt#prompt.pl
	end.