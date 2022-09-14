-module(tcp_server).
-behaviour(gen_server).

%% API
-export([start/1, stop/0]).
-export([
	connect/3, 
	decode_message/1, 
	call/2, 
	cast/2, 
	login/2, 
	find_password/2, 
	find_user/2, 
	active_users/1, 
	set_password/2, 
	logout/2, 
	send_message/2, 
	send_to_client/2,
	confirmation_from_server/2,
	confirmation_from_client/2,
	user_history/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-record(client, {pid, socket}).
-record(state, {port, listen_socket, client_list}).

-define(DIVIDER, "~n;~n").

% ================================================================================
% API CLIENT
% ================================================================================

connect(IP, Port, Opts) ->
	{ok, Socket} = gen_tcp:connect(IP, Port, Opts),
	Socket.

cast(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

call(Socket, Packet) ->
	cast(Socket, Packet),
	{ok, Reply} = gen_tcp:recv(Socket, 0),
	Reply.

login(Socket, Args) ->
	Packet = string:join(["login"] ++ Args, ?DIVIDER),
	RawReply = call(Socket, Packet),
	{Reply, _}= tcp_server:decode_message(RawReply),
	Reply.

find_password(Socket, Args) ->
	Packet = string:join(["find_password"] ++ Args, ?DIVIDER),
	RawReply = call(Socket, Packet),
	{FindPass, _}= tcp_server:decode_message(RawReply),
	FindPass.

find_user(Socket, Args) ->
	Packet = string:join(["find_user"] ++ Args, ?DIVIDER),
	RawReply = call(Socket, Packet),
	{Reply, _}= tcp_server:decode_message(RawReply),
	Reply.

active_users(Socket) ->
	RawReply = call(Socket, "active_users"),
	{ok, Reply} = decode_message(RawReply),
	Reply.

user_history(Socket, Args) ->
	Packet = string:join(["user_history"] ++ Args, ?DIVIDER),
	RawReply = call(Socket, Packet),
	{ok, Reply} = decode_message(RawReply),
	Reply.

set_password(Socket, Args) ->
	Packet = string:join(["set_password"] ++ Args, ?DIVIDER),
	cast(Socket, Packet).

logout(Socket, Args) ->
	Packet = string:join(["logout"] ++ Args, ?DIVIDER),
	cast(Socket, Packet).

send_message(Socket, Args) ->
	Packet = string:join(["send_message"] ++ Args, ?DIVIDER),
	cast(Socket, Packet).

confirmation_from_client(Socket, Args) ->
	Packet = string:join(["confirmation_from_client"] ++ Args, ?DIVIDER),
	cast(Socket, Packet).

send_to_client(ClientSocket, Args) ->
	Packet = string:join(["message"] ++ Args, ?DIVIDER),
	cast(ClientSocket, Packet).

confirmation_from_server(ClientSocket, Args) ->
	Packet = string:join(["confirmation_from_server"] ++ Args, ?DIVIDER),
	cast(ClientSocket, Packet).

% ================================================================================
% API SERVER
% ================================================================================

start(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []),
	communicator:start_link().
	
stop() ->
	communicator:stop(),
	gen_server:stop(?MODULE, shutdown, infinity).

% ================================================================================
% CALLBACK
% ================================================================================

init(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}, list]),
	spawn_link(fun() -> acceptConnections(ListenSocket) end),
	{ok, #state{port=Port, listen_socket=ListenSocket, client_list=[]}}.

handle_cast({connected, Client}, #state{client_list=ClientList}=State) ->
	NewState = State#state{client_list = lists:append(ClientList, [Client])},
	{noreply, NewState};
	
handle_cast({disconnected, ClientSocket}, State) ->
	Clients = lists:keydelete(ClientSocket, #client.socket, State#state.client_list),
	NewState = State#state{client_list=Clients},
	{noreply, NewState};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(shutdown, State) ->
	Fun = fun(#client{socket=ClientSocket}) -> disconnect(ClientSocket, "Server is shutting down!") end,
	lists:foreach(Fun, State#state.client_list);

terminate(Reason, State) ->
	io:format("terminate! Reason: ~p~nState: ~p~n", [Reason, State]).

% ================================================================================
% INTERNAL
% ================================================================================

acceptConnections(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, AcceptedSocket} ->
			Pid = spawn_link(fun() -> handleConnection(AcceptedSocket) end),
			gen_tcp:controlling_process(AcceptedSocket, Pid),
			Pid2 = spawn_link(fun() -> client_loop(ListenSocket) end), % w connect nie dziala, gdzie to dac?
			gen_server:cast(?MODULE, {connected, #client{pid=Pid, socket=AcceptedSocket}}),
			acceptConnections(ListenSocket);
		{error, closed} -> 
			gen_tcp:close(ListenSocket)
	end.


	
% handleHandshake(AcceptedSocket) ->
% 	inet:setopts(AcceptedSocket, [{active, once}]),
% 	receive
% 		{tcp, AcceptedSocket, <<"quit", _/binary>>} ->
% 			gen_server:cast(?MODULE, {disconnected, AcceptedSocket}),
% 			gen_tcp:close(AcceptedSocket);
			
% 		{tcp, AcceptedSocket, Msg} ->
% 			case ws_lib:handle_handshake_request(Msg) of
% 				{ok, Response} ->
% 					gen_tcp:send(AcceptedSocket, Response),
% 					handleConnection(AcceptedSocket);
% 				{error, Response} ->
% 					gen_tcp:send(AcceptedSocket, Response),
% 					gen_server:cast(?MODULE, {disconnected, AcceptedSocket}),
% 					gen_tcp:close(AcceptedSocket)
% 			end;			
% 		{error, closed} -> 
% 			gen_server:cast(?MODULE, {disconnected, AcceptedSocket}),
% 			gen_tcp:close(AcceptedSocket)
% 	end.
	
handleConnection(ClientSocket) ->
	inet:setopts(ClientSocket, [{active, once}]),
	receive
		{tcp, ClientSocket, <<"quit", _/binary>>} ->
			gen_server:cast(?MODULE, {disconnected, ClientSocket}),
			gen_server:cast(?MODULE, {message, "Server: Client dropped!"}),
			gen_tcp:close(ClientSocket);
			
		{tcp, ClientSocket, Message} ->
			case decode_message(Message) of
				% CALLS
				{login, Frame} ->
					[Username, IsPassword, Password] = Frame,
					handle_login(ClientSocket, Username, IsPassword, Password),
					handleConnection(ClientSocket);
				{find_password, Frame} ->
					[Username] = Frame,
					handle_find_password(ClientSocket, Username),
					handleConnection(ClientSocket);
				{find_user, Frame} ->
					[Username] = Frame,
					handle_find_user(ClientSocket, Username),
					handleConnection(ClientSocket);
				{active_users, _Frame} ->
					handle_active_users(ClientSocket),
					handleConnection(ClientSocket);
				{user_history, Frame} ->
					[Username] = Frame,
					handle_user_history(ClientSocket, Username),
					handleConnection(ClientSocket);
				% CASTS
				{logout, Frame} ->
					[Username] = Frame,
					handle_logout(Username),
					handleConnection(ClientSocket);
				{set_password, Frame} ->
					[Username, Password] = Frame,
					handle_set_password(Username, Password),
					handleConnection(ClientSocket);
				{send_message, Frame} ->
					[IsPrivate, To, Time, From, Message_txt, StringMsgId] = Frame,
					handle_send_message(IsPrivate, To, Time, From, Message_txt, list_to_ref(StringMsgId)),
					handleConnection(ClientSocket);
				{confirmation_from_client, Frame} ->
					[StringMsgId, Username] = Frame,
					handle_confirmation_from_client({list_to_ref(StringMsgId), Username}),
					handleConnection(ClientSocket);
				{close, _Frame} ->
					gen_server:cast(?MODULE, {disconnected, ClientSocket}),
					gen_server:cast(?MODULE, {message, "Server: Client disconnected!"}),
					gen_tcp:close(ClientSocket);
				{_, Frame} ->
					io:format("Unsupported frame:~n~p~n", [Frame]);
				{error, Reason, Frame} ->
					io:format("Error! Reason: ~p~n~p~n", [Reason, Frame])
			end
	end.

% SERVER TO CLIENT
client_loop(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive	
		{tcp, ClientSocket, Message} ->
			case decode_message(Message) of
				% CALLS
				{confirmation_from_server, Frame} ->
					[MsgId] = Frame,
					handle_confirmation_from_server(ref_to_list(MsgId)),
					client_loop(Socket);
				{message, Frame} ->
					[Time, From, Message, StringMsgId] = Frame,
					handle_send_to_client(Time, From, Message, list_to_ref(StringMsgId)),
					handleConnection(ClientSocket);
				{close, _Frame} ->
					gen_server:cast(?MODULE, {disconnected, ClientSocket}),
					gen_server:cast(?MODULE, {message, "Server: Client disconnected!"}),
					gen_tcp:close(ClientSocket);
				{_, Frame} ->
					io:format("Unsupported frame:~n~p~n", [Frame]);
				{error, Reason, Frame} ->
					io:format("Error! Reason: ~p~n~p~n", [Reason, Frame])
			end
	end.


% ================================================================================
decode_message(Message) ->
	[H|Frame] = string:split(Message,?DIVIDER, all),
	Atom = list_to_atom(H),
	{Atom, Frame}.

disconnect(ClientSocket, _Reason) ->
	%sendMessage(ClientSocket, Reason),
	gen_tcp:close(ClientSocket).

% CALLS
handle_login(ClientSocket, Username, IsPassword, Password) ->
	case IsPassword of 
		"0" ->
			Atom = communicator:login(Username, ClientSocket, undefined),
			gen_tcp:send(ClientSocket, atom_to_list(Atom));
		"1" ->
			Atom = communicator:login(Username, ClientSocket, Password),
			gen_tcp:send(ClientSocket, atom_to_list(Atom))
	end.

handle_find_password(ClientSocket, Username) ->
	Atom = communicator:find_password(Username),
	gen_tcp:send(ClientSocket, atom_to_list(Atom)).

handle_find_user(ClientSocket, Username) ->
	Atom = communicator:find_user(Username),
	gen_tcp:send(ClientSocket, atom_to_list(Atom)).

handle_active_users(ClientSocket) ->
	ActiveUsers = ["ok"] ++ communicator:show_active_users(),
	Reply = string:join(ActiveUsers, ?DIVIDER),
	gen_tcp:send(ClientSocket, Reply).

handle_user_history(ClientSocket, Username) ->
	_History = communicator:user_history(Username),
	gen_tcp:send(ClientSocket, atom_to_list(ok)).


% CASTS
handle_logout(Username) ->
	communicator:logout(Username).

handle_set_password(Username, Password) ->
	communicator:set_password(Username, Password).

handle_send_message(IsPrivate, To, Time, From, Message, MsgId) ->
	case IsPrivate of 
		"0" ->
			communicator:send_message(all, Time, From, Message, MsgId);
		"1" ->
			communicator:send_message(To, Time, From, Message, MsgId)
	end.

handle_confirmation_from_client(MsgId) ->
	communicator:confirm(MsgId).

% SERVER TO CLIENT

handle_confirmation_from_server(MsgId) ->
	client:receive_confirmation_from_server(MsgId).

handle_send_to_client(Time, From, Message, MsgId) ->
	client:receive_message(Time, From, Message, MsgId).






