-module(tcp_server).
-behaviour(gen_server).

%% API
-export([start/1, stop/0, send_to_client/2, confirmation_from_server/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-record(client, {pid, socket}).
-record(state, {port, listen_socket, client_list}).

-define(DIVIDER, "~n;~n").
% ================================================================================
% API
% ================================================================================

start(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []),
	communicator:start_link().
	
stop() ->
	communicator:stop(),
	gen_server:stop(?MODULE, shutdown, infinity).

send_to_client(ClientSocket, Args) ->
	Packet = string:join(["message"] ++ Args, ?DIVIDER),
	gen_tcp:send(ClientSocket, Packet).

confirmation_from_server(ClientSocket, Args) ->
	Packet = string:join(["confirmation_from_server"] ++ Args, ?DIVIDER),
	gen_tcp:send(ClientSocket, Packet).

% ================================================================================
% CALLBACK
% ================================================================================

init(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}, list]),
	spawn_link(fun() -> acceptConnections(ListenSocket) end),
	{ok, #state{port=Port, listen_socket=ListenSocket, client_list=[]}}.

handle_cast({connected, Client}, #state{client_list=ClientList}=State) ->
	io:format("Connected on port: ~p~n", [Client#client.socket]),
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
	inet:setopts(ClientSocket, [{active, true}]),
	receive
		{tcp, ClientSocket, <<"quit", _/binary>>} ->
			gen_server:cast(?MODULE, {disconnected, ClientSocket}),
			gen_server:cast(?MODULE, {message, "Server: Client dropped!"}),
			gen_tcp:close(ClientSocket);
			
		{tcp, ClientSocket, Message} ->
			io:format("tcp case ~s~n", [Message]),
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
					[StringMsgId, ToMsgId] = Frame,
					handle_confirmation_from_client({list_to_ref(StringMsgId), ToMsgId}),
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
			gen_tcp:send(ClientSocket, "reply" ++ ?DIVIDER ++ atom_to_list(Atom));
		"1" ->
			Atom = communicator:login(Username, ClientSocket, Password),
			gen_tcp:send(ClientSocket, "reply" ++ ?DIVIDER ++ atom_to_list(Atom))
	end.

handle_find_password(ClientSocket, Username) ->
	Atom = communicator:find_password(Username),
	gen_tcp:send(ClientSocket, "reply" ++ ?DIVIDER ++ atom_to_list(Atom)).

handle_find_user(ClientSocket, Username) ->
	Atom = communicator:find_user(Username),
	gen_tcp:send(ClientSocket, "reply" ++ ?DIVIDER ++ atom_to_list(Atom)).

handle_active_users(ClientSocket) ->
	ActiveUsers = ["ok"] ++ communicator:show_active_users(),
	Reply = string:join(ActiveUsers, ?DIVIDER),
	gen_tcp:send(ClientSocket, "reply" ++ ?DIVIDER ++ Reply).

handle_user_history(ClientSocket, Username) ->
	_History = communicator:user_history(Username),
	gen_tcp:send(ClientSocket, "reply" ++ ?DIVIDER ++ atom_to_list(ok)).

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