-module(tcp_client).
-behaviour(gen_server).

%% API
-export([
	stop/0, 
	start_link/3,
	decode_message/1, 
	call/1, 
	cast/1, 
	login/1, 
	find_password/1, 
	find_user/1, 
	active_users/0, 
	set_password/1, 
	logout/1, 
	send_message/1,
	user_history/1,
	confirmation_from_client/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-record(state, {socket, replies}).

-define(DIVIDER, "~n;~n").

% ================================================================================
% API CLIENT
% ================================================================================

stop() ->
	gen_server:call(?MODULE, stop).

start_link(IP, Port, Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [IP, Port, Opts], []).

cast(Packet) ->
	gen_server:cast(?MODULE, {cast_to_server, Packet}),
	ok.

call(Packet) ->
	cast(Packet),
	get_reply().

login(Args) ->
	Packet = string:join(["login"] ++ Args, ?DIVIDER),
	RawReply = call(Packet),
	{Reply, _}= tcp_client:decode_message(RawReply),
	Reply.

find_password(Args) ->
	Packet = string:join(["find_password"] ++ Args, ?DIVIDER),
	RawReply = call(Packet),
	{FindPass, _}= tcp_client:decode_message(RawReply),
	FindPass.

find_user(Args) ->
	Packet = string:join(["find_user"] ++ Args, ?DIVIDER),
	RawReply = call(Packet),
	{Reply, _}= tcp_client:decode_message(RawReply),
	Reply.

active_users() ->
	RawReply = call("active_users"),
	{ok, Reply} = decode_message(RawReply),
	Reply.

user_history(Args) ->
	Packet = string:join(["user_history"] ++ Args, ?DIVIDER),
	RawReply = call(Packet),
	{ok, Reply} = decode_message(RawReply),
	Reply.

set_password(Args) ->
	Packet = string:join(["set_password"] ++ Args, ?DIVIDER),
	cast(Packet).

logout(Args) ->
	Packet = string:join(["logout"] ++ Args, ?DIVIDER),
	cast(Packet).

send_message(Args) ->
	Packet = string:join(["send_message"] ++ Args, ?DIVIDER),
	cast(Packet).

confirmation_from_client(Args) ->
	Packet = string:join(["confirmation_from_client"] ++ Args, ?DIVIDER),
	cast(Packet).
% ================================================================================
% CALLBACK
% ================================================================================

init([IP, Port, Opts]) ->
	{ok, Socket} = gen_tcp:connect(IP, Port, Opts),
	Pid = spawn_link(fun() -> client_loop(Socket) end),
	gen_tcp:controlling_process(Socket, Pid),
	{ok, #state{socket = Socket, replies = []}}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(get_reply, _From, State) ->
	case State#state.replies of 
		[] -> 
			{reply, try_again, State};
		[H|T] ->
			{reply, H, State#state{replies = T}}
		end;

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({cast_to_server, Packet}, State) ->
	gen_tcp:send(State#state.socket, Packet),
	{noreply, State};

handle_cast({reply, Reply}, State) ->
	{noreply, State#state{replies = State#state.replies ++ [Reply]}}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

% ================================================================================
% INTERNAL
% ================================================================================
client_loop(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Message} ->
			case decode_message(Message) of
				% CALL REPLIES FROM SERVER
				{reply, Frame} ->
					io:format("Reply frame: ~p~n", [Frame]),
					handle_reply(string:join(Frame, ?DIVIDER)),
					client_loop(Socket);
				% CASTS FROM SERVER
				{confirmation_from_server, Frame} ->
					[MsgId] = Frame,
					handle_confirmation_from_server(list_to_ref(MsgId)),
					client_loop(Socket);
				{message, Frame} ->
					[Time, From, MessageTxt, StringMsgId, ToMsgId] = Frame,
					handle_send_to_client(Time, From, MessageTxt, {list_to_ref(StringMsgId), ToMsgId}),
					client_loop(Socket);
				{_, Frame} ->
					io:format("Unsupported frame:~n~p~n", [Frame]),
					client_loop(Socket);
				{error, Reason, Frame} ->
					io:format("Error! Reason: ~p~n~p~n", [Reason, Frame])
			end
	end.

get_reply() ->
	Reply = gen_server:call(?MODULE, get_reply),
	case Reply of
		try_again ->
			get_reply();
		_ ->
			Reply
	end.
	
handle_reply(Reply) ->
	gen_server:cast(?MODULE, {reply, Reply}).

handle_confirmation_from_server(MsgId) ->
	client:receive_confirmation_from_server(MsgId).

handle_send_to_client(Time, From, Message, MsgId) ->
	client:receive_message(Time, From, Message, MsgId).

decode_message(Message) ->
	[H|Frame] = string:split(Message,?DIVIDER, all),
	Atom = list_to_atom(H),
	{Atom, Frame}.