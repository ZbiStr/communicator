-module(communicator).
-behaviour(gen_server).

%% API
-export([
    start_link/0, 
    stop/0, 
    login/3, 
    logout/1, 
    set_password/2, 
    find_password/1, 
    find_user/1, 
    show_active_users/0, 
    user_history/1, 
    get_state/0, 
    send_message/5, 
    confirm/1, 
    clear_whole_table/0
]).
%% CALLBACK
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(NODE_NAME, noplosze).
-define(COOKIE, ciasteczko).
-define(MSG_DELIVERY_TIME, 5000).

-record(state, {
    server_name = undefined,
    log_file = undefined,
    max_clients = undefined,
    clients = #{},
    outbox = []}).
-record(client, {
    address = undefined,
    inbox=[],
    password = undefined}).
-record(msg_sent, {
	msg_ref, 
	timer_ref, 
	msg
}).

% ================================================================================
% API
% ================================================================================
start_link() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    io:format("Communicator server has been started. Created on ~p~n", [server_node()]),
	Result.

stop() ->
    gen_server:stop({?SERVER, server_node()}),
    io:format("Communicator server has been closed~n").

login(Name, Address, Password) ->
    CodedName = code_to_7_bits(Name),
    case Password of
        undefined ->
            gen_server:call({?SERVER, server_node()}, {login, CodedName, Address, undefined});
        _ ->
            CodedPassword = code_to_7_bits(Password),
            gen_server:call({?SERVER, server_node()}, {login, CodedName, Address, CodedPassword})
    end.

logout(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:cast({?SERVER, server_node()}, {logout, CodedName}).

find_password(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:call({?SERVER, server_node()}, {find_password, CodedName}).

find_user(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:call({?SERVER, server_node()}, {find_user, CodedName}).

show_active_users() ->
    gen_server:call({?SERVER, server_node()}, show_active_users).

user_history(Username) ->
    CodedUsername = code_to_7_bits(Username),
    History = gen_server:call({?SERVER, server_node()}, {history, CodedUsername}),
    case History of
        empty ->
            empty;
        _ ->
            [{decode_from_7_bits(Time),
            decode_from_7_bits(From), 
            decode_from_7_bits(Message)}
            || {Time, From, Message} <- History]
    end.

get_state() ->
    gen_server:call({?SERVER, server_node()}, get_state).

set_password(Name, Password) ->
    CodedName = code_to_7_bits(Name),
    CodedPassword = code_to_7_bits(Password),
    gen_server:cast({?SERVER, server_node()}, {set_password, CodedName, CodedPassword}).

send_message(To, Time, From, Message, MsgId) ->
    CodedFrom = code_to_7_bits(From),
    CodedMessage = code_to_7_bits(Message),
    CodedTime = code_to_7_bits(Time),
    gen_server:cast({?SERVER, server_node()}, {confirm_and_send, To, CodedTime, CodedFrom, CodedMessage, MsgId}).

send_message_to(To, Time, From, Message, MsgId) ->
    CodedFrom = code_to_7_bits(From),
    CodedMessage = code_to_7_bits(Message),
    CodedTime = code_to_7_bits(Time),
    case To of
        all ->
            gen_server:cast({?SERVER, server_node()}, {send_message_to, To, CodedTime, CodedFrom, CodedMessage, MsgId});
        _ ->
            CodedTo = code_to_7_bits(To),
            gen_server:cast({?SERVER, server_node()}, {send_message_to, CodedTo, CodedTime, CodedFrom, CodedMessage, MsgId})
    end.
confirm(MsgId) ->
    gen_server:cast({?SERVER, server_node()}, {msg_confirmation_from_client, MsgId}).
% ================================================================================
% CALLBACK
% ================================================================================
init(_Args) ->
    case node() of
        'nonode@nohost' ->
            net_kernel:start(?NODE_NAME, #{name_domain => shortnames});
        _ ->
            ok
    end,
    erlang:set_cookie(local, ?COOKIE),
    {ok, {ServerName,MaxNumber,LogFilePath}} = load_configuration("server_config.txt"),
    log(LogFilePath, "Server with name \"~s\" has been started. Created on node ~p", [ServerName,server_node()]),
    {ok, #state{server_name = ServerName, max_clients = list_to_integer(MaxNumber), log_file = LogFilePath}}.

handle_call({login, CodedName, Address, CodedPassword}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    ListOfUsers = maps:to_list(State#state.clients),
    ActiveUsers = [ Name1 || {Name1, Client} <- ListOfUsers, Client#client.address =/= undefined ],
    NumberOfActiveUsers = length(ActiveUsers),
    if  
        NumberOfActiveUsers >= State#state.max_clients ->
            log(State#state.log_file, "Login attempt: \"~s\" from \"~w\" failed with max_reached", [Name, Address]),
            {reply, max_reached, State};
        true ->
            Client = maps:get(Name, State#state.clients, not_found),
            case Client of
                not_found ->
                    UpdatedClients = maps:put(Name, #client{address = Address}, State#state.clients),
                    log(State#state.log_file, "Temporary user \"~s\" logged on from \"~w\"", [Name, Address]),
                    {reply, ok, State#state{clients = UpdatedClients}};
                _ ->
                    case Client#client.address of
                        undefined ->
                            SetPass = Client#client.password,
                            Password = decode_from_7_bits(CodedPassword),
                            case Password of
                                SetPass ->
                                    % automatic messages sending after logging
                                    [send_message_to(Name, Time, From, Message, MsgId) || {Time, From, Message, MsgId} <- Client#client.inbox],
                                    UpdatedClients = maps:update(Name, Client#client{address = Address, inbox = []}, State#state.clients),
                                    log(State#state.log_file, "Registered user \"~s\" logged on from \"~w\"", [Name, Address]),
                                    {reply, ok, State#state{clients = UpdatedClients}};
                                _ ->
                                    log(State#state.log_file, "Login attempt: \"~s\" from \"~w\" failed with wrong_password", [Name, Address]),
                                    {reply, wrong_password, State#state{}}
                            end;
                        _ ->
                            log(State#state.log_file, "Login attempt: \"~s\" from \"~w\" failed with alredy_exists", [Name, Address]),
                            {reply, already_exists, State}
                    end
            end
    end;
handle_call({find_password, CodedName}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    Client = maps:get(Name, State#state.clients, not_found),
    case Client of
        not_found ->
            {reply, undefined, State};
        _ ->
            case Client#client.password of
                undefined -> 
                    {reply, undefined, State};
                _ -> 
                    {reply, defined, State}
            end
        end;
handle_call({find_user, CodedName}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    case maps:get(Name, State#state.clients, not_found) of
        not_found ->
            {reply, does_not_exist, State#state{}};
        _Client ->
            {reply, ok, State#state{}}
    end;
handle_call(show_active_users, _From, State) ->
    log(State#state.log_file, "Show active users called", []),
    ListOfUsers = maps:to_list(State#state.clients),
    ActiveUsers = [ Name || {Name, Client} <- ListOfUsers, Client#client.address =/= undefined ],
    {reply, ActiveUsers, State#state{}};
handle_call({history, CodedUsername}, _From, State) ->
    Username = decode_from_7_bits(CodedUsername),
    log(State#state.log_file, "User \"~s\" requested his massage history", [Username]),
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    case dets:lookup(messages, Username) of
        [{Username, History}] ->
            CodedHistory = [ {code_to_7_bits(Time), code_to_7_bits(From), code_to_7_bits(Message)} || {Time, From, Message} <- History ],
            dets:close(Table),
            {reply, CodedHistory, State#state{}};
        [] -> 
            dets:close(Table),
            {reply, empty, State#state{}}
    end;
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    log(State#state.log_file, "\"~s\" has been closed", [State#state.server_name]),
    net_kernel:stop(),
    {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
    log(State#state.log_file, "Unrecognized call request ~p", [Request]),
    {reply, ok, State}.

% CASTS
handle_cast({logout, CodedName}, State) ->
    Name = decode_from_7_bits(CodedName),
    {ok, Client} = maps:find(Name, State#state.clients),
    case Client#client.password of
        undefined ->
            UpdatedClients = maps:without([Name], State#state.clients),
            log(State#state.log_file, "Temporary user \"~s\" logged out", [Name]),
            {noreply, State#state{clients = UpdatedClients}};
        _Password ->
            log(State#state.log_file, "Registered user \"~s\" logged out", [Name]),
            UpdatedClients = maps:update(Name, Client#client{address = undefined}, State#state.clients),
            {noreply, State#state{clients = UpdatedClients}}
    end;
handle_cast({set_password, CodedName, CodedPassword}, State) ->
    Name = decode_from_7_bits(CodedName),
    Password = decode_from_7_bits(CodedPassword),
    Client = maps:get(Name, State#state.clients),
    UpdatedClients = maps:put(Name, Client#client{password = Password}, State#state.clients),
    log(State#state.log_file, "User \"~s\" registered (set password)", [Name]),
    {noreply, State#state{clients = UpdatedClients}};
handle_cast({confirm_and_send, To, CodedTime, CodedFrom, CodedMessage, MsgId}, State) ->
    From = decode_from_7_bits(CodedFrom),
    Message = decode_from_7_bits(CodedMessage),
    Time = decode_from_7_bits(CodedTime),
    % calling the actual sending function
    send_message_to(To, Time, From, Message, MsgId),
    case To of
        all ->
            ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)),
            log(State#state.log_file, "User \"~s\" send message: \"~s\" with MasgId: ~p to all: ~p", [From, Message, MsgId, [Name || {Name, _Client} <- ListWithoutSender]]);
        _->
            log(State#state.log_file, "User \"~s\" send message: \"~s\" with MasgId: ~p to: \"~s\"", [From, Message, MsgId, To])
    end,
    % sending sender a confirmation of receiving the message by server 
    {ok, Sender} = maps:find(From, State#state.clients),
    tcp_server:confirmation_from_server(Sender#client.address, [ref_to_list(MsgId)]),
    {noreply, State};
    
handle_cast({send_message_to, CodedTo, CodedTime, CodedFrom, CodedMessage, MsgId}, State) when CodedTo == all ->
    From = decode_from_7_bits(CodedFrom),
    Message = decode_from_7_bits(CodedMessage),
    Time = decode_from_7_bits(CodedTime),
    ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
    % calling sending function for all users in users list except the sender
    [send_message_to(Name, Time, From, Message, MsgId) || {Name, _Client} <- ListWithoutSender],
    {noreply, State#state{}};
handle_cast({send_message_to, CodedTo, CodedTime, CodedFrom, CodedMessage, MsgId}, State) ->
    To = decode_from_7_bits(CodedTo),
    From = decode_from_7_bits(CodedFrom),
    Message = decode_from_7_bits(CodedMessage),
    Time = decode_from_7_bits(CodedTime),
    {ok, Client} = maps:find(To, State#state.clients),
    case Client#client.address of
        undefined -> % inbox update for registered & logged out
            UpdatedClients = maps:update(To, Client#client{inbox = Client#client.inbox ++ [{Time, From, Message, MsgId}]}, State#state.clients),
            {noreply, State#state{clients = UpdatedClients}};
        _ -> % sending message for logged in users
            timer:sleep(10), % nie wiem naprawde XDDDDD Ale z tym dziala ((((: 
                             % inaczej badmatch bo gdzieś dokleja "reply", nie mam siły szukać o co chodzi
            tcp_server:send_to_client(Client#client.address, [Time, From, Message, ref_to_list(MsgId), To]),
            % outbox update
            {ok, TimeRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_retry, {MsgId, To}}),
            NewOutbox = #msg_sent{msg_ref = {MsgId, To}, timer_ref = TimeRef, msg = {To, Time, From, Message}},
            UpdatedOutbox = State#state.outbox ++ [NewOutbox],
            {noreply, State#state{outbox = UpdatedOutbox}}
    end;

handle_cast({msg_confirmation_from_client, MsgId}, State) ->
	{MsgSent, NewOutBox} = take_msg_by_ref(MsgId, State#state.outbox),
	timer:cancel(MsgSent#msg_sent.timer_ref),
    log(State#state.log_file, "Message with ID ~p has been delivered", [MsgId]),
    {To, Time, From, Message_txt} = MsgSent#msg_sent.msg,
    {ok, Client} = maps:find(To, State#state.clients),
    Client = maps:get(To, State#state.clients, not_found),
    case Client of
        not_found ->
            {noreply, State#state{outbox = NewOutBox}};
        _ ->
            case Client#client.password of
                        undefined ->
                            {noreply, State#state{outbox = NewOutBox}};
                        _ ->
                            save_to_file(To, Time, From, Message_txt),
                            {noreply, State#state{outbox = NewOutBox}}
            end
    end;
handle_cast(Msg, State) ->
    log(State#state.log_file, "Unrecognized cast request ~w", [Msg]),
    {noreply, State}.

handle_info({msg_retry, MsgId}, State) ->
	{Message, Outbox1} = take_msg_by_ref(MsgId, State#state.outbox),
	{ok, TimerRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_retry, MsgId}),
	NewMsg = Message#msg_sent{timer_ref = TimerRef}, 
	{To, Time, From, Message_txt} = Message#msg_sent.msg,
	NewOutbox = Outbox1 ++ [NewMsg],
	send_message_to(To, Time, From, Message_txt, MsgId),
	{noreply, State#state{outbox = NewOutbox}};
handle_info(Info, State) ->
    log(State#state.log_file, "Unrecognized info request ~w", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    log(State#state.log_file, "\"~s\" has been terminated with reason ~p", [State#state.server_name, Reason]),
    net_kernel:stop(),
    ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================

save_to_file(Username, Time, From, Message) ->
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    case dets:member(Table, Username) of
        false ->
            % saving message for user with empty history
            dets:insert(messages, {Username, [{Time, From, Message}]});
        true ->
            save_to_file_when_existed(Username, Time, From, Message)
    end,
    dets:close(Table).

save_to_file_when_existed(Username, Time, From, Message) ->
    % saving message for user with non-empty history
    [{Username, Inbox}] = dets:lookup(messages, Username), 
    UpdatedInbox = Inbox ++ [{Time, From, Message}],
    dets:insert(messages, {Username, UpdatedInbox}).

clear_whole_table() ->
    % delete all messages history
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    dets:delete_all_objects(Table),
    dets:close(Table).

% finding message by its ID, returning message and updated outbox
take_msg_by_ref(MsgId, Outbox) ->
	take_msg_by_ref(MsgId, Outbox, []).
take_msg_by_ref(_MsgId, [], _Acc) ->
	{not_found, []};
take_msg_by_ref(MsgId, [SentMsg | Tl], Acc) when SentMsg#msg_sent.msg_ref == MsgId ->
	{SentMsg, Acc ++ Tl};
take_msg_by_ref(MsgId, [H | Tl], Acc) ->
	take_msg_by_ref(MsgId, Tl, Acc ++ [H]).

code_to_7_bits(Input) ->
	Bit = <<  <<(A-32)>> || A <- Input>>,
	<< <<Code>> || <<_A:1,Code:7>> <= Bit>>.

decode_from_7_bits(Input) ->
	Bit = << <<0:1,Code:7>> || <<Code>> <= Input>>,
	[(A+32) || <<A:8>> <= Bit].

load_configuration(ConfigPath) ->
    try file:open(ConfigPath, [read]) of
        {ok, IoDevice} ->
            PromptServerName = "Server name: ",
            PromptMaxClients = "Max number of clients: ",
            PromptLogFilePath = "Log file path: ",
            _Trash = io:get_line(IoDevice,""),
            ServerName = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptServerName,
            MaxNumber = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptMaxClients,
            LogFilePath = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptLogFilePath,
            file:close(ConfigPath),
            {ok, {ServerName,MaxNumber,LogFilePath}}
    catch
        error:Reason ->
            io:format("Loading config file failed with reason: ~w",[Reason]),
            {error, Reason}
    end.

log(LogFilePath, LogMessage, Args) ->
    try file:open(LogFilePath, [append]) of
        {ok, IoDevice} ->
            {{Y,M,D},{H,Min,S}} = calendar:local_time(),
            Year = integer_to_list(Y),
            TempTime = [ "00" ++ integer_to_list(X) || X <- [M, D, H, Min, S]],
            [Month,Day,Hour,Minute,Second] = [lists:sublist(X, lists:flatlength(X) - 1, 2) || X <- TempTime],
            Time =  Year ++ "/" ++ Month ++ "/" ++ Day ++ " " ++ Hour ++ ":" ++ Minute ++ ":" ++ Second ++ " ",
            io:format(IoDevice, Time ++ LogMessage ++ "~n", Args),
            file:close(LogFilePath)
    catch
        error:Reason ->
            io:format("Opening the log file failed with reason: ~w",[Reason])
    end,
    ok.

server_node() ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(?NODE_NAME) ++ "@" ++ Host).