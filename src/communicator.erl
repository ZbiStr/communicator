-module(communicator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,
    stop/1,
    login/3,
    logout/1,
    set_password/2,
    make_key/1,
    find_password/1,
    find_user/1,
    list_of_users/0,
    user_history/1,
    get_state/0,
    send_message/5,
    confirm/1,
    confirm_activity/1,
    clear_whole_table/2,
    change_message/1
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
-define(NODE_NAME, dzialajkupo).
-define(COOKIE, ciasteczko).
-define(MSG_DELIVERY_TIME, 5000).
-define(AFK_TIME, 120000).

-record(state, {
    server_name = undefined,
    log_file = undefined,
    max_clients = undefined,
    message = undefined,
    clients = #{},
    outbox = []}).
-record(client, {
    address = undefined,
    inbox=[],
    password = undefined,
    private_key = undefined,
    afk_timer = undefined, 
    logout_time = "undefined", 
    login_time = "undefined", 
    last_msg_time = "undefined"
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
start_link() ->
    start_link(en).

start_link(Lang) ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    Prompt = read_prompt(Lang, server_start),
    io:format(Prompt, [server_node()]),
    Result.

stop() ->
    stop(en).

stop(Lang) ->
    gen_server:stop({?SERVER, server_node()}),
    Prompt = read_prompt(Lang, server_stop),
    io:format(Prompt).

login(Name, Address, Password) ->
    gen_server:call({?SERVER, server_node()}, {login, Name, Address, Password}).

logout(Name) ->
    gen_server:cast({?SERVER, server_node()}, {logout, Name}).

set_password(Name, EncryptedPass) ->
    gen_server:cast({?SERVER, server_node()}, {set_password, Name, EncryptedPass}).

make_key(Name) ->
    gen_server:call({?SERVER, server_node()}, {make_key, Name}).

find_password(Name) ->
    gen_server:call({?SERVER, server_node()}, {find_password, Name}).

find_user(Name) ->
    gen_server:call({?SERVER, server_node()}, {find_user, Name}).

list_of_users() ->
    gen_server:call({?SERVER, server_node()}, list_of_users).

user_history(Username) ->
    History = gen_server:call({?SERVER, server_node()}, {history, Username}),
    case History of
        empty ->
            empty;
        _ ->
            [{Time, From, Message}
            || {Time, From, Message} <- History]
    end.

get_state() ->
    gen_server:call({?SERVER, server_node()}, get_state).

send_message(To, Time, From, Message, MsgId) ->
    gen_server:cast({?SERVER, server_node()}, {confirm_and_send, To, Time, From, Message, MsgId}).

send_message_to(To, Time, From, Message, MsgId) ->
    case To of
        all ->
            gen_server:cast({?SERVER, server_node()}, {send_message_to, To, Time, From, Message, MsgId});
        _ ->
            gen_server:cast({?SERVER, server_node()}, {send_message_to, To, Time, From, Message, MsgId})
    end.

change_message(Message) ->
    gen_server:cast({?SERVER, server_node()}, {change_message, Message}),
    custom_server_message().

custom_server_message() ->
    gen_server:cast({?SERVER, server_node()}, custom_server_message).

confirm(MsgId) ->
    gen_server:cast({?SERVER, server_node()}, {msg_confirmation_from_client, MsgId}).

confirm_activity(Name) ->
    gen_server:cast({?SERVER, server_node()}, {i_am_active, Name}).
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
    {ok, {ServerName,MaxNumber,LogFilePath, CustomMessage}} = load_configuration("server_config.txt"),
    log(LogFilePath, "Server with name \"~s\" has been started. Created on node ~p", [ServerName,server_node()]),
    RegisteredUsers = read_server_status(),
    {ok, #state{
        server_name = ServerName,
        max_clients = list_to_integer(MaxNumber),
        log_file = LogFilePath,
        message = CustomMessage,
        clients = RegisteredUsers}}.

handle_call({login, Name, Address, EncryptedPass}, _From, State) ->
    ListOfUsers = maps:to_list(State#state.clients),
    ActiveUsers = [ Name1 || {Name1, Client} <- ListOfUsers, Client#client.address =/= undefined ],
    NumberOfActiveUsers = length(ActiveUsers),
    if  
        NumberOfActiveUsers >= State#state.max_clients ->
            log(State#state.log_file, "Login attempt: \"~s\" from \"~w\" failed with max_reached", [Name, Address]),
            {reply, {max_reached, State#state.server_name}, State};
        true ->
            Client = maps:get(Name, State#state.clients, not_found),
            case Client of
                not_found ->
                    % Custom message from server
                    tcp_server:custom_server_message(Address, [State#state.server_name, State#state.message]),
                    % starting afk timer
                    {ok, TimeRef} = timer:send_after(?AFK_TIME, {afk_time, Name}),
                    UpdatedClients = maps:put(Name, #client{address = Address, afk_timer = TimeRef, login_time = get_time(), logout_time = "temp"}, State#state.clients),
                    log(State#state.log_file, "Temporary user \"~s\" logged on from \"~w\"", [Name, Address]),
                    {reply, {ok, State#state.server_name}, State#state{clients = UpdatedClients}};
                _ ->
                    case Client#client.address of
                        undefined ->
                            SetPass = Client#client.password,
                            DecryptedPass = rsa_decrypt(EncryptedPass, Client#client.private_key),
                            HashedPass = crypto:hash(sha256, DecryptedPass),
                            case HashedPass of
                                SetPass ->
                                    % Custom message from server
                                    tcp_server:custom_server_message(Address, [State#state.server_name, State#state.message]),
                                    % automatic messages sending after logging
                                    [send_message_to(Name, Time, From, Message, MsgId) || {Time, From, Message, MsgId} <- Client#client.inbox],
                                    % starting afk timer
                                    {ok, TimeRef} = timer:send_after(?AFK_TIME, {afk_time, Name}),
                                    UpdatedClients = maps:update(Name, Client#client{
                                        address = Address, 
                                        inbox = [], 
                                        afk_timer = TimeRef,
                                        login_time = get_time(),
                                        logout_time = "active"
                                    }, State#state.clients),
                                    log(State#state.log_file, "Registered user \"~s\" logged on from \"~w\"", [Name, Address]),
                                    {reply, {ok, State#state.server_name}, State#state{clients = UpdatedClients}};
                                _ ->
                                    log(State#state.log_file, "Login attempt: \"~s\" from \"~w\" failed with wrong_password", [Name, Address]),
                                    {reply, {wrong_password, State#state.server_name}, State#state{}}
                            end;
                        _ ->
                            log(State#state.log_file, "Login attempt: \"~s\" from \"~w\" failed with alredy_exists", [Name, Address]),
                            {reply, {already_exists, State#state.server_name}, State}
                    end
            end
    end;
handle_call({make_key, Name}, _From, State) ->
    Client = maps:get(Name, State#state.clients),
    {PubKey, PrivKey} = crypto:generate_key(rsa, {1024,65537}),
    UpdatedClients = maps:put(Name, Client#client{private_key = PrivKey}, State#state.clients),
    {reply, PubKey, State#state{clients = UpdatedClients}};
handle_call({find_password, Name}, _From, State) ->
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
handle_call({find_user, Name}, _From, State) ->
    case maps:get(Name, State#state.clients, not_found) of
        not_found ->
            {reply, does_not_exist, State#state{}};
        _Client ->
            {reply, ok, State#state{}}
    end;
handle_call(list_of_users, _From, State) ->
    log(State#state.log_file, "Show active users called", []),
    MapToList = maps:to_list(State#state.clients),
    ListOfUsers = [ {
                        Name, 
                        Client#client.last_msg_time, 
                        Client#client.login_time, 
                        Client#client.logout_time
                    } || {Name, Client} <- MapToList ],
    {reply, ListOfUsers, State#state{}};
handle_call({history, Username}, _From, State) ->
    log(State#state.log_file, "User \"~s\" requested his massage history", [Username]),
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    case dets:lookup(messages, Username) of
        [{Username, History}] ->
            dets:close(Table),
            {reply, History, State#state{}};
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
handle_cast({logout, Name}, State) ->
    {ok, Client} = maps:find(Name, State#state.clients),
    case Client#client.password of
        undefined ->
            UpdatedClients = maps:without([Name], State#state.clients),
            log(State#state.log_file, "Temporary user \"~s\" logged out", [Name]),
            {noreply, State#state{clients = UpdatedClients}};
        _Password ->
            log(State#state.log_file, "Registered user \"~s\" logged out", [Name]),
            UpdatedClients = maps:update(Name, Client#client{address = undefined, logout_time = get_time()}, State#state.clients),
            {noreply, State#state{clients = UpdatedClients}}
    end;
handle_cast({set_password, Name, EncryptedPass}, State) ->
    Client = maps:get(Name, State#state.clients),
    DecryptedPass = rsa_decrypt(EncryptedPass, Client#client.private_key),
    HashedPass = crypto:hash(sha256, DecryptedPass),
    UpdatedClients = maps:put(Name, Client#client{password = HashedPass, logout_time = "active"}, State#state.clients),
    log(State#state.log_file, "User \"~s\" registered (set password)", [Name]),
    {noreply, State#state{clients = UpdatedClients}};
handle_cast({confirm_and_send, To, Time, From, Message, MsgId}, State) ->
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
    UpdateSender = maps:update(From, Sender#client{last_msg_time = Time}, State#state.clients),
    {noreply, State#state{clients = UpdateSender}};
handle_cast(custom_server_message, State) ->
    % calling sending function for all users in users list except the sender
    [tcp_server:custom_server_message(Client#client.address, [State#state.server_name, State#state.message])
    || {_Name, Client} <- maps:to_list(State#state.clients), Client#client.address =/= undefined],
    {noreply, State};
handle_cast({send_message_to, all, Time, From, Message, MsgId}, State) ->
    ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
    % calling sending function for all users in users list except the sender
    [send_message_to(Name, Time, From, Message, MsgId) || {Name, _Client} <- ListWithoutSender],
    {noreply, State#state{}};
handle_cast({send_message_to, To, Time, From, Message, MsgId}, State) ->
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
handle_cast({change_message, Message}, State) ->
    {noreply, State#state{message = Message}};
handle_cast({msg_confirm_from_client, MsgId}, State) ->
	{MsgSent, NewOutBox} = take_msg_by_ref(MsgId, State#state.outbox),
	timer:cancel(MsgSent#msg_sent.timer_ref),
    log(State#state.log_file, "Message with ID ~p has been delivered", [MsgId]),
    {To, Time, From, Message_txt} = MsgSent#msg_sent.msg,
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
handle_cast({i_am_active, Name}, State) ->
    Client = maps:get(Name, State#state.clients, not_found),
    case Client of
        not_found ->
            {noreply, State};
        _ ->
            timer:cancel(Client#client.afk_timer),
            {ok, NewTimeRef} = timer:send_after(?AFK_TIME, {afk_time, Name}),
            UpdatedClients = maps:update(Name, Client#client{afk_timer = NewTimeRef}, State#state.clients),
            {noreply, State#state{clients = UpdatedClients}}
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
    {MsgId_ref, To} = MsgId,
	send_message_to(To, Time, From, Message_txt, MsgId_ref),
	{noreply, State#state{outbox = NewOutbox}};
handle_info({afk_time, Name}, State) ->
    {ok, Client} = maps:find(Name, State#state.clients),
    tcp_server:automatic_logout(Client#client.address, [Name]),
    log(State#state.log_file, "User \"~s\" has been automatically logged out", [Name]),
	{noreply, State};
handle_info(Info, State) ->
    log(State#state.log_file, "Unrecognized info request ~w", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    log(State#state.log_file, "\"~s\" has been terminated with reason ~p", [State#state.server_name, Reason]),
    save_to_file_server_status(State), 
    net_kernel:stop(),
    ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================
save_to_file_server_status(State) ->
    ListOfUsers = maps:to_list(State#state.clients),
    {ok, File} = dets:open_file(server_status, [{file, "server_status"}, {type, set}]),
    RegisteredUsers = [{Name, Client#client{address = undefined}} || {Name, Client} <- ListOfUsers, Client#client.password =/= undefined],
    dets:insert(server_status, {keyOfUsers, RegisteredUsers}),
    % io:format("~p~n", [dets:lookup(server_status, keyOfUsers)]),
    dets:close(File).

read_server_status() ->
    {ok, File} = dets:open_file(server_status, [{file, "server_status"}, {type, set}]),
    LookupReply = dets:lookup(server_status, keyOfUsers),
    dets:close(File),
    case LookupReply of
        [] ->
            #{};  
        _ ->
            [{keyOfUsers, RegisteredUsers}] = LookupReply,
            _RegisteredUsersInMap = maps:from_list(RegisteredUsers)    
    end.

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

clear_whole_table(Table, File) ->
    % delete all messages history
    {ok, Table} = dets:open_file(Table, [{file, File}, {type, set}]),
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

rsa_decrypt(EncPass, Priv) ->
    BinPass = list_to_binary(EncPass),
    crypto:private_decrypt(rsa, BinPass, Priv, [{rsa_padding,rsa_pkcs1_padding},{rsa_pad, rsa_pkcs1_padding}]).

get_time() ->
    {{Y,M,D},{H,Min,S}} = calendar:local_time(),
    Year = integer_to_list(Y),
    TempTime = [ "00" ++ integer_to_list(X) || X <- [M, D, H, Min, S]],
    [Month,Day,Hour,Minute,Second] = [lists:sublist(X, lists:flatlength(X) - 1, 2) || X <- TempTime],
    Year ++ "/" ++ Month ++ "/" ++ Day ++ " " ++ Hour ++ ":" ++ Minute ++ ":" ++ Second.

load_configuration(ConfigPath) ->
    try file:open(ConfigPath, [read]) of
        {ok, IoDevice} ->
            PromptServerName = "Server name: ",
            PromptMaxClients = "Max number of clients: ",
            PromptLogFilePath = "Log file path: ",
            PromptCustomMessage = "Default server message: ",
            _Trash = io:get_line(IoDevice,""),
            ServerName = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptServerName,
            MaxNumber = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptMaxClients,
            LogFilePath = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptLogFilePath,
            CustomMessage = string:trim(io:get_line(IoDevice,""), trailing, [$\n]) -- PromptCustomMessage,
            file:close(ConfigPath),
            {ok, {ServerName,MaxNumber,LogFilePath, CustomMessage}}
    catch
        error:Reason ->
            io:format("Loading config file failed with reason: ~w",[Reason]),
            {error, Reason}
    end.

log(LogFilePath, LogMessage, Args) ->
    try file:open(LogFilePath, [append]) of
        {ok, IoDevice} ->
            Time = get_time(),
            io:format(IoDevice, Time ++ " " ++ LogMessage ++ "~n", Args),
            file:close(LogFilePath)
    catch
        error:Reason ->
            io:format("Opening the log file failed with reason: ~w",[Reason])
    end,
    ok.

server_node() ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(?NODE_NAME) ++ "@" ++ Host).

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