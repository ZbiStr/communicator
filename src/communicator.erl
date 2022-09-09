-module(communicator).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    login/3,
    logout/1,
    set_password/2,
    make_key/1,
    find_password/1,
    find_user/1,
    show_active_users/0,
    user_history/1,
    get_state/0,
    send_message/5,
    confirm/1,
    confirm_activity/1,
    clear_whole_table/0,
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
-define(NODE_NAME, erlangpol).
-define(COOKIE, ciasteczko).
-define(MSG_DELIVERY_TIME, 5000).
-define(AFK_TIME, 120000000).

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
    afk_timer = undefined}).
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
start_link(Lang) ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    Prompt = read_prompt(Lang, server_start),
    io:format(Prompt, [server_node()]),
	Result.

stop(Lang) ->
    gen_server:stop({?SERVER, server_node()}),
    Prompt = read_prompt(Lang, server_stop),
    io:format(Prompt).

login(Name, Address, Password) ->
    CodedName = code_to_7_bits(Name),
    case Password of
        undefined ->
            gen_server:call({?SERVER, server_node()}, {login, CodedName, Address, undefined});
        _ ->
            CodedPass = code_to_7_bits(Password),
            PubKey = make_key(Name),
			EncryptedPass = rsa_encrypt(CodedPass, PubKey),
            gen_server:call({?SERVER, server_node()}, {login, CodedName, Address, EncryptedPass})
    end.

logout(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:cast({?SERVER, server_node()}, {logout, CodedName}).

set_password(Name, Password) ->
    CodedPass = code_to_7_bits(Password),
    PubKey = communicator:make_key(Name),
	EncryptedPass = rsa_encrypt(CodedPass, PubKey),
    CodedName = code_to_7_bits(Name),
    gen_server:call({?SERVER, server_node()}, {password, CodedName, EncryptedPass}).

make_key(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:call({?SERVER, server_node()}, {make_key, CodedName}).

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
    [{decode_from_7_bits(Time),
    decode_from_7_bits(From), 
    decode_from_7_bits(Message)}
    || {Time, From, Message} <- History].

get_state() ->
    gen_server:call({?SERVER, server_node()}, get_state).

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

change_message(Message) ->
    gen_server:cast({?SERVER, server_node()}, {change_message, Message}),
    custom_server_message().

custom_server_message() ->
    gen_server:cast({?SERVER, server_node()}, custom_server_message).

confirm(MsgId) ->
    gen_server:cast({?SERVER, server_node()}, {msg_confirm_from_client, MsgId}).

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

handle_call({login, CodedName, Address, EncryptedPass}, _From, State) ->
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
                    % Custom message from server
                    gen_statem:cast(Address, 
                        {custom_server_message,
                        code_to_7_bits(State#state.server_name),
                        code_to_7_bits(State#state.message)}),
                    % starting afk timer
                    {ok, TimeRef} = timer:send_after(?AFK_TIME, {afk_time, Name}),
                    UpdatedClients = maps:put(Name, #client{address = Address, afk_timer = TimeRef}, State#state.clients),
                    log(State#state.log_file, "Temporary user \"~s\" logged on from \"~w\"", [Name, Address]),
                    {reply, {ok, State#state.server_name}, State#state{clients = UpdatedClients}};
                _ ->
                    case Client#client.address of
                        undefined ->
                            SetPass = Client#client.password,
                            DecryptedPass = rsa_decrypt(EncryptedPass, Client#client.private_key),
                            DecodedPass = decode_from_7_bits(DecryptedPass),
                            HashedPass = crypto:hash(sha256, DecodedPass),
                            case HashedPass of
                                SetPass ->
                                    % Custom message from server
                                    gen_statem:cast(Address, 
                                        {custom_server_message,
                                        code_to_7_bits(State#state.server_name),
                                        code_to_7_bits(State#state.message)}),
                                    % automatic messages sending after logging
                                    [send_message_to(Name, Time, From, Message, MsgId) || {Time, From, Message, MsgId} <- Client#client.inbox],
                                    % starting afk timer
                                    {ok, TimeRef} = timer:send_after(?AFK_TIME, {afk_time, Name}),
                                    UpdatedClients = maps:update(Name, Client#client{
                                        address = Address, 
                                        inbox = [], 
                                        afk_timer = TimeRef
                                    }, State#state.clients),
                                    log(State#state.log_file, "Registered user \"~s\" logged on from \"~w\"", [Name, Address]),
                                    {reply, {ok, State#state.server_name}, State#state{clients = UpdatedClients}};
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
handle_call({password, CodedName, EncryptedPass}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    Client = maps:get(Name, State#state.clients),
    DecryptedPass = rsa_decrypt(EncryptedPass, Client#client.private_key),
    DecodedPass = decode_from_7_bits(DecryptedPass),
    HashedPass = crypto:hash(sha256, DecodedPass),
    UpdatedClients = maps:put(Name, Client#client{password = HashedPass}, State#state.clients),
    log(State#state.log_file, "User \"~s\" registered (set password)", [Name]),
    {reply, ok, State#state{clients = UpdatedClients}};
handle_call({make_key, CodedName}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    Client = maps:get(Name, State#state.clients),
    {PubKey, PrivKey} = crypto:generate_key(rsa, {1024,65537}),
    UpdatedClients = maps:put(Name, Client#client{private_key = PrivKey}, State#state.clients),
    {reply, PubKey, State#state{clients = UpdatedClients}};
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
            {reply, [], State#state{}}
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
    gen_statem:cast(Sender#client.address, {msg_confirm_from_server, MsgId}),
    {noreply, State};
handle_cast(custom_server_message, State) ->
    % calling sending function for all users in users list except the sender
    [gen_statem:cast(Client#client.address, 
        {custom_server_message,
        code_to_7_bits(State#state.server_name),
        code_to_7_bits(State#state.message)})
    || {_Name, Client} <- maps:to_list(State#state.clients), Client#client.address =/= undefined],
    {noreply, State};
handle_cast({send_message_to, all, CodedTime, CodedFrom, CodedMessage, MsgId}, State) ->
    From = decode_from_7_bits(CodedFrom),
    Message = decode_from_7_bits(CodedMessage),
    Time = decode_from_7_bits(CodedTime),
    ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
    % calling sending function for all users in users list except the sender
    [send_message_to(Name, Time, From, Message, {MsgId, Name}) || {Name, _Client} <- ListWithoutSender],
    {noreply, State};
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
            gen_statem:cast(Client#client.address, {message, code_to_7_bits(Time), code_to_7_bits(From), code_to_7_bits(Message), MsgId}),
            % outbox update
            {ok, TimeRef} = timer:send_after(?MSG_DELIVERY_TIME, {msg_retry, MsgId}),
            NewOutbox = #msg_sent{msg_ref = MsgId, timer_ref = TimeRef, msg = {To, Time, From, Message}},
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
	send_message_to(To, Time, From, Message_txt, MsgId),
	{noreply, State#state{outbox = NewOutbox}};
handle_info({afk_time, Name}, State) ->
	logout(Name),
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

rsa_decrypt(EncPass, Priv) ->
    crypto:private_decrypt(rsa, EncPass, Priv, [{rsa_padding,rsa_pkcs1_padding},{rsa_pad, rsa_pkcs1_padding}]).

rsa_encrypt(Password, PubKey) ->
    crypto:public_encrypt(rsa, Password, PubKey, [{rsa_padding,rsa_pkcs1_padding},{rsa_mgf1_md, sha}]).

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