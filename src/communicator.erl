-module(communicator).
-behaviour(gen_server).

%% API
-export([stop/0, start_link/0, logout/1, send_message/4, set_password/2, find_user/1, show_active_users/0, find_password/1, login/3, user_history/1, clear_whole_table/0]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(NODE_NAME, erlangpol).
-define(COOKIE, ciasteczko).

-record(state, {
    server_name = undefined,
    log_file = undefined,
    max_clients = undefined,
    clients = #{}}).
-record(client, {
    address = undefined,
    inbox=[],
    password = undefined}).

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
    gen_server:call({?SERVER, server_node()}, {logout, CodedName}).

send_message(To, Time, From, Message) ->
    CodedFrom = code_to_7_bits(From),
    CodedMessage = code_to_7_bits(Message),
    CodedTime = code_to_7_bits(Time),
    case To of
        all ->
            gen_server:cast({?SERVER, server_node()},{send_message, To, CodedTime, CodedFrom, CodedMessage});
        _ ->
            CodedTo = code_to_7_bits(To),
            gen_server:cast({?SERVER, server_node()},{send_message, CodedTo, CodedTime, CodedFrom, CodedMessage})
    end.

set_password(Name, Password) ->
    CodedName = code_to_7_bits(Name),
    CodedPassword = code_to_7_bits(Password),
    gen_server:call({?SERVER, server_node()}, {password, CodedName, CodedPassword}).

find_user(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:call({?SERVER, server_node()}, {find_user, CodedName}).

show_active_users() ->
    gen_server:call({?SERVER, server_node()}, show_active_users).

find_password(Name) ->
    CodedName = code_to_7_bits(Name),
    gen_server:call({?SERVER, server_node()},{find_password, CodedName}).

user_history(Username) ->
    CodedUsername = code_to_7_bits(Username),
    History = gen_server:call({?SERVER, server_node()},{history, CodedUsername}),
    [{decode_from_7_bits(Time),
    decode_from_7_bits(From), 
    decode_from_7_bits(Message)}
    || {Time, From, Message} <- History].


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
    %% Przeszukiwanie mapy, zwrócenie already_exists w przypadku gdy użytkownik Name
    %% już w niej występuje, dodanie go w przeciwnym przypadku.
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
                                    [send_message(Name, Time, From, Message) || {Time, From, Message} <- Client#client.inbox],
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
handle_call({logout, CodedName}, _From, State) ->
    %% Przeszukiwanie mapy, zwrócenie does_not_exists w przypadku gdy użytkownik Name
    %% w niej nie występuje, usunięcie go w przeciwnym przypadku.
    Name = decode_from_7_bits(CodedName),
    {ok, Client} = maps:find(Name, State#state.clients),
    case Client#client.password of
        undefined ->
            UpdatedClients = maps:without([Name], State#state.clients),
            log(State#state.log_file, "Temporary user \"~s\" logged out", [Name]),
            {reply, ok, State#state{clients = UpdatedClients}};
        _Password ->
            log(State#state.log_file, "Registered user \"~s\" logged out", [Name]),
            UpdatedClients = maps:update(Name, Client#client{address = undefined}, State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}}
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
handle_call({password, CodedName, CodedPassword}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    Password = decode_from_7_bits(CodedPassword),
    Client = maps:get(Name, State#state.clients),
    UpdatedClients = maps:put(Name, Client#client{password = Password}, State#state.clients),
    log(State#state.log_file, "User \"~s\" registered (set password)", [Name]),
    {reply, ok, State#state{clients = UpdatedClients}};
handle_call({find_password, CodedName}, _From, State) ->
    Name = decode_from_7_bits(CodedName),
    Client = maps:get(Name, State#state.clients, not_found), %szukam po Name
    case Client of
        not_found ->
            {reply, undefined, State};
        _ ->
            {reply, Client#client.password, State}
        end;
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
handle_call(stop, _From, State) ->
    log(State#state.log_file, "\"~s\" has been closed", [State#state.server_name]),
    net_kernel:stop(),
    {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
    log(State#state.log_file, "Unrecognized call request ~w", [Request]),
    {reply, ok, State}.
    
handle_cast({send_message, CodedTo, CodedTime, CodedFrom, CodedMessage}, State) ->
    From = decode_from_7_bits(CodedFrom),
    Message = decode_from_7_bits(CodedMessage),
    Time = decode_from_7_bits(CodedTime),
    case CodedTo of
        all ->
            {ok, Value} = maps:find(From, State#state.clients),
            %% Wysyłanie wiadomości do wszystkich aktywnych osób poza nadawcą
            ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
            [gen_statem:cast(Client#client.address, {message, code_to_7_bits(Time), code_to_7_bits(From), code_to_7_bits(Message)}) 
            || {_Name, Client} <- ListWithoutSender, Client#client.address =/= undefined],
            log(State#state.log_file, "User \"~s\" send message: \"~s\" to all: ~p", [From, Message, [Name || {Name, _Client} <- ListWithoutSender]]),
            %% Aktualizacja inboxów
            UpdatedInboxes = [ update(Name, Client, Time, From, Message) || {Name, Client} <- ListWithoutSender],
            UpdatedClients = maps:put(From, Value, maps:from_list(UpdatedInboxes)),
            %% Wyznaczenie listy wszystkich zarejestrowanych & zalogowanych użytkowników w celu zapisania otrzymanej wiadomości do pliku
            RegisteredAndActiveUsers = [User || {User, Client} <- ListWithoutSender, status(Client) == registered_on],
            case RegisteredAndActiveUsers of
                [] -> ok;
                _ -> save_to_file(RegisteredAndActiveUsers, Time, From, Message)
            end,
            {noreply, State#state{clients = UpdatedClients}};
        _ ->
            To = decode_from_7_bits(CodedTo),
            log(State#state.log_file, "User \"~s\" send message: \"~s\" to: \"~s\"", [From, Message, To]),
            {ok, Client} = maps:find(To, State#state.clients),
            case status(Client) of
                registered_off -> %% aktualizacja skrzynki odbiorczej zarejestrowanych & wylogowanych
                    UpdatedClients = maps:update(To, Client#client{inbox = Client#client.inbox ++ [{Time, From, Message}]}, State#state.clients),
                    {noreply, State#state{clients = UpdatedClients}};
                registered_on -> %% wysłanie wiadomości prywatnych & zapisanie do pliku dla zarejestrowanych & zalogowanych
                    gen_statem:cast(Client#client.address, {message, code_to_7_bits(Time), code_to_7_bits(From), code_to_7_bits(Message)}),
                    save_to_file([To], Time, From, Message),
                    {noreply, State#state{}};
                on -> %% wysłanie wiadomości prywatnych niezarejestrowanych & zalogowanych
                    gen_statem:cast(Client#client.address, {message, code_to_7_bits(Time), code_to_7_bits(From), code_to_7_bits(Message)}),
                    {noreply, State#state{}}    
            end  
    end;
handle_cast(Msg, State) ->
    log(State#state.log_file, "Unrecognized cast request ~w", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    log(State#state.log_file, "Unrecognized info request ~w", [Info]),
    {noreply, State}.


terminate(Reason, State) ->
    log(State#state.log_file, "\"~s\" has been terminated with reason ~w", [State#state.server_name, Reason]),
    net_kernel:stop(),
    ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================

save_to_file(Username, Time, From, Message) ->
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    case Username of
        [OneUser] -> %% musi być w przypadku jednego aktywnego użytkownika w send all
            case dets:member(Table, OneUser) of
                false ->
                    dets:insert(messages, {OneUser, [{Time, From, Message}]});
                true ->
                    save_to_file_when_existed(OneUser, Time, From, Message)
            end; 
        [_OneUser, _Tail] ->
                ListOfUsers = Username,
                NewUsers = [User || User <- ListOfUsers, dets:member(Table, User) == false],
                [dets:insert(messages, {User, [{Time, From, Message}]}) || User <- NewUsers],
                OldUsers = ListOfUsers -- NewUsers,
                [save_to_file_when_existed(User, Time, From, Message) || User <- OldUsers]        
    end,
    dets:close(Table).

save_to_file_when_existed(Username, Time, From, Message) ->
    [{Username, Inbox}] = dets:lookup(messages, Username), 
    UpdatedInbox = Inbox ++ [{Time, From, Message}],
    dets:insert(messages, {Username, UpdatedInbox}).

clear_whole_table() ->
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    dets:delete_all_objects(Table),
    dets:close(Table).

update(Name, Client, Time, From, Message) ->
    case Client#client.address of
        undefined -> {Name, Client#client{inbox = Client#client.inbox ++ [{Time, From, Message}]}};
        _         -> {Name, Client#client{}}
    end.

status(Client) ->
    case Client#client.address of
    undefined ->
        registered_off;
    _ ->
        case Client#client.password of
            undefined ->
                on;
            _ ->
                registered_on
        end
    end.
                
server_node() ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(?NODE_NAME) ++ "@" ++ Host).

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