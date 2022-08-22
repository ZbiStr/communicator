-module(communicator).
-behaviour(gen_server).

%% API
-export([show_inbox_user/1, stop/0, start_link/0, logout/1, send_message/4, set_password/2, find_user/1, save_to_file/4, show_active_users/0, find_password/1, login/3, user_history/1, clear_whole_table/0, show_table/0]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(NODE_NAME, erlangpol).
-define(COOKIE, ciasteczko).

-record(state, {clients = #{}}).
-record(client, {address = undefined, inbox=[], password = undefined}).

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
    gen_server:call({?SERVER, server_node()}, {login, Name, Address, Password}).

logout(Name) ->
    gen_server:call({?SERVER, server_node()}, {logout, Name}).

send_message(To, Time, From, Message) ->
    gen_server:cast({?SERVER, server_node()},{send_message, To, Time, From, Message}).

set_password(Name, Password) ->
    gen_server:call({?SERVER, server_node()}, {password, Name, Password}).
    
find_user(Name) ->
    gen_server:call({?SERVER, server_node()}, {find_user, Name}).

show_active_users() ->
    gen_server:call({?SERVER, server_node()}, show_active_users).

find_password(Name) ->
    gen_server:call({?SERVER, server_node()},{find_password, Name}).

user_history(Username) ->
    gen_server:call({?SERVER, server_node()},{history, Username}).



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
    {ok, #state{}}.

handle_call({login, Name, Address, Password}, _From, State) ->
    %% Przeszukiwanie mapy, zwrócenie already_exists w przypadku gdy użytkownik Name 
    %% już w niej występuje, dodanie go w przeciwnym przypadku.
    Client = maps:get(Name, State#state.clients, not_found),
    case Client of   
        not_found ->
            UpdatedClients = maps:put(Name, #client{address = Address}, State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}};
        _ ->      
            case Client#client.address of
                undefined -> 
                    SetPass = Client#client.password,
                    case Password of
                        SetPass ->
                            UpdatedClients = maps:update(Name, Client#client{address = Address}, State#state.clients),
                            {reply, ok, State#state{clients = UpdatedClients}};
                        _ ->
                            {reply, wrong_password, State#state{}}
                    end;
                _ ->
                    {reply, already_exists, State}
            end
    end;
handle_call({logout, Name}, _From, State) ->         
    %% Przeszukiwanie mapy, zwrócenie does_not_exists w przypadku gdy użytkownik Name 
    %% w niej nie występuje, usunięcie go w przeciwnym przypadku.
    {ok, Client} = maps:find(Name, State#state.clients),
    case Client#client.password of 
        undefined ->
            UpdatedClients = maps:without([Name], State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}};
        _Password ->
            UpdatedClients = maps:update(Name, Client#client{address = undefined}, State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}}
    end;
handle_call({find_user, Name}, _From, State) ->  
    case maps:get(Name, State#state.clients, not_found) of 
        not_found ->
            {reply, does_not_exist, State#state{}};
        _Client ->
            {reply, ok, State#state{}}
    end;
handle_call(show_active_users, _From, State) ->  
    ListOfUsers = maps:to_list(State#state.clients), 
    ActiveUsers = [ Name || {Name, Client} <- ListOfUsers, Client#client.address =/= undefined ],
    {reply, ActiveUsers, State#state{}};
handle_call({password, Name, Password}, _From, State) ->        
    Client = maps:get(Name, State#state.clients),
    UpdatedClients = maps:put(Name, Client#client{password = Password}, State#state.clients),
    {reply, ok, State#state{clients = UpdatedClients}};
handle_call({find_password, Name}, _From, State) ->
Client = maps:get(Name, State#state.clients, not_found), %szukam po Name
     case Client of
        not_found ->
            {reply, undefined, State};
        _ ->
            {reply, Client#client.password, State}
        end;
handle_call({history, Username}, _From, State) ->
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    case dets:lookup(messages, Username) of
        [{Username, History}] ->
            dets:close(Table),
            {reply, History, State#state{}};
        [] -> 
            dets:close(Table),
            {reply, [], State#state{}}
    end;
handle_call(stop, _From, State) ->
    net_kernel:stop(),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    

handle_cast({send_message, To, Time, From, Message}, State) ->  
    case To of 
        all ->
            {ok, Value} = maps:find(From, State#state.clients),
            %% Wysyłanie wiadomości do wszystkich aktywnych osób poza nadawcą
            ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
            [gen_statem:cast(Client#client.address, {message, Time, From, Message}) || {_Name, Client} <- ListWithoutSender, 
            Client#client.address =/= undefined],
            %% Aktualizacja inboxów
            UpdatedInboxes = [ update(Name, Client, From, Message) || {Name, Client} <- ListWithoutSender],
            UpdatedClients = maps:put(From, Value, maps:from_list(UpdatedInboxes)),
            %% Wyznaczenie listy wszystkich zarejestrowanych użytkowników w celu zapisania otrzymanej wiadomości do pliku
            RegisteredAndActiveUsers = [User || {User, Client} <- ListWithoutSender, status(Client) == registered_on],
            case RegisteredAndActiveUsers of
                [] -> ok;
                _ -> save_to_file(RegisteredAndActiveUsers, Time, From, Message)
            end,
            {noreply, State#state{clients = UpdatedClients}};
        _ ->
            {ok, Client} = maps:find(To, State#state.clients),
            case status(Client) of
                registered_off -> %% aktualizacja skrzynki odbiorczej zarejestrowanych & wylogowanych
                    UpdatedClients = maps:update(To, Client#client{inbox = Client#client.inbox ++ [{Time, From, Message}]}, State#state.clients),
                    {noreply, State#state{clients = UpdatedClients}};
                registered_on -> %% wysłanie wiadomości prywatnych & zapisanie do pliku
                    gen_statem:cast(Client#client.address, {message, Time, From, Message}),
                    save_to_file(To, Time, From, Message),
                    {noreply, State#state{}};
                on -> %% wysłanie wiadomości prywatnych
                    gen_statem:cast(Client#client.address, {message, Time, From, Message}),
                    {noreply, State#state{}}    
            end  
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
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
                [save_to_file_when_existed(User, Time, From, Message) || User <- OldUsers];
         _ ->
                case dets:member(Table, Username) of
                    false ->
                        dets:insert(messages, {Username, [{Time, From, Message}]});
                    true ->
                        save_to_file_when_existed(Username, Time, From, Message)
                end              
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

show_table() ->
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    ets:new(messages_ets, [named_table, set]),         
    dets:to_ets(messages, messages_ets),               
    LIST = ets:tab2list(messages_ets),
    MAP = maps:from_list(LIST),
    io:format("~p~n", [MAP]),
    ets:delete(messages_ets),
    dets:close(Table).

show_inbox_user(User) ->
    {ok, Table} = dets:open_file(messages, [{file, "messages"}, {type, set}]),
    [{User, Inbox}] = dets:lookup(messages, User), 
    io:format("User: ~p, Inbox: ~p~n", [User, Inbox]),
    dets:close(Table).

update(Name, Client, From, Message) ->
    case Client#client.address of
        undefined -> {Name, Client#client{inbox = Client#client.inbox ++ [{From, Message}]}};
        _ -> {Name, Client#client{}}
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


