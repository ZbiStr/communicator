-module(communicator).
-behaviour(gen_server).

%% API
-export([stop/0, start_link/0, logout/1, send_message/3, set_password/2, find_user/1, show_active_users/0, find_password/1, login/3]).
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

send_message(From, To, Message) ->
    gen_server:cast({?SERVER, server_node()},{send_message, From, To, Message}).

set_password(Name, Password) ->
    gen_server:call({?SERVER, server_node()}, {password, Name, Password}).
    

find_user(Name) ->
    gen_server:call({?SERVER, server_node()}, {find_user, Name}).

show_active_users() ->
    gen_server:call({?SERVER, server_node()}, show_active_users).

find_password(Name) ->
    gen_server:call({?SERVER, server_node()},{find_password, Name}).

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
            SetPass = Client#client.password,
            case Password of
                SetPass ->
                    UpdatedClients = maps:update(Name, Client#client{address = Address}, State#state.clients),
                    {reply, correct, State#state{clients = UpdatedClients}};
                _ ->
                    {reply, wrong_password, State#state{}}
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
handle_call(stop, _From, State) ->
    net_kernel:stop(),
    {stop, normal, stopped, State};
handle_call({password, Name, Password}, _From, State) ->        
    Client = maps:get(Name, State#state.clients),
    UpdatedClients = maps:put(Name, Client#client{password = Password}, State#state.clients),
    {reply, ok, State#state{clients = UpdatedClients}};
handle_call({find_password, Name}, _From, State) ->
    {ok, Client} = maps:find(Name, State#state.clients), %szukam po Name 
 	case Client#client.password of     
        undefined ->		%niezdefiniowany logowanie bez hasla, jesli chce przypisujemy pierwsze haslo
            undefined;
        Password ->		%zdefiniowany, logowanie z haslem, jesli chce moze zmienic haslo, zwracam haslo
            Password                              %zwracam haslo, żeby pozniej porownac w cliencie
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    

handle_cast({send_message, From, To, Message}, State) ->  
    case To of 
        all ->
            io:format("~p", [State#state.clients]),
            %% Usuwanie nadawcy z listy użytkowników do których ma trafić wiadomość, 
            %% wysyłanie wiadomości i aktualizacja skrzynek odbiorczych (przypadek zarejestrowanych i nie),
            %% Dodanie nadawcy z powrotem do listy użytkownyków i zwrócenie zaktualizowanej listy.
            {ok, Value} = maps:find(From, State#state.clients),
            ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
            [gen_statem:cast(Client#client.address, {message, From, Message}) || {_Name, Client} <- ListWithoutSender, 
            Client#client.address =/= undefined],
            UpdatedInboxes = [ {Name, Client#client{inbox = Client#client.inbox ++ [{From, Message}]}} || 
            {Name, Client} <- ListWithoutSender, 
            Client#client.address == undefined, 
            Client#client.password =/= undefined],
            UpdatedClients = maps:put(From, Value, maps:from_list(UpdatedInboxes)),
            {noreply, State#state{clients = UpdatedClients}};
        _ ->
            %% wysłanie wiadomości, aktualizacja skrzynki odbiorczej
            %% i zwrócenie zaktualizowanej listy.
            {ok, Client} = maps:find(To, State#state.clients),
            case Client#client.address of
                undefined ->
                    case Client#client.password of
                        undefined ->
                            {noreply, State#state{}};
                        _ ->
                            UpdatedClients = maps:update(To, Client#client{inbox = Client#client.inbox ++ [{From, Message}]}, State#state.clients),
                            {noreply, State#state{clients = UpdatedClients}}
                        end;
                _ ->
                    gen_statem:cast(Client#client.address, {message, From, Message}),
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

server_node() ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(?NODE_NAME) ++ "@" ++ Host).


