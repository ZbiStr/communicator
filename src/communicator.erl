-module(communicator).
-behaviour(gen_server).

%% API
-export([stop/0, start_link/0, login/2, logout/1, send_message/3, find_user/1]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(NODE_NAME, erlangpol).
-define(COOKIE, ciasteczko).

-record(state, {clients = #{}}).
-record(client, {address, inbox=[]}).

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

login(Name, Address) ->
    gen_server:call({?SERVER, server_node()}, {login, Name, Address}).

logout(Name) ->
    gen_server:call({?SERVER, server_node()}, {logout, Name}).

send_message(From, To, Message) ->
    gen_server:cast({?SERVER, server_node()},{send_message, From, To, Message}).

find_user(Name) ->
    gen_server:call({?SERVER, server_node()}, {find_user, Name}).

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

handle_call({login, Name, Address}, _From, State) ->
    %% Przeszukiwanie mapy, zwrócenie already_exists w przypadku gdy użytkownik Name 
    %% już w niej występuje, dodanie go w przeciwnym przypadku.
    case maps:get(Name, State#state.clients, not_found) of   
        not_found ->
            UpdatedClients = maps:put(Name, #client{address = Address}, State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}};
        {client,_OtherAddress,_Inbox} ->
            {reply, already_exists, State#state{}}
    end;
handle_call({logout, Name}, _From, State) ->         
    %% Przeszukiwanie mapy, zwrócenie does_not_exists w przypadku gdy użytkownik Name 
    %% w niej nie występuje, usunięcie go w przeciwnym przypadku.
    case maps:get(Name, State#state.clients, not_found) of 
        not_found ->
            {reply, does_not_exist, State#state{}};
        {client,_Address,_Inbox} ->
            UpdatedClients = maps:without([Name], State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}}
    end;
handle_call({find_user, Name}, _From, State) ->         
    case maps:get(Name, State#state.clients, not_found) of 
        not_found ->
            {reply, does_not_exist, State#state{}};
        {client,_Address,_Inbox} ->
            {reply, ok, State#state{}}
    end;
handle_call(stop, _From, State) ->
    net_kernel:stop(),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_message, From, To, Message}, State) ->  
    case To of 
        all ->
            %% Usuwanie nadawcy z listy użytkowników do których ma trafić wiadomość, 
            %% wysyłanie wiadomości i aktualizacja skrzynek odbiorczych.
            %% Dodanie nadawcy z powrotem do listy użytkownyków i zwrócenie zaktualizowanej listy.
            {ok, Value} = maps:find(From, State#state.clients),
            ListWithoutSender = maps:to_list(maps:without([From], State#state.clients)), 
            [gen_statem:cast(Address, {message, From, Message}) || {_Name, {client, Address, _Inbox}} <- ListWithoutSender],   
            UpdatedInboxes = [ {Name, {client, Address, Inbox ++ [{From, Message}]}} || {Name, {client, Address, Inbox}} <- ListWithoutSender], 
            UpdatedClients = maps:put(From, Value, maps:from_list(UpdatedInboxes)),  
            {noreply, State#state{clients = UpdatedClients}}; 
        _ ->
            %% wysłanie wiadomości, aktualizacja skrzynki odbiorczej
            %% i zwrócenie zaktualizowanej listy.
            {ok, {client, Address, Inbox}} = maps:find(To, State#state.clients),  
            gen_statem:cast(Address, {message, From, Message}),
            UpdatedInbox = Inbox ++ [{From, Message}],
            UpdatedClients = maps:update(To, {client, Address, UpdatedInbox}, State#state.clients),
            {noreply, State#state{clients = UpdatedClients}}             
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


