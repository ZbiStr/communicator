-module(communicator).
-behaviour(gen_server).

%% API
-export([stop/0, start_link/0, login/2, logout/1]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(NODE_NAME, server2).
-define(COOKIE, ciasteczko).

-record(state, {clients = #{}}).
-record(client, {address}).

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


% ================================================================================
% CALLBACK
% ================================================================================
init(_Args) ->
    net_kernel:start(?NODE_NAME, #{name_domain => shortnames}),
    erlang:set_cookie(local, ?COOKIE),
    {ok, #state{}}.

handle_call({login, Name, Address}, _From, State) ->     %przeszukiwanie mapy, jeśli nie ma w niej użytkownika Name, 
    case maps:get(Name, State#state.clients, not_found) of      %to go dodaje, jeśli jest, zwraca already_exists
        not_found ->
            UpdatedClients = maps:put(Name, #client{address = Address}, State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}};
        {client, Address} ->
            {reply, already_exists, State#state{}}
    end;
handle_call({logout, Name}, _From, State) ->         %przeszukiwanie mapy, jeśli nie ma w niej użytkownika Name, 
    case maps:get(Name, State#state.clients, not_found) of      %to zwraca does_not_exists, jeśli jest, to go usuwa
        not_found ->
            {reply, does_not_exist, State#state{}};

        {client, _Address} ->
            UpdatedClients = maps:without([Name], State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}}
    end;
handle_call(stop, _From, State) ->
    net_kernel:stop(),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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


