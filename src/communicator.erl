-module(communicator).

-behaviour(gen_server).

%% API
-export([stop/0, start_link/0, login/2, logout/1]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-record(state, {clients = #{}}).
-record(client, {pid}).



% ================================================================================
% API
% ================================================================================
stop() ->
	gen_server:call(?SERVER, stop).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

login(Name, PID) ->
    gen_server:call(?SERVER, {login, Name, PID}).

logout(Name) ->
    gen_server:call(?SERVER, {logout, Name}).


% ================================================================================
% CALLBACK
% ================================================================================
init(_Args) ->
	{ok, #state{}}.

handle_call({login, Name, PID}, _From, State = #state{}) ->     %przeszukiwanie mapy, jeśli nie ma w niej użytkownika Name, 
    case maps:get(Name, State#state.clients, not_found) of      %to go dodaje, jeśli jest, zwraca already_exists
        not_found ->
            Fun = fun(V) -> V + 1 end,
            UpdatedClients = maps:update_with(Name, Fun, #client{pid = PID}, State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}};
        {client, PID} ->
            {reply, already_exists, State#state{}}
    end;

handle_call({logout, Name}, _From, State = #state{}) ->         %przeszukiwanie mapy, jeśli nie ma w niej użytkownika Name, 
    case maps:get(Name, State#state.clients, not_found) of      %to zwraca do_not_exists, jeśli jest, to go usuwa
        not_found ->
            {reply, do_not_exist, State#state{}};

        {client, _PID} ->
            UpdatedClients = maps:without([Name], State#state.clients),
            {reply, ok, State#state{clients = UpdatedClients}}
    end;


handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================

    
 

