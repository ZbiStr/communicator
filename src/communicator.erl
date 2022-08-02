-module(communicator).

-behaviour(gen_server).

%% API
-export([stop/0, start_link/0, login/2, logout/1]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-record(state, {clients = []}).
-record(client, {name, pid}).



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

handle_call({login, Name, PID}, _From, #state{clients = Clients} = State) ->
    CheckIfExists = [X || X <- State#state.clients, X#client.name == Name],


    case CheckIfExists of
        [] ->
            UpdatedClients = Clients ++ [#client{name = Name, pid = PID}],
            {reply, ok, State#state{clients = UpdatedClients}};

        [{client, Name, _}] ->
            {reply, already_exists, State}
    end;


handle_call({logout, Name}, _From, #state{clients = Clients} = State) ->
    CheckIfExists = [X || X <- State#state.clients, X#client.name == Name],

    case CheckIfExists of
        [] ->
            {reply, do_not_exist, State};

        [{client, Name, PID}] ->
            UpdatedClients = Clients -- [#client{name = Name, pid = PID}],
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

