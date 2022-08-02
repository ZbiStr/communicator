-module(communicator).

-behaviour(gen_server).

%% API
-export([stop/1, start_link/1]).
%% CALLBACK
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-record(state, {dummy}).

% ================================================================================
% API
% ================================================================================
stop(Name) ->
	gen_server:call(Name, stop).

start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [], []).

% ================================================================================
% CALLBACK
% ================================================================================
init(_Args) ->
	{ok, #state{dummy=1}}.

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

