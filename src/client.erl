-module(client).
-behaviour(gen_statem).

%%API
-export([stop/0, start_link/0]).
%%CALLBACK
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

% ================================================================================
% API
% ================================================================================
stop() ->
	gen_statem:stop(?MODULE).

start_link() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

% ================================================================================
% CALLBACK
% ================================================================================
init(_Args) ->
	{ok, state, []}.

%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
	handle_event_function.

handle_event(enter, _OldState, _State, _Data) ->
	keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

terminate(_Reason, _State, _Data) ->
	ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================