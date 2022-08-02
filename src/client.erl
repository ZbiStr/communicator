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

logout(Name) ->
	gen_statem:call(?MODULE, {logout, Name}).

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

handle_event({logout, Name}, _OldState, _State, _Data) ->
	case communicator:logout(Name) of
		do_not_exist ->
			io:format("This name does not exiist!~n", []),
			keep_state_and_data;
		ok ->
			io:format("You have been successfully logged out!~n", [])
			%zmiana stanu na wylogowany

	%is_logged_in = false 
	end;

handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

terminate(_Reason, _State, _Data) ->
	ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================

