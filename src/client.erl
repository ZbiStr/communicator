-module(client).
-behaviour(gen_statem).

%%API
-export([stop/0, start_link/0, start/0, start2/0]).
%%CALLBACK
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

% ================================================================================
% API
% ================================================================================
stop() ->
	gen_statem:stop(?MODULE).

start_link() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
%     io:format("\nchoose one command below:\n"),
    %     io:format("
    % ////////////////////////////////////////////
    % /////    Glad to see you in our app!   /////
    % ////////////////////////////////////////////"),
    %     io:format("
    % //       Choose one command below:        //
    % // help                                   //
    % // login                                  //
    % // exit                                   //
    % ////////////////////////////////////////////\n"),
    io:format("\nchoose one command below:\n"),
    Input = io:get_line(""),
    Choice = lists:droplast(Input),
	case Choice of
		"help" ->
			help(),
			start();
		"login" ->
			login(),
			start2();
		"exit" ->
			stop();
		_ ->
			start()
	end.
	
start2() ->
    Input = io:get_line(""),
    Choice = lists:droplast(Input),
	case Choice of
		"help" ->
			help(),
			start2();
		"logout" ->
			logout(),
			start2();
		"exit" ->
			stop();
		_ -> 
			start()
	end.
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

help() -> 
    io:format("You can use the commands below:\nLOGIN     Allows you to log in to our app\nLOGOUT    Allows you to log out of our app\n").

login() -> 
    % io:format("ok\n").
    Prompt = "Put your username: ",
    {ok,[Name]} = io:fread(Prompt, "~s"),
    io:format("Username: ~s~n", [Name]).

logout() -> 
    io:format("Log out successfully\n").