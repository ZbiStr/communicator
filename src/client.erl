-module(client).
-behaviour(gen_statem).

%%API
-export([stop/1, start_link/1, start/0]).
%%CALLBACK
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

-define(COOKIE, ciasteczko).

% ================================================================================
% API
% ================================================================================
stop(Username) ->
	gen_statem:stop({?MODULE, client_node(list_to_atom(Username))}).

start_link(Username) ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [Username], []).

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
			% print some exit message
			ok;
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
			logout(Choice),
			start2();
		"exit" ->
			% stop()
			% print some exit message
			ok;
		_ -> 
			start2()
	end.

login() -> 
    Prompt = "Put your username: ",
    {ok,[Username]} = io:fread(Prompt, "~s"),
	start_link(Username),
	Result = communicator:login(Username, {?MODULE, client_node(Username)}),
	case Result of
		already_exists ->
			stop(Username),
			io:fwrite("Username already logged on ~n"),
			login();
		ok ->
			io:format("Username: ~s~n", [Username]),
			Username
	end.

logout(Username) ->
	Result = communicator:logout(Username),
	case Result of
		do_not_exist ->
			io:format("This name does not exiist!~n", []),
			Result;
		ok ->
			stop(Username),
			io:format("You have been successfully logged out!~n", []),
			Result
	end.

% ================================================================================
% CALLBACK
% ================================================================================
init([Username]) ->
	net_kernel:start([list_to_atom(Username),shortnames]),
	erlang:set_cookie(local, ?COOKIE),
	{ok, state, []}.

%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
	handle_event_function.

handle_event(enter, _OldState, _State, _Data) ->
	keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

terminate(_Reason, _State, _Data) ->
	net_kernel:stop(),
	ok.

% ================================================================================
% INTERNAL FUNCTIONS
% ================================================================================
help() -> 
    io:format("You can use the commands below:\nLOGIN     Allows you to log in to our app\nLOGOUT    Allows you to log out of our app\n").


client_node(Username) ->
	{ok, Host} = inet:gethostname(),
	list_to_atom(atom_to_list(Username) ++ "@" ++ Host).
