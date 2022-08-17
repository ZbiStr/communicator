-module(client).
-behaviour(gen_statem).

%% API
-export([start/0]).
%% CALLBACKS
-export([init/1, callback_mode/0, terminate/3, logged_out/3, logged_in/3]).

-define(COOKIE, ciasteczko).

-record(data, {
	username = "" :: string(),
	address  :: atom()
}).


% ================================================================================
% API
% ================================================================================


start() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []),
	greet(),
	help_logged_out(),
	read_commands().


% ================================================================================
% CALLBACKS
% ================================================================================


init([]) ->
	Address = case node() of
        'nonode@nohost' ->
            start_node();
        Node ->
            Node
    end,
	erlang:set_cookie(local, ?COOKIE),
	{ok, logged_out, #data{address = Address}}.

callback_mode() ->
	state_functions.

logged_out({call, From}, {login, Username}, Data) ->
	case login(Username, Data#data.address) of
		already_exists ->
			{keep_state_and_data, {reply, From, ok}};
		ok ->
			help_logged_in(),
			{next_state, logged_in, Data#data{username = Username}, {reply, From, ok}}
	end;
logged_out({call, From}, help, _Data) ->
	help_logged_out(),
	{keep_state_and_data, {reply, From, ok}};
logged_out({call, From}, _, _Data) ->
	handle_unknown(From).

logged_in({call, From}, logout, Data) ->
	case logout(Data#data.username) of
		does_not_exist ->
			% that would make no sense but it can stay in for now
			{keep_state_and_data, {reply, From, ok}};
		ok ->
			help_logged_out(),
			{next_state, logged_out, Data#data{username = ""}, {reply, From, ok}}
	end;
logged_in({call, From}, help, _Data) ->
	help_logged_in(),
	{keep_state_and_data, {reply, From, ok}};
logged_in({call, From}, {message, To, Message}, Data) ->
	case To of 
		all ->
			communicator:send_message(Data#data.username, To, Message);
		_ ->
			io:format("Not an available command.~n")
			%% case communicator:find_user(To) of 						<--- DO PRYWATNYCH
			%%  	does_not_exist ->
			%%			io:format("There is no such user!~n"),
			%%			help_logged_in(),
			%%			parse_logged_in(Username);
			%%		ok ->
			%%			io:format("Private chat with ~p started. Type quit to go back to the main menu~n", [To]),
			%%			chat(Username, To),
			%%			help_logged_in(),
			%%			parse_logged_in(Username)
			%%	end
	end,
	{keep_state_and_data, {reply, From, ok}};
logged_in({call, From}, chat, Data) ->
	{keep_state_and_data, {reply, From, {ok, Data#data.username}}};
logged_in({call, From}, _, _Data) ->
	handle_unknown(From);
logged_in(cast, {message, From, Message}, _Data) ->
	io:format("From ~p: ~p~n", [From, Message]),                                  
    keep_state_and_data.

terminate(_Reason, _State, Data) ->
	case Data#data.username of
		"" ->
			ok;
		Username ->
			logout(Username)
	end,
	net_kernel:stop().


% ================================================================================
% INTERNAL
% ================================================================================


read_commands() ->
	Input = io:get_line(""),
	[Command, Opts] =
		[list_to_atom(string:trim(Token)) || Token <- string:split(Input ++ " ", " ")],
	case Command of
		exit ->
			gen_statem:stop(?MODULE),
			exit(normal);
		chat ->
			case gen_statem:call(?MODULE, chat) of
				{ok, Username} ->
					To = Opts,
					case To of 
						all ->
							io:format("Chat with all users started. Type quit to go back to the main menu~n"),
							chat(Username, all);
						_ ->
							io:format("Not an available command.~n")
							%% case communicator:find_user(To) of 						<--- DO PRYWATNYCH
							%%  	does_not_exist ->
							%%			io:format("There is no such user!~n"),
							%%			help_logged_in(),
							%%			parse_logged_in(Username);
							%%		ok ->
							%%			io:format("Private chat with ~p started. Type quit to go back to the main menu~n", [To]),
							%%			chat(Username, To),
							%%			help_logged_in(),
							%%			parse_logged_in(Username)
							%%	end
					end;
				_ ->
					ok
			end;
		message ->
			To = Opts,
			Message = string:trim(io:get_line("> "), trailing, [$\n]),
			gen_statem:call(?MODULE, {message, To, Message});
		login ->
			{ok, [Username]} = io:fread("Please input your username: ", "~s"),
			gen_statem:call(?MODULE, {login, Username});
		_ ->
			gen_statem:call(?MODULE, Command)			
	end,
	read_commands().

login(Username, Address) -> 
	Login = communicator:login(Username, {?MODULE, Address}),
	case Login of
		already_exists ->
			% makes no sense in this implementation
			io:format("Username already logged on~n");
		ok ->
			io:format("Succesfully logged in as: ~s~n", [Username])
	end,
	Login.

logout(Username) ->
	Logout = communicator:logout(Username),
	case Logout of
		does_not_exist ->
			io:format("This username doesn't exist~n");
		ok ->
			io:format("You have been successfully logged out~n")
	end,
	Logout.

chat(From, To) ->
	Message = string:trim(io:get_line("> "), trailing, [$\n]),
	case Message of
		"quit" ->
			{ok, [Y_N]} = io:fread("Do you want to quit? (y/n) ", "~a"),
			%% ten case jest na wypadek gdyby ktoś chciał wysłać komuś słowo "quit",
			%% a jakoś trzeba dać możliwość wyjścia z czatu.
			case Y_N of
				y ->
					io:format("Chat ended.~n");
				n -> 
					communicator:send_message(From, To, Message),
					chat(From, To)
				end;
		_ ->
			communicator:send_message(From, To, Message),
			chat(From, To)
	end.

handle_unknown(From) ->
	io:format("Not a viable command~n"),
	{keep_state_and_data, {reply, From, unknown}}.

greet() ->
	io:format("
////////////////////////////////////////////
/////    Glad to see you in our app!   /////
////////////////////////////////////////////~n").

help_logged_out() ->
	io:format("You can use the following commands:
login     to log in to the server
help      to view this again
exit      to exit the app~n").

help_logged_in() ->
	io:format("You can use the following commands:
logout			to log out from the server
message all		to send a message to all users
chat all		to chat with all users
help			to view this again
exit			to exit the app~n").
%% DO PRYWATNYCH: message Username	to start private chat with user named Username

start_node() ->
	% random lowercase letters
	Name = [96 + rand:uniform(26) || _ <- lists:seq(1,9)],
	try net_kernel:start(list_to_atom(Name), #{name_domain => shortnames}) of
		{ok, _} ->
			{ok, Host} = inet:gethostname(),
			list_to_atom(Name ++ "@" ++ Host)
	catch
		error:{already_started, _Pid} ->
			start_node()
	end.