-module(login).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3, stop/0, get_login/0]).

-include_lib("wx/include/wx.hrl").
-define(SERVER, ?MODULE).

-record(state, {log, pass, subscribe_click}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Erlangpol Communicator", [{size, {310,440}}]),
    Panel = wxPanel:new(Frame, []),
    wxFrame:createStatusBar(Frame,[]),
    % wxFrame:connect(Frame, close_window),

    Image = wxImage:new("C:/Users/ecimkat/sprint2/communicator/erl.png", []),
    Bitmap = wxBitmap:new(wxImage:scale(Image,
            round(wxImage:getWidth(Image)*0.5),
            round(wxImage:getHeight(Image)*0.5),
            [{quality, ?wxIMAGE_QUALITY_HIGH}])),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "Login"}]),
    Sizer2 = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				  [{label, "Password"}]),

    TextCtrl  = wxTextCtrl:new(Panel, 2, [{value, ""}, 
					 {style, ?wxDEFAULT}]),
    TextCtrl2 = wxTextCtrl:new(Panel, 3, [{value, ""}, {size, {310, 25}},
					  {style, ?wxDEFAULT bor ?wxTE_PASSWORD}]),
    Button = wxButton:new(Panel, 4, [{label,"Sign up!"}]),
    wxButton:setToolTip(Button, "Click here to sign up!"),
    wxSizer:add(MainSizer, StaticBitmap, []),
    wxSizer:add(Sizer, TextCtrl,  [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, TextCtrl2, []),
    wxSizer:add(MainSizer, Sizer,  [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10), 
    wxSizer:add(MainSizer, Button, [{flag, ?wxCENTER}]),  
    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:setSizeHints(MainSizer, Panel),
    wxButton:connect(Button, command_button_clicked),
    
    MenuBar  = wxMenuBar:new(),
    FileM    = wxMenu:new([]),
    HelpM    = wxMenu:new([]),
    LangM    = wxMenu:new([]),
    % unlike wxwidgets the stock menu items still need text to be given, 
    % although help text does appear
    _QuitMenuItem  = wxMenu:append(FileM, ?wxID_EXIT, "&Quit"),
    % Note the keyboard accelerator
    _AboutMenuItem = wxMenu:append(HelpM, ?wxID_ABOUT, "&About...\tF1"),
    wxMenu:appendRadioItem(LangM, 1, "EN"), 
    wxMenu:appendRadioItem(LangM, 2, "PL"), 

    wxMenu:appendSeparator(HelpM),    
    ContentsMenuItem = wxMenu:append(HelpM, ?wxID_HELP_CONTENTS, "&Contents"),
    wxMenuItem:enable(ContentsMenuItem, [{enable, false}]),
    ok = wxFrame:connect(Frame, command_menu_selected), 

    wxMenuBar:append(MenuBar, FileM, "&Menu"),
    wxMenuBar:append(MenuBar, HelpM, "&Help"),
    wxMenuBar:append(MenuBar, LangM, "&Language"),
    wxFrame:setMenuBar(Frame, MenuBar),
    ok = wxFrame:setStatusText(Frame, "Welcome to Erlangpol Communicator!", []),
    
    wxFrame:show(Frame),
    % wxFrame:refresh(Frame),
    {ok, #state{log = TextCtrl, pass = TextCtrl2}}.

get_login() ->
    gen_server:cast(?MODULE, {subscribe_click, self()}),
    receive
        button_clicked ->
            gen_server:call(?MODULE, get_login)
    end.

handle_call(get_login, _, State) ->
    Rep = {State#state.log, State#state.pass},
    {reply, Rep, State};
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({subscribe_click, Pid}, State) ->
    {noreply, State#state{subscribe_click=Pid}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_event(#wx{event=#wxClose{}}, #state{log = TextCtrl, pass = TextCtrl2} = State) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText("Closing...",[]),
    {stop, normal, State}.

handle_info(#wx{event = #wxCommand{type = command_button_clicked}}, #state{log = TextCtrl, pass = TextCtrl2, subscribe_click = Pid} = State) ->
    LabelLogin =  wxTextCtrl:getValue(TextCtrl),
    LabelPassword =  wxTextCtrl:getValue(TextCtrl2),
    io:format("Login: ~p~n", [LabelLogin]),
    io:format("Password: ~p~n", [LabelPassword]),
    % _Label = "Welcome" ++ LabelLogin + "!",
    %stop(),
    Pid ! button_clicked,
    Reply = State#state{log = LabelLogin, pass = LabelPassword},
   %{stop, normal, Reply}.
   {noreply, Reply}.

terminate(normal, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:cast(?MODULE, stop).   

