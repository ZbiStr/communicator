-module(login).
-behaviour(wx_object).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2, 
	 terminate/2, code_change/3, stop/0, get_login/0]).

-include_lib("wx/include/wx.hrl").
-define(SERVER, ?MODULE).
-define(EN, 20000).
-define(PL, 20001).
-define(PRZYCISK, 20002).
-record(state, {log, pass, win, status, subscribe_click}).

start_link() ->
    wx_object:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    Button = wxButton:new(Panel, ?PRZYCISK, [{label,"Sign up!"}]),
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
    wxMenu:appendRadioItem(LangM, ?EN, "EN"), 
    wxMenu:appendRadioItem(LangM, ?PL, "PL"), 

    wxMenu:appendSeparator(HelpM),    
    ContentsMenuItem = wxMenu:append(HelpM, ?wxID_HELP_CONTENTS, "&Contents"),
    wxMenuItem:enable(ContentsMenuItem, [{enable, false}]),
    wxFrame:connect(Frame, command_menu_selected, [{skip, true}]),

    wxMenuBar:append(MenuBar, FileM, "&Menu"),
    wxMenuBar:append(MenuBar, HelpM, "&Help"),
    % wxMenuBar:append(MenuBar, LangM, "&Language"),
    wxFrame:setMenuBar(Frame, MenuBar),

    wxFrame:show(Frame),
    wxFrame:refresh(Frame),
    {Frame, #state{log = TextCtrl, pass = TextCtrl2, win = Frame}}.

get_login() ->
    gen_server:cast(?MODULE, {subscribe_click, self()}),
    receive
        button_clicked ->
            gen_server:call(?MODULE, get_login)
    end.

handle_call(get_login, _, State) ->
    % case State#state.pass of 
    %     [] ->
    %         {reply, {State#state.log, undefined}, State};
    %     _ ->
    %         {reply, {State#state.log, State#state.pass}, State}
    % end;
    Rep = {State#state.log, State#state.pass},
    {reply, Rep, State};
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({subscribe_click, Pid}, State) ->
    {noreply, State#state{subscribe_click=Pid}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};

handle_event(#wx{id = Id,
		 event = #wxCommand{type = command_menu_selected}},
	     State = #state{}) ->
    case Id of
            ?wxID_EXIT ->
                {stop, normal, State};
            ?wxID_ABOUT ->
                dialog(?wxID_ABOUT, State#state.win),
                {noreply, State};
            % ?EN ->
            %     wxFrame:setStatusText(State#state.win, "ELOOOO", []),
            %     {noreply, State};
            % ?PL ->
            %     wxFrame:setStatusText(State#state.win, "SIEMAAA", []),
            %     {noreply, State};
            _ -> 
                io:format("inne"),
                {noreply, State}
    end;

handle_event(#wx{id = ?PRZYCISK, event = #wxCommand{type = command_button_clicked}}, State = #state{log = TextCtrl, pass = TextCtrl2, win = Frame, subscribe_click = Pid}) ->
    LabelLogin =  wxTextCtrl:getValue(TextCtrl),
    LabelPassword =  wxTextCtrl:getValue(TextCtrl2),
    % io:format("Login: ~p~n", [LabelLogin]),
    % io:format("Password: ~p~n", [LabelPassword]),
    NewState = case LabelLogin of
        [] ->
            Status = "Please enter your login.",
            State;
        _ ->
            case LabelPassword of
                [] ->
                    % Status = "Please enter your password.",
                    % State;
                    Pid ! button_clicked,
                    Status = "Welcome " ++ LabelLogin ++ "! Connected to server.",
                    State#state{log = LabelLogin, pass = undefined};
                _ ->
                    % communicator:login(LabelLogin, address, undefined),
                    % communicator:set_password(LabelLogin, LabelPassword),
                    % communicator:logout(LabelLogin),
                    Pid ! button_clicked,
                    Status = "Welcome " ++ LabelLogin ++ "! Connected to server.",
                    State#state{log = LabelLogin, pass = LabelPassword}
            end 
    end,
    wxFrame:setStatusText(Frame, Status, []),
    {noreply, NewState}.

handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State}.

terminate(_Reason, _State = #state{win=Frame}) ->
    wxFrame:destroy(Frame),
    wx:destroy().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:cast(?MODULE, stop).   

dialog(?wxID_ABOUT,  Frame) ->
    Str = string:join(["Welcome to Erlangpol Communicator. ", 
		       "This is the progress of the work of the two months sprint. ",
		       "Running under ",
		       wx_misc:getOsDescription(),
		       "."], 
		      ""),
    MD = wxMessageDialog:new(Frame,
   			     Str,
   			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
   			      {caption, "About project"}]),

    wxDialog:showModal(MD),
    wxDialog:destroy(MD).