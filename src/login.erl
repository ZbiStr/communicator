-module(login).
-include_lib("wx/include/wx.hrl").

-export([start/0]).

start() ->
    Wx = wx:new(),
    Frame = wx:batch(fun() -> create_window(Wx) end),
    wxWindow:show(Frame),
    loop(Frame),
    wx:destroy(),
    ok.

create_window(Wx) ->
    Frame = wxFrame:new(Wx, -1, "Erlangpol Communicator", [{size, {310,440}}]),
    Panel = wxPanel:new(Frame, []),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    Image = wxImage:new("C:/Users/ecimkat/sprint2/communicator/src/erl.png", []),
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
    FrameNew = wxFrame:new(Wx, -1, "Erlangpol Communicator", [{size, {310,440}}]),
    wxButton:connect(Button, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{login => TextCtrl, password => TextCtrl2, oldWindow => Frame, newWindow => FrameNew, env => wx:get_env()}}]),

    wxSizer:add(MainSizer, StaticBitmap, []),
    wxSizer:add(Sizer, TextCtrl,  [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, TextCtrl2, []),
    wxSizer:add(MainSizer, Sizer,  [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10), 
    wxSizer:add(MainSizer, Button, [{flag, ?wxCENTER}]),  
    wxPanel:setSizer(Panel, MainSizer),

    MenuBar  = wxMenuBar:new(),
    FileM    = wxMenu:new([]),
    HelpM    = wxMenu:new([]),

    % unlike wxwidgets the stock menu items still need text to be given, 
    % although help text does appear
    _QuitMenuItem  = wxMenu:append(FileM, ?wxID_EXIT, "&Quit"),
    % Note the keyboard accelerator
    _AboutMenuItem = wxMenu:append(HelpM, ?wxID_ABOUT, "&About...\tF1"),

    wxMenu:appendSeparator(HelpM),    
    ContentsMenuItem = wxMenu:append(HelpM, ?wxID_HELP_CONTENTS, "&Contents"),
    wxMenuItem:enable(ContentsMenuItem, [{enable, false}]),

    ok = wxFrame:connect(Frame, command_menu_selected), 

    wxMenuBar:append(MenuBar, FileM, "&Menu"),
    wxMenuBar:append(MenuBar, HelpM, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),

    ok = wxFrame:setStatusText(Frame, "Welcome to Erlangpol Communicator!",[]),
    Frame.

handle_click(#wx{obj = _Button, userData = #{login := TextCtrl, password := TextCtrl2, oldWindow := Frame, newWindow := FrameNew, env := Env}}, _Event) ->
    wx:set_env(Env),
    LabelLogin =  wxTextCtrl:getValue(TextCtrl),
    LabelPassword =  wxTextCtrl:getValue(TextCtrl2),
    io:format("~p~n", [LabelLogin]),
    io:format("~p~n", [LabelPassword]),
    wxWindow:destroy(Frame),
    wxFrame:show(FrameNew).

loop(Frame) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxFrame:destroy(Frame),
  	    ok;
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:destroy(Frame),
	    ok;
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
	    io:format("Got about ~n", []),
	    dialog(?wxID_ABOUT, Frame),
	    loop(Frame);
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(Frame)
    after 1000 ->
	loop(Frame)
    end.

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


