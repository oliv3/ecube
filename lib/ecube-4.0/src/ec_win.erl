%%%-------------------------------------------------------------------
%%% File    : ec_win.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Main window
%%%-------------------------------------------------------------------
-module(ec_win).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

%% wx_object API
-export([new/2]).

%% module API
-export([gl/0, set_title/0]).

%% wx_object callbacks
-export([init/1, handle_info/2, handle_event/2, terminate/2]).

-behaviour(wx_object).

%% GUI macros
-define(QUIT,   ?wxID_EXIT).
-define(ABOUT,  ?wxID_ABOUT).

-record(state, {frame, gl}).

-define(SERVER, ?MODULE).


new(Wx, Size) ->
    wx_object:start_link(?MODULE, [Wx, Size], []).


init([Wx, Size]) ->
    process_flag(trap_exit, true),
    {Frame, GL} = wx:batch(fun() -> create_window(Wx, Size) end),
    ?D_REGISTER(?SERVER, self()),
    {Frame, #state{frame=Frame, gl=GL}}.


create_window(Wx, Size) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, title(),
			[{pos, {0, 0}},
			 {size, Size},
			 {style, ?wxDEFAULT_FRAME_STYLE}]),

    Icon = wxIcon:new(ec:priv_path("wxwin.ico")),
    wxFrame:setIcon(Frame, Icon),

    wxFrame:setBackgroundColour(Frame, {0, 0, 0}),
    wxFrame:connect(Frame, close_window),

    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?QUIT, "&Quit"),
    wxMenu:append(Help, ?ABOUT, "&About"), 

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Help, "&Help"),

    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    GL = ec_gl:new(Frame, Size),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addStretchSpacer(Sizer),
    wxSizer:add(Sizer, GL, [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addStretchSpacer(Sizer),
    wxFrame:setSizer(Frame, Sizer),

    wxFrame:show(Frame),

    {Frame, GL}.


handle_info({Pid, Ref, gl}, State) ->
    Pid ! {Ref, State#state.gl},
    {noreply, State};

handle_info({set_title, T}, State) ->
    wxTopLevelWindow:setTitle(State#state.frame, T),
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    ?D_F("process ~p died: ~p, exiting", [_Pid, _Reason]),
    {stop, normal, State}.


handle_event(#wx{event=#wxClose{}}, State) ->
    ec:stop(),
    {noreply, State};

handle_event(#wx{id=?QUIT}, State) ->
    ec:stop(),
    {noreply, State};

handle_event(#wx{id=?ABOUT}, #state{frame=Frame} = State) ->
    Str = "wxBiniou\n"
        "A Biniou/wxErlang demo\n\n"
        "by oliv3 <olivier@biniou.info>",
    MD = wxMessageDialog:new(Frame, Str,
			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			      {caption, "About"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    {noreply, State}.


terminate(_Reason, #state{gl=GL}) ->
    ?D_TERMINATE(_Reason),
    wxGLCanvas:destroy(GL).


gl() ->
    Ref = make_ref(),
    ?SERVER ! {self(), Ref, gl},
    receive
        {Ref, GL} ->
	    GL
    end.


title() ->
    "wxBiniou v" ++ ec:vsn().


set_title() ->
    ?SERVER ! {set_title, title()}.
