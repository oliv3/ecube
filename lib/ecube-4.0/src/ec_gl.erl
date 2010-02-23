-module(ec_gl).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

-behaviour(wx_object).

%% wx_object API
-export([new/2]).

%% wx_object callbacks
-export([init/1, handle_event/2, terminate/2]).

-define(SERVER, ?MODULE).

%% GL widget state
-record(state, {frame, gl, mouse}).


new(Frame, Size) ->
    wx_object:start_link(?MODULE, [Frame, Size], []).


init([Frame, Size]) ->
    Opts = [{size, Size}],
    GLAttrib = [{attribList, [?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_DEPTH_SIZE, 16,
			      0]}],
    GL = wxGLCanvas:new(Frame, Opts ++ GLAttrib),

    wxFrame:connect(GL, left_down),
    wxFrame:connect(GL, mousewheel),
    wxFrame:connect(GL, motion),
    wxFrame:connect(GL, enter_window),
    wxFrame:connect(GL, key_up),

    ?D_REGISTER(?SERVER, self()), %% not needed

    {GL, #state{frame=Frame, gl=GL}}.


handle_event(#wx{event=#wxMouse{type=left_down, x=X, y=Y}}, State) ->
    {noreply, State#state{mouse={X, Y}}};

handle_event(#wx{event=#wxMouse{type=motion, leftDown=true, x=X, y=Y}}, State) ->
    {OldX, OldY} = State#state.mouse,
    DX = X - OldX,
    DY = Y - OldY,
    {RX, RY, RZ} = ec_cf:rot(),
    NRX = trunc(RX+DY+360) rem 360,
    NRY = trunc(RY+DX+360) rem 360,
    ec_cf:rot({NRX, NRY, RZ}),
    {noreply, State#state{mouse={X, Y}}};

handle_event(#wx{event=#wxMouse{type=motion}}, State) ->
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=mousewheel, wheelRotation=R}}, State) when R < 0 ->
    ec_cf:inc_fov(),
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=mousewheel}}, State) ->
    ec_cf:dec_fov(),
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=enter_window}}, State) ->
    wxFrame:setFocus(State#state.gl),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=?O_FS}}, State) ->
    Frame = State#state.frame,
    ec_cf:toggle(?O_FS),
    New = ec_cf:opt(?O_FS),
    wxTopLevelWindow:showFullScreen(Frame, New),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$P}}, State) ->
    ec_cf:toggle(?O_PLANE),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$A}}, State) ->
    ec_cf:toggle(?O_AXES),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$S}}, State) ->
    ec_cf:toggle(?O_SPIN),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$Z}}, State) ->
    ec_cf:no_spin(),
    ec_cf:reset_rot(),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$E}}, State) ->
    ec_cf:toggle(?O_EDGES),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$T}}, State) ->
    ec_cf:toggle(?O_TEXT),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$M}}, State) ->
    ec_cf:toggle(?O_MUTE),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$O}}, State) ->
    ec_cf:toggle(?O_OSD),
    {noreply, State};

handle_event(#wx{event=#wxKey{}}, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ?D_TERMINATE(_Reason).
