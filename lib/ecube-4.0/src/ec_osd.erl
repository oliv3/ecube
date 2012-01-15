%%%-------------------------------------------------------------------
%%% File    : ec_osd.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : On Screen Display plugin
%%%-------------------------------------------------------------------
-module(ec_osd).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

%% API
-export([start_link/0]).
-export([message/1, message/2]).
-export([test/0]).

%% Callbacks
-export([init/1, system_continue/3]).

-define(SERVER, ?MODULE).

-define(OSD_MSG_DELAY, 3000).
-define(OSD_MSG_PTSIZE, 50).
-define(PTSIZE, 10).
-define(ON,     {0, 255, 127}).

-record(state, {texth, labels, msg, msg_font}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

message(Msg) when is_list(Msg) ->
    message(Msg, ?OSD_MSG_DELAY).
message(Msg, Delay) when is_list(Msg), is_integer(Delay) ->
    ?SERVER ! {message, Msg, Delay}.

%%====================================================================
%% Callbacks
%%====================================================================
init(Parent) ->
    %% Delete lists on terminate + register_lists in GUI
    %% process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    
    Self = self(),
    ?D_REGISTER(?SERVER, Self), %% not needed
    ec_gui:register(Self),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, Self}),
    loop(Parent, Debug, #state{}).


%% Initialization step, kinda like gen_server:init/1
loop(Parent, Debug, #state{labels=undefined, msg_font=undefined} = State) ->
    receive
	{Pid, Ref, {draw, GL}} ->
	    wxGLCanvas:setCurrent(GL),
	    Fixed = wxFont:new(?PTSIZE, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL,
			       ?wxFONTWEIGHT_NORMAL),
	    {ok, GLFixed} = wx_glfont:load_font(Fixed, []),
	    TextH = float(wx_glfont:height(GLFixed)),
	    Labels = build_labels(GLFixed),

	    MsgFixed = wxFont:new(?OSD_MSG_PTSIZE, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL,
				  ?wxFONTWEIGHT_NORMAL),
	    {ok, MsgGLFixed} = wx_glfont:load_font(MsgFixed, []),

	    NewState = State#state{texth=TextH, labels=Labels, msg_font=MsgGLFixed},
	    Pid ! {Ref, osd(GL, NewState)},
	    loop(Parent, Debug, NewState);
	
	{system, From, Request} ->
	    ?D_F("code v1 system message(1): From ~p Request: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
	
	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end;
%% Main loop
loop(Parent, Debug, State) ->
    receive
	{remove, Ref} ->
	    case State#state.msg of
		{Ref, _Msg} ->
		    loop(Parent, Debug, State#state{msg=undefined});
		_Other ->
		    loop(Parent, Debug, State)
	    end;

	{message, Msg, Delay} ->
	    Ref = make_ref(),
	    erlang:send_after(Delay, ?MODULE, {remove, Ref}),
	    loop(Parent, Debug, State#state{msg={Ref,Msg}});

	{Pid, Ref, {draw, GL}} ->
	    Pid ! {Ref, osd(GL, State)},
	    loop(Parent, Debug, State);

	{'EXIT', _Pid, Reason} ->
	    ?D_F("got EXIT from ~p with reason: ~p", [_Pid, Reason]),
	    exit(Reason);

 	{system, From, Request} ->
	    ?D_F("code v1 system message(2): From ~p Request: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end.


system_continue(Parent, Debug, State) ->
    ?D_F("code v1 system_continue(~p, ~p, ~p)", [Parent, Debug, State]),
    loop(Parent, Debug, State).


osd(GL, State) ->
    osd(ec_cf:opt(?O_OSD), GL, State).

osd(false, _GL, _State) ->
    ok;
osd(true, GL, #state{texth=TextH, labels=Labels, msg=Msg, msg_font=MsgFont}) ->
    wxGLCanvas:setCurrent(GL),

    %% TODO: display (centered, yellow, big) OSD message

    gl:enable(?GL_COLOR_MATERIAL), %% necessary ?

    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_TEXTURE_2D),

    {W, H} = wxWindow:getSize(GL),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),

    gl:translatef(TextH, H-TextH, 0.0),
    draw_labels(TextH, Labels),

    gl:loadIdentity(),
    draw_msg(Msg, MsgFont),

    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),

    gl:disable(?GL_TEXTURE_2D),
    gl:disable(?GL_BLEND),
    gl:enable(?GL_DEPTH_TEST).    


build_labels(Font) ->
    L = build_labels(Font, ?ALL_OPTIONS, []),
    dict:from_list(L).

build_labels(_Font, [], Acc) -> %% TODO refactor lists comprehension
    Acc;
build_labels(Font, [L|Ls], Acc) ->
    {_Size, Label} = wx_glfont:render_to_list(Font, desc(L)),
    build_labels(Font, Ls, [{L, Label}|Acc]).


desc(?O_SPIN) ->
    "Spin";
desc(?O_PLANE) ->
    "Plane";
desc(?O_EDGES) ->
    "Edges";
desc(?O_AXES) ->
    "Axes";
desc(?O_TEXT) ->
    "Texture";
desc(?O_MUTE) ->
    "Mute".


draw_labels(TextH, Dict) ->
    draw_labels(?ALL_OPTIONS, TextH, Dict).


draw_labels([], _TextH, _Dict) ->
    ok;
draw_labels([Opt|Opts], TextH, Dict) ->
    case ec_cf:opt(Opt) of
	false ->
	    ok;
	true ->
	    gl:translatef(0.0, -TextH, 0.0),
	    gl:color3ubv(?ON),
	    List = dict:fetch(Opt, Dict),
	    gl:callList(List)
    end,
    draw_labels(Opts, TextH, Dict).


draw_msg(undefined, _Font) ->
    ok;
draw_msg({_Ref, Msg}, Font) ->
    %% TextH = float(wx_glfont:height(GLFixed)),
    %% gl:translatef(0.0, -TextH, 0.0),
    gl:color3ub(255, 255, 0),
    wx_glfont:render(Font, Msg).


test() ->
    message("w00t !").
