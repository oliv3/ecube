%%%-------------------------------------------------------------------
%%% File    : ec_gui.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : GUI process
%%%-------------------------------------------------------------------
-module(ec_gui).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

%% wx_object API
-export([start_link/0]).

%% module API
-export([get_env/0, register/1, unregister/1]).
-export([register_texture/2, unregister_texture/2, unregister_textures/1]).

%% wx_object callbacks
-export([init/1]).

%% proc_lib callbacks
-export([system_code_change/4, system_continue/3]).

-define(SERVER, ?MODULE).

-define(ZMAX, 5).

-record(state,  {ifps, frame, gl, size, plugins=[]}). %% v1 record
-record(state3, {ifps, frame, gl, size, plugins=[], textures=ets()}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).


get_env() ->
    Ref = make_ref(),
    ?SERVER ! {self(), Ref, get_env},
    receive
	{Ref, Env} ->
	    Env
    end.


register(Pid) ->
    ?SERVER ! {register, Pid}.


unregister(Pid) ->
    ?SERVER ! {unregister, Pid}.


register_texture(Pid, Tid) ->
    ?SERVER ! {register_texture, Pid, Tid}.


unregister_texture(Pid, Tid) ->
    ?SERVER ! {unregister_texture, Pid, Tid}.


unregister_textures(Pid) ->
    ?SERVER ! {unregister_textures, Pid}.


%%====================================================================
%% wx_object Callbacks
%%====================================================================
init(Parent) ->
    %% process_flag(trap_exit, true),
    Size = ec:get_env(size),
    Wx = wx:new(),
    Frame = ec_win:new(Wx, Size),
    GL = ec_win:gl(),
    ?D_REGISTER(?SERVER, self()),
    Debug = sys:debug_options([]),
    IFPS = trunc(1000 / ec:get_env(fps)),
    proc_lib:init_ack(Parent, {ok, self()}),
    tick(IFPS),
    loop(Parent, Debug, #state3{ifps=IFPS, frame=Frame, gl=GL, size=Size}).


%%====================================================================
%% proc_lib callbacks
%%====================================================================
system_code_change(#state{frame=Frame, gl=GL, size=Size, plugins=Plugins} = _State, _Module, ?V1 = _OldVsn, _Extra) ->
    ?D_F("code v3 system_code_change from ~p (~p, ~p, ~p)~n", [_OldVsn, _State, _Module, _Extra]),
    NewState = #state3{frame=Frame, gl=GL, size=Size, plugins=Plugins, textures=ets()},
    ec_win:set_title(),
    {ok, NewState};

system_code_change(#state3{frame=Frame, gl=GL, size=Size, plugins=Plugins} = _State, _Module, {down, ?V1} = _OldVsn, _Extra) ->
    ?D_F("code v3 system_code_change to ~p (~p, ~p, ~p)~n", [_OldVsn, _State, _Module, _Extra]),
    NewState = #state{frame=Frame, gl=GL, size=Size, plugins=Plugins},
    ec_win:set_title(),
    ?D_F("code v3 system_code_change to 1.0: NewState= ~p~n", [NewState]),
    {ok, NewState}.


system_continue(Parent, Debug, State) ->
    ?D_F("code v3 system_continue(~p, ~p, ~p)~n", [Parent, Debug, State]),
    loop(Parent, Debug, State).


%%====================================================================
%% Internal functions
%%====================================================================
loop(Parent, Debug, #state3{ifps=IFPS, gl=GL, textures=Texs} = State) ->
    receive
	{Pid, Ref, get_env} ->
	    Pid ! {Ref, wx:get_env()},
	    loop(Parent, Debug, State);

	{register, Pid} ->
	    link(Pid),
            Plugins = [Pid|State#state3.plugins],
	    loop(Parent, Debug, State#state3{plugins=Plugins});

	{unregister, Pid} ->
	    unlink(Pid),
	    {ok, NewState} = unregister(Pid, State),
	    loop(Parent, Debug, NewState);

	{register_texture, Pid, Tid} ->
	    ets:insert(Texs, {Pid, Tid}),
	    loop(Parent, Debug, State);

	{unregister_texture, Pid, Tid} ->
	    ets:delete_object(Texs, {Pid, Tid}),
	    wxGLCanvas:setCurrent(GL),
	    gl:deleteTextures([Tid]),
	    loop(Parent, Debug, State);

	{unregister_textures, Pid} ->
	    Records = ets:lookup(Texs, Pid),
	    ets:delete(Texs, Pid),
	    Tids = [Tid || {_Pid, Tid} <- Records],
	    ?D_F("freeing textures owned by pid ~p: ~p~n", [Pid, Tids]),
	    wxGLCanvas:setCurrent(GL),
	    gl:deleteTextures(Tids),
	    loop(Parent, Debug, State);

	draw ->
	    NewState = draw(State),
	    tick(IFPS),
	    loop(Parent, Debug, NewState);

	{'EXIT', Pid, Reason} ->
	    ?D_F("got EXIT from ~p with reason: ~p~n", [Pid, Reason]),
            case unregister(Pid, State) of
		{ok, NewState} ->
		    loop(Parent, Debug, NewState);

		{false, State} ->
                    wxFrame:destroy(State#state3.frame),
                    wx:destroy(),
                    ?D_F("wxWidgets destroyed~n", []),
                    exit(Reason)
            end;

        {system, From, Request} ->
	    ?D_F("code v3 system message: From ~p Request: ~p~n", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
	
	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end.


draw(#state3{gl=GL, size=Size} = State) ->
    wxGLCanvas:setCurrent(GL),
    set_view(Size),
    NewState = draw2(State),
    wxGLCanvas:swapBuffers(GL),
    NewState.


draw2(State) ->
    draw2(State, State#state3.plugins, []).
draw2(State, [], Acc) ->
    State#state3{plugins=lists:reverse(Acc)};
draw2(State, [Pid|Pids], Acc) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, {draw, State#state3.gl}},
    receive
        {Ref, _Result} ->
            draw2(State, Pids, [Pid|Acc]);

        {'EXIT', Pid, _Reason} ->
            ?D_F("plugin ~p crashed, reason: ~p~n", [Pid, _Reason]),
            {ok, NewState} = unregister(Pid, State),
            draw2(NewState, Pids, Acc)
    end.


set_view({Width, Height}) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:clearColor(0.0, 0.0, 0.0, 1.0),
    gl:clearDepth(?ZMAX),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    Ratio = Width / Height,

    glu:perspective(ec_cf:fov(), Ratio, 0.1, ?ZMAX),
    glu:lookAt(0, 0, 3.14,
	       0, 0, -3.14,
	       0, 1, 0),

    {RotX, RotY, RotZ} = ec_cf:rot(),
    gl:rotatef(RotX, 1.0, 0.0, 0.0),
    gl:rotatef(RotY, 0.0, 1.0, 0.0),
    gl:rotatef(RotZ, 0.0, 0.0, 1.0),

    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).


tick(T) ->
    erlang:send_after(T, ?SERVER, draw).


unregister(Pid, #state3{plugins=Plugins} = State) ->
    case lists:member(Pid, Plugins) of
        true ->
            NPlugins = Plugins -- [Pid],
            unregister_textures(Pid),
	    {ok, State#state3{plugins=NPlugins}};

        false ->
	    {false, State}
    end.


ets() ->
    ets:new(?SERVER, [duplicate_bag, private]).
