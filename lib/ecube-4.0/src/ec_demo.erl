%%%-------------------------------------------------------------------
%%% File    : ec_demo.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : OpenGL demo
%%%-------------------------------------------------------------------
-module(ec_demo).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1, system_code_change/4, system_continue/3]).

-define(SERVER, ?MODULE).

-record(state, {tex_size, splash, texs}).

-egg('Duo habet et bene pendentes').

%%====================================================================
%% API
%%====================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%%====================================================================
%% Callbacks
%%====================================================================
init(Parent) ->
    process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    Self = self(),
    ?D_REGISTER(?SERVER, Self), %% not needed
    ec_gui:register(Self),
    Debug = sys:debug_options([]),
    TS = ec:get_env(tex_size),
    proc_lib:init_ack(Parent, {ok, Self}),
    loop(Parent, Debug, #state{tex_size=TS}).


loop(Parent, Debug, #state{tex_size=TexSize, splash=undefined, texs=undefined} = State) ->
    receive
	{Pid, Ref, {draw, GL}} ->
	    N = ec:get_env(n),
            wxGLCanvas:setCurrent(GL),
	    Splash = splash_texture(GL, State#state.tex_size),
	    Texs = erlang:make_tuple(N, undefined),
	    NewTexs = ec_cube:draw(TexSize, Splash, Texs),
	    NewState = State#state{splash=Splash, texs=NewTexs},
	    Pid ! {Ref, ok},
	    loop(Parent, Debug, NewState);

        {system, From, Request} ->
	    ?D_F("code v3 system message(1): From ~p Request: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end;

loop(Parent, Debug, #state{tex_size=TexSize, splash=Splash, texs=Texs} = State) ->
    receive
	{Pid, Ref, {draw, GL}} ->
            wxGLCanvas:setCurrent(GL),
	    NewTexs = ec_cube:draw(TexSize, Splash, Texs),
	    Pid ! {Ref, ok},
	    loop(Parent, Debug, State#state{texs=NewTexs});

	{'EXIT', _Pid, Reason} ->
	    ?D_F("got EXIT from ~p with reason: ~p", [_Pid, Reason]),
	    exit(Reason);

        {system, From, Request} ->
	    ?D_F("code v3 system message(2): From ~p Request: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end.


system_code_change(_State, _Module, ?V1 = _OldVsn, _Extra) ->
    ?D_F("code v3 system_code_change from ~p (~p, ~p, ~p)", [_OldVsn, _State, _Module, _Extra]),
    {ok, #state{}};
system_code_change(_State, _Module, {down, ?V1} = _OldVsn, _Extra) ->
    ?D_F("code v3 system_code_change to ~p (~p, ~p, ~p)", [_OldVsn, _State, _Module, _Extra]),
    {ok, undefined}.


system_continue(Parent, Debug, State) ->
    ?D_F("code v3 system_continue(~p, ~p, ~p)", [Parent, Debug, State]),
    loop(Parent, Debug, State).

%%====================================================================
%% Internal functions
%%====================================================================
splash_texture(GL, TexSize) ->
    wxGLCanvas:setCurrent(GL),
    [TexId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    %% Those two calls not used since we map the texture in [0.0 .. 1.0]
    %% gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    %% gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    Splash = ec:get_env(splash),
    File = case Splash of
	       undefined ->
		   ec:priv_path(?SPLASH);

	       F when is_list(F) ->
		   ec:priv_path(F);

	       Other ->
		   exit({error, {bad_splash, Other}})
	   end,
    Img0 = wxImage:new(),
    case wxImage:loadFile(Img0, File) of
	true ->
	    wxImage:rescale(Img0, TexSize, TexSize),
	    Img1 = wxImage:mirror(Img0, [{horizontally, false}]),
	    Bin = wxImage:getData(Img1),
	    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, TexSize, TexSize,
			  0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Bin),
	    ec_gui:register_texture(self(), TexId),
	    TexId;

	false ->
	    exit({error, enoent})
    end.
