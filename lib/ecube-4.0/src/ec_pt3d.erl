%%%-------------------------------------------------------------------
%%% File    : ec_pt3d.erl
%%% Author  : Olivier Girondel <olivier@biniou.info>
%%% Description : Packard-Takens 3d reconstruction from sound
%%%-------------------------------------------------------------------
-module(ec_pt3d).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").
-include("ec_ps.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Module API
-export([points/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(F32, 32/float-native).

-define(RATE,  4410).
%%-define(RATE,  8000).
%%-define(RATE,  44100).

-define(VELF, 3.0).

-record(state, {rec, points=[]}).

%% Minimum value a `signed short int' can hold.
-define(SHRT_MIN, -32768).
%%-define(ISHRT_MIN, (1/?SHRT_MIN)).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

points(Points) ->
    gen_server:cast(?SERVER, {points, Points}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),

    Env = ec_gui:get_env(),
    wx:set_env(Env),

    curve:start(), %% FIXME curve:stop() not called in terminate/2 ?!

    rec:start(),
    ok = rec:record(?RATE),

    ec_gui:register(self()),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({points, Points}, State) ->
    {noreply, State#state{points=Points}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Pid, Ref, {draw, GL}}, #state{points=Ps} = State) ->
    wxGLCanvas:setCurrent(GL),
    Res = draw(Ps),
    Pid ! {Ref, Res},
    {noreply, State};

handle_info(_Info, State) ->
    ?D_UNHANDLED(_Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?D_TERMINATE(_Reason),
    curve:stop(),
    rec:stop().

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
to_comp(Val) -> %% rescale dans [0.0..2.0] pour du glColor3fv
    abs(Val) * 2.0.


draw([]) ->
    ok;
draw(Points) ->
    gl:lineWidth(2.0),
    gl:'begin'(?GL_LINE_STRIP),
    Draw = fun(P = {X, Y, Z}) ->
		   C = {to_comp(X), to_comp(Y), to_comp(Z)},
		   Vel = {X*?VELF, Y*?VELF, Z*?VELF},
		   ec_ps:add(#part{pos=P, col=C, ttl=1.5*?MICRO, vel=Vel}),
		   gl:color3fv(C),
		   gl:vertex3fv(P)
	   end,
    [Draw(P) || P <- Points],
    gl:'end'().
