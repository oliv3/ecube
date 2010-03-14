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
-export([chunk/1]).

%% Internal exports
%% -export([rec/0, player/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(F32, 32/float-native).

-define(SPAN, 4).

-define(RATE,  4410).
%%-define(RATE,  8000).
%%-define(RATE,  44100).

-define(VELF, 3.0).

-record(state, {rec, points=[], colors=[]}).

%% Minimum value a `signed short int' can hold.
-define(SHRT_MIN, -32768).
%%-define(ISHRT_MIN, (1/?SHRT_MIN)).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

chunk(List) ->
    gen_server:cast(?SERVER, {chunk, List}).

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
handle_cast({chunk, List}, State) ->
    {Points, Colors} = pt3d(List),
    {Curve1, Curve2} = case curve:curve(?SPAN, Points) of
			   {error, badarg} ->
			       {[], []};
			   C ->
			       Cols = curve:curve(?SPAN, Colors),
			       {C, Cols}
		       end,
    {noreply, State#state{points=Curve1, colors=Curve2}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Pid, Ref, {draw, GL}}, #state{points=Ps, colors=Cs} = State) ->
    wxGLCanvas:setCurrent(GL),
    Res = draw(Ps, Cs),
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
to_comp(Val) -> %% rescale dans [0.0 .. 1.0] pour du glColor3fv
    (Val + 1.0) / 2.


pt3d(List) ->
    pt3d(List, [], []).
pt3d([X,Y,Z,R,G,B|Tail], Acc1, Acc2) ->
    Point = {X, Y, Z},
    Color = {to_comp(R), to_comp(G), to_comp(B)},
    pt3d([Y,Z,R,G,B|Tail], [Point|Acc1], [Color|Acc2]);
pt3d(_Rest, Acc1, Acc2) -> %% no need to reverse, these are points
    {Acc1, Acc2}.


draw([], []) ->
    ok;
draw(Points, Colors) ->
    gl:lineWidth(2.0),
    gl:'begin'(?GL_LINE_STRIP),
    %% ?D_F("draw2: ~p ~p~n", [Points, Colors]),
    draw2(Points, Colors),
    gl:'end'().


-define(ORIGIN, {0.0, -1.0, 0.0}).

draw2([], []) ->
    ok;
draw2([P = {X, Y, Z}|Ps], [C|Cs]) ->
    %% Spline
    gl:color3fv(C),
    gl:vertex3fv(P),
    %% gl:vertex3fv(?ORIGIN), %% fountain

    %% Particles
    %% Vel = {X*?VELF, Y*?VELF, Z*?VELF},
    %% Vel = {X*?VELF, abs(Y*?VELF), Z*?VELF}, %% fountain
    %% ec_ps:add(#part{pos=P, col=C, ttl=1.5*?MICRO, vel=Vel}),

    draw2(Ps, Cs).
