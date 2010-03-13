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

-define(SPAN, 5).

-define(RATE,  4410).
%%-define(RATE,  8000).
%%-define(RATE,  44100).

-define(VELF, 3.0).

%% -define(CHUNKSIZE, 256).
%% -define(BINARY_CHUNKSIZE, (?CHUNKSIZE*2)).    %% binary chunk size

%% %% XXXXX gros problemes de synchro ça bouffe le stream input trop
%% %% rapidemnt wtf #{@
%% -define(SLEEP, trunc(?CHUNKSIZE/?RATE*1000)). %% sleep time

-record(state, {rec, points=[], colors=[]}).

%% Minimum value a `signed short int' can hold.
-define(SHRT_MIN, -32768).
%%-define(ISHRT_MIN, (1/?SHRT_MIN)).

%% -define(PLAYER, player).

%% -record(player, {
%% 	  track = 1,    %% current track
%% 	  ntracks = 0,  %% number of tracks
%% 	  tracks = {},  %% tuple of track names
%% 	  iport, oport, %% input / output ports
%% 	  acc = <<>>    %% accumulator
%% 	 }).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


chunk(List)
    gen_server:cast(?SERVER, {chunk, List}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    curve:start(), %% FIXME :stop() not called in terminate ?!

    %%
    %% ===========>  HERE  <================
    %%
    RecPid = case Playlist of
		 undefined ->
		     spawn_link(fun ?MODULE:rec/0);
		 Playlist ->
		     spawn_link(fun() -> ?MODULE:player(Playlist) end)
	     end,

    ec_gui:register(self()),
    {ok, #state{rec=RecPid}}.

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

handle_info({'EXIT', RecPid, Reason}, #state{rec=RecPid} = State) ->
    ?D_F("Recorder pid ~p exited with reason ~p", [RecPid, Reason]),
    {stop, Reason, State};

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
terminate(_Reason, #state{rec=RecPid}) ->
    ?D_TERMINATE(_Reason),
    Ref = make_ref(),
    RecPid ! {self(), Ref, stop},
    receive
	{Ref, ok} ->
	    curve:stop()
    end.

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
    gl:lineWidth(1.0),
    gl:'begin'(?GL_LINE_STRIP),
    draw2(Points, Colors),
    gl:'end'().


draw2([], []) ->
    ok;
draw2([P = {X, Y, Z}|Ps], [C|Cs]) ->
    %% Spline
    gl:color3fv(C),
    gl:vertex3fv(P),

    %% Particles
    Vel = {X*?VELF, Y*?VELF, Z*?VELF},
    ec_ps:add(#part{pos=P, col=C, ttl=2.0*?MICRO, vel=Vel}),

    draw2(Ps, Cs).
