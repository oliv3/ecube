%%%-------------------------------------------------------------------
%%% File    : ec_ps.erl
%%% Author  : Olivier Girondel <olivier@biniou.info>
%%% Description : Particle system
%%%-------------------------------------------------------------------
-module(ec_ps).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").
-include("ec_ps.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([add/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {p=[]}). %% the particles

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    ec_gui:register(self()),
    {ok, #state{}}.


add(Part) when is_record(Part, part) ->
    gen_server:cast(?SERVER, {add, Part}).

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
handle_cast({add, Part}, #state{p=P} = State) ->
    {noreply, State#state{p=[Part|P]}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Pid, Ref, {draw, GL}}, #state{p=P} = State) ->
    wxGLCanvas:setCurrent(GL),
    NewParts = draw(P),
    Pid ! {Ref, ok},
    {noreply, State#state{p=NewParts}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
go(#part{ttl=TTL, born=Born, last=Last} = P) ->
    Now = now(),
    Age = timer:now_diff(Now, Born),
    if
	Age > TTL ->
	    %% ?D_F("~p died", [self()]),
	    dead;
	
	true ->
	    Elapsed = timer:now_diff(Now, Last),
	    ElapsedS = Elapsed / ?MICRO, %% in secs
	    %% ?D_F("~p Age= ~p Elapsed= ~p (~p secs)", [self(), Age, Elapsed, ElapsedS]),
	    NPos = move(P, ElapsedS),
	    P#part{pos=NPos, last=Now}
    end.


draw(Particles) ->
    %% _N = length(Particles),
    %% ?D_F("~p particles", [_N]),
    Moved = [go(P) || P <- Particles],
    Alive = [P || P <- Moved, P =/= dead],
    draw2(Alive),
    Alive.


draw2([]) ->
    ok;
draw2(Particles) ->
    gl:pointSize(?PTSIZE),
    gl:'begin'(?GL_POINTS),
    [render_particle(P) || P <- Particles],
    gl:'end'().


render_particle(#part{pos=Pos, col=Col}) ->
    gl:color3fv(Col),
    gl:vertex3fv(Pos).


move(#part{pos={Px, Py, Pz}, vel={Vx, Vy, Vz}}, Elapsed) ->
    NPx = Px + Elapsed * Vx,
    NPy = Py + Elapsed * Vy,
    NPz = Pz + Elapsed * Vz,
    {NPx, NPy, NPz}.
