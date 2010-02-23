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
-export([loop/1]).

-define(SERVER, ?MODULE).

-record(state, {
	  p=[]    %% the particles
	 }).

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

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
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
    Pid = spawn(?MODULE, loop, [Part]),
    Parts = [Pid|P],
    {noreply, State#state{p=Parts}};

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
    NewPids = draw(P),
    Pid ! {Ref, ok},
    {noreply, State#state{p=NewPids}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{p=Parts}) ->
    ?D_TERMINATE(_Reason),
    ?D_F("Killing ~p particles", [length(Parts)]),
    [P ! stop || P <- Parts].

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
loop(Part) ->
    receive
	stop ->
	    ok;

	{From, Ref, go} ->
	    Res = go(Part),
	    From ! {Ref, self(), Res},
	    %% ?D_F("Res: ~p, rec= ~p", [Res, is_record(Res, part)]),
	    case Res of
		dead ->
		    done;
		
		P when is_record(P, part) ->
		    loop(P)
	    end
    end.


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
	    {NPos, NVel} = move(P, ElapsedS),
	    P#part{pos=NPos, vel=NVel, last=Now}
    end.


draw([]) ->
    [];
draw(Particles) when is_list(Particles) ->
    N = length(Particles),
    %% ?D_F("~p particles ~p processes", [N, length(processes())]),
    Ref = make_ref(),
    [P ! {self(), Ref, go} || P <- Particles],
    %% draw_particles(N).
    %% ?D_F("Drawing particles: ~p", [Particles]),
    %% safe_draw_particles(N, Ref, Particles).
    draw_particles(N, Ref).

draw_particles(N, Ref) ->
    %% gl:color3ubv(?WHITE),
    gl:pointSize(?PTSIZE),
    gl:'begin'(?GL_POINTS),
    NewPids = draw_particles(N, Ref, []),
    gl:'end'(),
    NewPids.

draw_particles(0, _Ref, Acc) ->
    Acc;
draw_particles(N, Ref, Acc) ->
    NM1 = N-1,
    receive
	{Ref, _Pid, dead} ->
	    %% ?D_F("particle ~p died", [_Pid]),
	    draw_particles(NM1, Ref, Acc);

	{Ref, Pid, #part{pos=Pos, col=Col}} ->
	    render_particle(Pos, Col),
	    draw_particles(NM1, Ref, [Pid|Acc])
    end.


render_particle(Pos, Col) ->
    %% ?D_F("Pos= ~p", [Pos]),
    gl:color3ubv(Col),
    gl:vertex3fv(Pos).


move(#part{pos=Pos, vel=Vel, acc=undefined}, Elapsed) ->
    NPos = move(Pos, Vel, Elapsed),
    {NPos, Vel};
move(#part{pos=Pos, vel={Vx, Vy, Vz}, acc={Ax, Ay, Az}}, Elapsed) ->
    NVx = Vx + Elapsed * Ax,
    NVy = Vy + Elapsed * Ay,
    NVz = Vz + Elapsed * Az,
    NVel = {NVx, NVy, NVz},
    NPos = move(Pos, NVel, Elapsed),
    {NPos, NVel}.


move({Px, Py, Pz}, {Vx, Vy, Vz}, Elapsed) ->
    NPx = Px + Elapsed * Vx,
    NPy = Py + Elapsed * Vy,
    NPz = Pz + Elapsed * Vz,
    {NPx, NPy, NPz}.
