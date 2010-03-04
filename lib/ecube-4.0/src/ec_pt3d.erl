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
%%-export([chunk/1]).

%% Internal exports
-export([rec/0, player/1]). %%, tick/1]). %%, gen/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%% -define(GEN, datagen).
%%-define(REC, recorder).
-define(F32, 32/float-native).

-define(SPAN, 5).

-define(RATE,  4410).
%%-define(RATE,  8000).
%%-define(RATE,  44100).
%% -define(RATE,  5000).

%%-define(CFACT, 20).
%%-define(CFACT, 17). %% gives ~= 256 points
%%-define(CFACT, 50).

-define(VELF, 3.0).

-define(CHUNKSIZE, 256). %% (round(?RATE/?CFACT))). %% chunk size
-define(BINARY_CHUNKSIZE, (?CHUNKSIZE*2)). %% binary chunk size
%% -define(CHUNKBITS, (?CHUNKSIZE*8)).
-define(SLEEP, trunc(?CHUNKSIZE/?RATE*1000)). %% sleep time
-define(IRATE, round(1/(?RATE*1.01))). %% XXX ~= 0 even in millisecs
-record(state, {rec, spline=[]}).

%% /* Minimum and maximum values a `signed short int' can hold. */
-define(SHRT_MIN, -32768).
-define(SHRT_MAX, 32767).

-define(ISHRT_MIN, (1/?SHRT_MIN)).
%%-define(ISHRT_MIN, ((4/3)/?SHRT_MIN)).

%% spline stuff
-define(NBPOINTS,   (?CHUNKSIZE-5)).
-define(RESOLUTION, 6).
-define(DELTA,      (1/(?NBPOINTS*?RESOLUTION))).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

chunk(Binary) ->
    gen_server:cast(?MODULE, {chunk, Binary}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    curve:start(), %% FIXME :stop() not called in terminate ?!
    Playlist = ec:get_env(playlist),
    ?D_F("Playlist: ~p", [Playlist]),
    RecPid = case Playlist of
		 undefined ->
		     spawn_link(?MODULE, rec, []);
		 Playlist ->
		     spawn_link(?MODULE, player, [Playlist])
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
handle_cast({chunk, Binary}, State) ->
    Decoded = decode(Binary),
    Points = pt3d(Decoded),
    Curve = case curve:curve(?SPAN, Points) of
		{error, badarg} ->
		    [];
		C ->
		    %% Now = now(),
		    %% Add = fun(Point = {X, Y, Z}) ->
		    %% 		  Vel = {X*?VELF, Y*?VELF, Z*?VELF},
		    %% 		  ec_ps:add(#part{pos=Point, ttl=2.0*?MICRO,
		    %% 				  vel=Vel, born=Now, last=Now})
		    %% 	  end,
		    %% [Add(P) || P <- C],
		    C
	    end,
    {noreply, State#state{spline=Curve}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Pid, Ref, {draw, GL}}, #state{spline=S} = State) ->
    wxGLCanvas:setCurrent(GL),
    %% Ref2 = make_ref(),
    %% RecPid ! {self(), Ref2, get},
    %% receive
    %% 	{Ref2, Chunk} ->
    %% io:format("Points: ~p~n", [Points]),
    Res = draw(S),
    Pid ! {Ref, Res},
    %%end,
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
rec() ->
    process_flag(trap_exit, true),

    %% play .flac and rescale
    %% sox -q /home/olivier/Desktop/pompBabylone/Kraftwerk\ -\ 1975\ -\ Radio-Activity/01\ -\ Geiger\ Counter.flac -t raw -r 4410 -b 16 -c 1 -e signed-integer -L - 
    %% + ouvrir un 2è port avec un play -

    Cmd = "/usr/bin/rec -q -t raw"
	++ " -r " ++ integer_to_list(?RATE)
	%% -L == endian little
	++ " --buffer 256 "
	++ " -b 16 -c 1 -e signed-integer -L - 2> /dev/null",
    Port = open_port({spawn, Cmd}, [binary]),
    rec1(Port, <<>>).


rec1(Port, Acc) ->
    receive
	{Port, {data, Data}} ->
	    NData = concat_binary([Acc, Data]),
	    if
		byte_size(NData) >= ?BINARY_CHUNKSIZE ->
		    {Chunk, Rest} = split_binary(NData, ?BINARY_CHUNKSIZE),
		    chunk(Chunk),
		    rec1(Port, Rest);

		true ->
		    rec1(Port, NData)
	    end;

	{Pid, Ref, stop} ->
	    Pid ! {Ref, ok}
    end.


decode(Chunk) ->
    decode(Chunk, []).
decode(<<>>, Acc) ->
    lists:reverse(Acc);
decode(<<Val:16/signed-little, Rest/binary>>, Acc) ->
    decode(Rest, [Val|Acc]).


to_pos(Val) ->
    (Val / ?SHRT_MIN).

%% to_comp(Val) ->
%%     round(((Val / ?SHRT_MIN) + 1.0) / 2 * 255).

to_comp(Val) -> %% rescale dans [0.0 .. 1.0] pour du glColor3fv
    ((Val / ?SHRT_MIN) + 1.0) / 2.


pt3d(List) ->
    pt3d(List, []).
pt3d([X,Y,Z|Tail], Acc) ->
    %%Point = {float(X), float(Y), float(Z)},
    %%Point = {X, Y, Z},
    Point = {to_pos(X), to_pos(Y), to_pos(Z)},
    pt3d([Y,Z|Tail], [Point|Acc]);
pt3d(_Rest, Acc) -> %% no need to reverse, these are points
    Acc.


d(Point = {X, Y, Z}, Now) ->
    %% gl:color3fv(Point),
    Vel = {X*?VELF, Y*?VELF, Z*?VELF},
    ec_ps:add(#part{pos=Point, ttl=2.0*?MICRO, vel=Vel, born=Now, last=Now}), %% , vel=Part}),
    gl:vertex3fv(Point).


draw([]) ->
    ok;
draw(Points) ->
%%    gl:pushMatrix(),
%%    gl:scalef(?ISHRT_MIN, ?ISHRT_MIN, ?ISHRT_MIN),
    gl:lineWidth(1.0),
    gl:color3ub(150, 150, 250),
    gl:'begin'(?GL_LINE_STRIP),
    Now = now(),
    [d(P, Now) || P <- Points],
    %%[gl:vertex3fv(P) || P <- Points],
    gl:'end'().

%%    gl:popMatrix().


mkspline(Chunk) ->
    mkspline(Chunk, [], []).
mkspline([X,Y,Z,R,G,B|Tail], AccC, AccB) ->
    Col = {to_comp(R), to_comp(G), to_comp(B)},
    %% Pos = <<X:?F32, Y:?F32, Z:?F32>>,
    X2 = to_pos(X), Y2 = to_pos(Y), Z2 = to_pos(Z),
    Pos = <<X2:?F32, Y2:?F32, Z2:?F32>>,
    mkspline([Y,Z,R,G,B|Tail], [Col|AccC], [Pos|AccB]);
mkspline(_Rest, AccC, AccB) ->
    %% {AccC, concat_binary(AccB)}.
    Res = {AccC, concat_binary(AccB)},
    %% ?D_F("mkspline: ~p", [Res]),
    Res.


draw2([], _Step) ->
    ok;
draw2([Col|Colors], Step) ->
    gl:color3fv(Col),
    %% gl:color3fv({1.0,1.0,1.0}),
    %% ?D_F("Color: ~p", [Col]),
    NewStep = draw3(?RESOLUTION, Step),
    %% ?D_F("NewStep: ~p", [NewStep]),
    draw2(Colors, NewStep).

draw3(0, Step) ->
    Step;
draw3(N, Step) ->
    gl:evalCoord1f(Step),
    draw3(N-1, Step+?DELTA).

-define(UNO, 1.0). %% TODO reutiliser dans les guards de loops

%% TODO remove, dead code
spline(Chunk) ->
    Colors = [to_comp(V) || V <- Chunk],
    spline(Chunk, Colors).
spline([P0,P1,P2,P3,P4,P5|PTail], [C0,C1,C2,C3,C4,C5|CTail]) ->
    Pos1 = <<P0:?F32, P1:?F32, P2:?F32>>,
    Pos2 = <<P1:?F32, P2:?F32, P3:?F32>>,
    Pos3 = <<P2:?F32, P3:?F32, P4:?F32>>,
    Pos4 = <<P3:?F32, P4:?F32, P5:?F32>>,
    Pos = concat_binary([Pos1, Pos2, Pos3, Pos4]),

    Col1 = <<C0:?F32, C1:?F32, C2:?F32, ?UNO:?F32>>,
    Col2 = <<C1:?F32, C2:?F32, C3:?F32, ?UNO:?F32>>,
    Col3 = <<C2:?F32, C3:?F32, C4:?F32, ?UNO:?F32>>,
    Col4 = <<C3:?F32, C4:?F32, C5:?F32, ?UNO:?F32>>,
    Col = concat_binary([Col1, Col2, Col3, Col4]),

    gl:map1f(?GL_MAP1_VERTEX_3, 0.0, 1.0, 3, 4, Pos),
    gl:map1f(?GL_MAP1_COLOR_4, 0.0, 1.0, 4, 4, Col),
    gl:enable(?GL_MAP1_VERTEX_3),
    gl:enable(?GL_MAP1_COLOR_4),
    gl:'begin'(?GL_LINE_STRIP),
    eval(),
    gl:'end'(),
    gl:disable(?GL_MAP1_VERTEX_3),
    gl:disable(?GL_MAP1_COLOR_4),
    spline([P3,P4,P5|PTail], [C3,C4,C5|CTail]);
spline(_RestP, _RestC) ->
    ok.

-define(RES, 100).
-define(IRES, (1/(?RES-1))).

eval() ->
    eval(0.0).
eval(Val) when Val > ?UNO ->
    ok;
eval(Val) ->
    gl:evalCoord1f(Val),
    eval(Val + ?IRES).

%%
%% player stuff
%% coded on a rainy day
%%

-define(PLAYER, player).

-record(player, {
	  track = 1, %% current track
	  ntracks, %% number of tracks (c'est peut etre pas un super nom pour valoriser mais osef)
	  tracks = {}, %% the tracks themselves
	  iport, oport, %% input / output ports
	  acc = <<>> %% accumulator
	 }).


player(Playlist) when is_atom(Playlist) ->
    player(atom_to_list(Playlist));

player(Playlist) when is_list(Playlist) ->
    process_flag(trap_exit, true),
    {ok, Files} = file:consult(Playlist),
    ?D_F("Playlist is made of ~p", [Files]),
    TPL = list_to_tuple(Files),
    LPL = tuple_size(TPL),
    State0 = #player{ntracks=LPL, tracks=TPL},
    State1 = play_track(State0),
    ?D_REGISTER(?PLAYER, self()), %% To send messages to the player process
    player(State1);

player(#player{track=T, ntracks=NT, tracks=_Tracks, iport=IP, oport=OP, acc=Acc} = State) ->
    receive
	next ->
	    T1 = case T of
		     NT ->
			 1;
		     T ->
			 T + 1
		 end,
	    NewState = play_track(State#player{track=T1}),
	    player(NewState);

	previous ->
	    T1 = case T of
		     1 ->
			 NT;
		     T ->
			 T - 1
		 end,
	    NewState = play_track(State#player{track=T1}),
	    player(NewState)

    after 0 ->
	    receive
		{IP, {data, Data}} ->
		    %% ?D_F("~p got ~p bytes", [IP, byte_size(Data)]),
		    NData = concat_binary([Acc, Data]),
		    if
			byte_size(NData) >= ?BINARY_CHUNKSIZE ->
			    {Chunk, Rest} = split_binary(NData, ?BINARY_CHUNKSIZE),
			    chunk(Chunk),
			    timer:sleep(?SLEEP),
			    player(State#player{acc=Rest});

			true ->
			    player(State#player{acc=NData})
		    end;

		{'EXIT', IP, normal} ->
		    player(State#player{iport=undefined});

		{'EXIT', OP, normal} ->
		    self() ! next,
		    player(State#player{oport=undefined});

		{Pid, Ref, stop} ->
                    catch port_close(IP),
		    %% XXX simply ugly
		    os:cmd("killall play"),
                    catch port_close(OP),
		    Pid ! {Ref, ok}

		%% TODO
		%% random ->
		%%     player(State);

	    after 0 ->
		    player(State)
	    end
    end.


play_track(#player{track=T, tracks=Tracks, iport=IP, oport=OP} = State) ->
    catch port_close(IP),

    %% XXX simply ugly
    os:cmd("killall play"),

    catch port_close(OP),
    Name = "\"" ++ element(T, Tracks) ++ "\"",
    ?D_F("Playing track ~p: ~p", [T, Name]),

    RecCmd = rec_cmd(Name),
    IPort = open_port({spawn, RecCmd}, [binary, in]),

    PlayCmd = play_cmd(Name),
    OPort = open_port({spawn, PlayCmd}, []),

    State#player{iport=IPort, oport=OPort, acc = <<>>}.


rec_cmd(File) ->
    "/usr/bin/sox -q "
	++ File ++ " -t raw -r " ++ integer_to_list(?RATE)
	++ " --buffer " ++ integer_to_list(?BINARY_CHUNKSIZE)
	++ " -b 16 -c 1 -e signed-integer -L -".

play_cmd(File) ->
    "/usr/bin/play -q " ++ File ++ " 2> /dev/null".
