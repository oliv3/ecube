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

%% Internal exports
-export([rec/0, player/1]).

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

-define(CHUNKSIZE, 256).
-define(BINARY_CHUNKSIZE, (?CHUNKSIZE*2)).    %% binary chunk size

%% XXXXX gros problemes de synchro ça bouffe le stream input trop
%% rapidemnt wtf #{@
-define(SLEEP, trunc(?CHUNKSIZE/?RATE*1000)). %% sleep time

-record(state, {rec, spline=[]}).

%% Minimum value a `signed short int' can hold.
-define(SHRT_MIN, -32768).
%%-define(ISHRT_MIN, (1/?SHRT_MIN)).

-define(PLAYER, player).

-record(player, {
	  track = 1,    %% current track
	  ntracks = 0,  %% number of tracks
	  tracks = {},  %% the tracks themselves
	  iport, oport, %% input / output ports
	  acc = <<>>    %% accumulator
	 }).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

chunk(Binary) ->
    gen_server:cast(?SERVER, {chunk, Binary}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    curve:start(), %% FIXME :stop() not called in terminate ?!
    Playlist = ec:get_env(playlist),
    %% ?D_F("Playlist: ~p", [Playlist]),
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
    %% sox -q file.flac -t raw -r 4410 -b 16 -c 1 -e signed-integer -L - 
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

%% to_comp(Val) -> %% rescale dans [0.0 .. 1.0] pour du glColor3fv
%%     ((Val / ?SHRT_MIN) + 1.0) / 2.


pt3d(List) ->
    pt3d(List, []).
pt3d([X,Y,Z|Tail], Acc) ->
    Point = {to_pos(X), to_pos(Y), to_pos(Z)},
    pt3d([Y,Z|Tail], [Point|Acc]);
pt3d(_Rest, Acc) -> %% no need to reverse, these are points
    Acc.


d(Point = {X, Y, Z}, Now) ->
    Vel = {X*?VELF, Y*?VELF, Z*?VELF},
    %% ec_ps:add(#part{pos=Point, ttl=2.0*?MICRO, vel=Vel, born=Now, last=Now}),
    gl:vertex3fv(Point).


draw([]) ->
    ok;
draw(Points) ->
    gl:lineWidth(1.0),
    gl:color3ub(255, 50, 150),
    gl:'begin'(?GL_LINE_STRIP),
    Now = now(),
    [d(P, Now) || P <- Points],
    gl:'end'().


player(Playlist) when is_atom(Playlist) ->
    player(atom_to_list(Playlist));

player(Playlist) when is_list(Playlist) ->
    process_flag(trap_exit, true),
    {ok, Files} = file:consult(Playlist),
    %% ?D_F("Playlist is made of ~p", [Files]),
    TPL = list_to_tuple(Files),
    LPL = tuple_size(TPL),
    State0 = #player{ntracks=LPL, tracks=TPL},
    State1 = play_track(State0),
    ?D_REGISTER(?PLAYER, self()), %% To send messages to the player process
    player(State1);

player(#player{track=T, ntracks=NT, tracks=Tracks,
	       iport=IP, oport=OP, acc=Acc} = State) ->
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
	    player(NewState);

	random ->
	    T1 = random:uniform(10000) rem tuple_size(Tracks) + 1,
	    NewState = play_track(State#player{track=T1}),
	    player(NewState)


%%
%% WTF ca chie completement la synchro la
%%



    after 0 ->
    %% after ?SLEEP ->
	    receive
		{IP, {data, Data}} ->
		    ?D_F("~p got ~p bytes", [IP, byte_size(Data)]),
		    NData = concat_binary([Acc, Data]),
		    ?D_F("Buffer: ~p bytes", [byte_size(NData)]),
		    if
			byte_size(NData) >= ?BINARY_CHUNKSIZE ->
			    %% ?D_YOUPI,
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
		    close_ports(IP, OP),
		    Pid ! {Ref, ok}

	    after 0 ->
		    player(State)
	    end
    end.


play_track(#player{track=T, tracks=Tracks, iport=IP, oport=OP} = State) ->
    close_ports(IP, OP),

    Name = "\"" ++ element(T, Tracks) ++ "\"",
    %% ?D_F("Playing track ~p: ~p", [T, Name]),
    ?D_F("Playing track ~p", [T]),

    RecCmd = rec_cmd(Name),
    PlayCmd = play_cmd(Name),

    IPort = open_port({spawn, RecCmd}, [binary, in]),
    OPort = open_port({spawn, PlayCmd}, []),

    State#player{iport=IPort, oport=OPort, acc = <<>>}.


rec_cmd(File) ->
    "/usr/bin/sox -q "
	++ File ++ " -t raw -r " ++ integer_to_list(?RATE)
	++ " --buffer 1024 " %%" ++ integer_to_list(?BINARY_CHUNKSIZE)
	++ " -b 16 -c 1 -e signed-integer -L - 2> /dev/null".

play_cmd(File) ->
    "/usr/bin/play -q " ++ File ++ " 2> /dev/null".


close_ports(Iport, Oport) ->
    catch port_close(Iport),
    %% XXX simply ugly
    catch port_close(Oport),
    os:cmd("killall play").
