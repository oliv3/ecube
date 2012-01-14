%%%-------------------------------------------------------------------
%%% File    : ec_crystal.erl
%%% Author  : Olivier Girondel <olivier@biniou.info>
%%% Description : Packard-Takens 7d reconstruction
%%%-------------------------------------------------------------------
-module(ec_crystal).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

-behaviour(gen_server).

%% For tests
-compile(export_all).

%% API
-export([start_link/0]).

%% Module API
-export([format/3, delay/1]).
-export([clear/0, feed/1]).

%% For EUnit
-export([is_su/1, is_uu/1]).
-export([su2uu/1, uu2su/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {rec, %% recorder port
		left = <<>>, %% binary left from previous run
		dim = 3, %% 3|4
		ws = 8, %% word size (multiple of 8 for now)
		sign = unsigned,
		order = big, %% big|little
		delay = 1,
		color = rgb, %% white|rgb|time
		tup7 = erlang:make_tuple(7, 0.0),
		ps = 1.0, %% point size
		t = 1.0, %% MaxT for 4d mode
		points = test() %% 7d points
	       }).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

format(Sign, WordSize, Order) ->
    gen_server:cast(?SERVER, {format, Sign, WordSize, Order}).

feed(Bin) when is_binary(Bin) ->
    gen_server:cast(?SERVER, {feed, Bin}).

delay(Delay) when is_integer(Delay) andalso Delay > 0 ->
    gen_server:cast(?SERVER, {delay, Delay}).

clear() ->
    gen_server:cast(?MODULE, {clear}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),

    Env = ec_gui:get_env(),
    wx:set_env(Env),

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
handle_cast({clear}, State) ->
    {noreply, State#state{points=[]}};
handle_cast({format, Sign, WS, Order}, State) ->
    {noreply, State#state{sign=Sign, ws=WS, order=Order}};
handle_cast({feed, Bin0}, #state{ws=WS, delay=Delay, order=Order, sign=Sign, left=L, points=Points} = State) ->
    Bin = list_to_binary([L, Bin0]),
    Type = {Sign, Order},
    [Data, Left] = extract(WS, Bin, Type, Delay),
    %% NewPoints = add(Data),
    NewPoints = [],
    {noreply, State#state{points=lists:flatten([Points,NewPoints]),
			  left=Left}};
handle_cast({delay, Delay}, State) ->
    {noreply, State#state{delay=Delay}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Pid, Ref, {draw, GL}}, #state{points=Points, dim=D, t=T} = State) ->
    wxGLCanvas:setCurrent(GL),
    %% io:format("[i] ~s:draw(~p)~n", [?MODULE, GL]),
    Points2 = filter(D, Points, T),
    Res = draw(Points2),
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

%% rescale dans [0.0..4.0] pour du glColor3fv
%% ouiiiii le max c'est 1.0 mais on estime que les composantes sont au
%% 25% de l'espace possible
to_comp(Val) ->
    abs(Val).


draw([]) ->
    ok;
draw(Points) ->
    %% TODO: GLhint smooth points
    gl:pointSize(4.0),
    gl:'begin'(?GL_POINTS),
    Draw = fun(P = {X, Y, Z}) ->
		   C = {to_comp(X), to_comp(Y), to_comp(Z)},
		   gl:color3fv(C),
		   gl:vertex3fv(P)
	   end,
    [Draw(P) || P <- Points],
    gl:'end'().

%% su: signed unit [-1.0 .. +1.0]
is_su(V) when is_float(V), V >= -1.0 andalso V =< 1.0 ->
    true;
is_su(V) when is_float(V) ->
    false;
is_su(_) ->
    throw(badarg).


%% uu: unsigned unit [0.0 .. +1.0]
is_uu(V) when is_float(V), V >= 0.0 andalso V =< 1.0 ->
    true;
is_uu(V) when is_float(V) ->
    false;
is_uu(_) ->
    throw(badarg).

%% su -> uu
su2uu(V) ->
    (V+1)/2.
%% uu -> su
uu2su(V) ->
    (V*2)-1.0.

%%
%% Testing
-define(N, 10).

test() ->
    Seq = fun(N) -> lists:seq(0, N) end,
    NSeq = fun(N) -> lists:seq(N, 0, -1) end,
    [{-1.0*norm(X),norm(Y),norm(Z)} || X<-Seq(?N), Y<-NSeq(?N), Z<-Seq(?N)].

norm(X) ->
    (X/?N)*2-1.


extract(WS, Bin, Type, Delay) ->
    extract(WS, Bin, Type, Delay, []).
extract(_WS, <<>>, _Type, _Delay, Points) ->
    [lists:reverse(Points), <<>>];
extract(WS, Bin, Type, Delay, Acc) ->
    <<Val:WS/bits, Rest/binary>> = Bin,
    %% io:format("Val= ~p~n", [Val]),
    %% NewBin = shift(WS, Delay, Bin),
    Value =  val(Val, WS, Type),
    extract(WS, Rest, Type, Delay, [Value|Acc]).

val(Val, WS, {signed, Order}) ->
    Max = umax(WS) bsr 1,
    Unsigned = binary:decode_unsigned(Val, Order),
    (Unsigned - Max) / -Max;
val(Val, WS, {unsigned, Order}) ->
    Max = umax(WS),
    Unsigned = binary:decode_unsigned(Val, Order),
    Unit = Unsigned/Max,
    uu2su(Unit).

%% umax(8) ->
%%     256;
umax(16) ->
    256*256.


%% add(_Values) ->
%%     todo.

%% add(Val, {T1,T2,T3,T4,T5,T6,T7} = Tuple) ->
%%     {T2,T3,T4,T5,T6,T7,Val}.

%% type(little, signed) ->
%%     little-signed-integer;
%% type(little, unsigned) ->
%%     little-unsigned-integer;
%% type(big, signed) ->
%%     big-signed-integer;
%% type(big, unsigned) ->
%%     big-unsigned-integer.

filter(3, Points, _MaxT) ->
    Points;
filter(4, Points, MaxT) ->
    [P || P = {_X, _Y, _Z, T,
	       _R, _G, _B} <- Points, T =< MaxT].
