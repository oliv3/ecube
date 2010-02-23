%%%-------------------------------------------------------------------
%%% File    : ec_m3d.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : 3D-Mandelbrot plugin (aka "Mandel Bulb")
%%%-------------------------------------------------------------------
-module(ec_m3d).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

%% API
-export([start_link/0]).

%% Plugin API
-export([collect/0, launcher/1, compute/2]).

%% Callbacks
-export([init/1, system_continue/3]).

-define(SERVER, ?MODULE).

-define(RES, 100). %% grid resolution
-define(LAUNCHER_TIMEOUT, 10). %% One new point calculation asked every 10ms
-define(PTSIZE, 5.0).
-define(CMAP, "/usr/share/biniou/colormaps/VOLCANO2.map.bin").
%% -define(APS, {255, 0, 0}).

%% -define(WHITE, {255, 255, 255}). %% TEMP debug, TODO version RGBA

-record(state, {collector, cmap}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).


%%====================================================================
%% Callbacks
%%====================================================================
init(Parent) ->
    %% Delete lists on terminate + register_lists in GUI
    %% process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    
    Self = self(),
    ?D_REGISTER(?SERVER, Self), %% not needed
    ec_gui:register(Self),
    %% Cmap = load_cmap(?CMAP),
    Collector = spawn(?MODULE, collect, []),
    proc_lib:init_ack(Parent, {ok, Self}),
    Debug = sys:debug_options([]),
    _Launcher = spawn(?MODULE, launcher, [Collector]),
    loop(Parent, Debug, #state{collector=Collector}).


loop(Parent, Debug, #state{collector=Collector} = State) ->
    receive
	{Pid, Ref, {draw, GL}} ->
	    wxGLCanvas:setCurrent(GL),
	    draw(Collector),
	    Pid ! {Ref, ok},
	    loop(Parent, Debug, State);
	
	{system, From, Request} ->
	    ?D_F("code v1 system message(1): From ~p Request: ~p~n", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
	
	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end.


system_continue(Parent, Debug, State) ->
    ?D_F("code v1 system_continue(~p, ~p, ~p)~n", [Parent, Debug, State]),
    loop(Parent, Debug, State).


draw(CollectorPid) ->
    Ref = make_ref(),
    CollectorPid ! {self(), Ref, get},
    receive
	{Ref, LastPoint, List} -> %% later, a GLlist ?
	    %% ?D_F("LastPoint= ~p~n", [LastPoint]),
	    lines(LastPoint),
	    %% ?D_F("List= ~p~n", [List]),
	    render(List)
    end.


render([]) ->
    ok;
render(List) ->
    gl:pointSize(?PTSIZE),
    gl:'begin'(?GL_POINTS),
    render2(List),
    gl:'end'().

render2([]) ->
    ok;
render2([{Pos, Col}|Points]) ->
    %% TODO version RGBA
    gl:color3ubv(Col),
    gl:vertex3fv(Pos),
    render2(Points).


collect() ->
    collector(undefined, []).

collector(LastPoint, List) ->
    receive
	%% {point, {Pos, Col} = Point} ->
	{point, {NewLast, _Col} = Point} ->
	    collector(NewLast, [Point|List]);
	
	{nok, Point} = _M ->
	    %% ?D_F("nok: ~p", [_M]),
	    collector(Point, List);
	
	{Pid, Ref, get} ->
	    Pid ! {Ref, LastPoint, List},
	    collector(LastPoint, List)
    end.


seq(N) ->
    L = lists:seq(0, N-1),
    shuffle(L).


launcher(Collector) ->
    F = fun(Point) ->
		spawn(?MODULE, compute, [Collector, Point]),
		timer:sleep(?LAUNCHER_TIMEOUT)
	end,
    [F({X, Y, Z}) || X <- seq(?RES), Y <- seq(?RES), Z <- seq(?RES)].


%% -define(XMIN, -2.5).
%% -define(XMAX, +1.5).
%% -define(YMIN, -2.0).
%% -define(YMAX, +2.0).
%% -define(ZMIN, -2.0).
%% -define(ZMAX, +2.0).

-define(XMIN, -1.5).
-define(XMAX, +0.5).
-define(YMIN, -1.0).
-define(YMAX, +1.0).
-define(ZMIN, -1.0).
-define(ZMAX, +1.0).

compute(Collector, {X, Y, Z} = _Point) ->
    MX = float(X) / (?RES-1) * 2.0 - 1.0,
    MY = float(Y) / (?RES-1) * 2.0 - 1.0,
    MZ = float(Z) / (?RES-1) * 2.0 - 1.0,
    RPoint = {MX, MY, MZ},
    
    MPoint = {
      (float(X) / (?RES-1)) * (?XMAX-?XMIN) + ?XMIN,
      (float(Y) / (?RES-1)) * (?YMAX-?YMIN) + ?YMIN,
      (float(Z) / (?RES-1)) * (?ZMAX-?ZMIN) + ?ZMIN
     },

    Iter = m3d:iter(MPoint),
    if
	Iter == 255 ->
	    %% ?D_F("Iter: ~p~n", [Iter]),
	    %% krkrkrk Cmap sert plus a rien
	    Collector ! {point, RPoint};
	
	true ->
	    ok %% Collector ! {nok, RPoint}
    end.


%% load_cmap(Cmap) ->
%%     {ok, Bin} = file:read_file(Cmap),
%%     fix_cmap(list_to_tuple(parse(Bin))).

%% parse(Bin) ->
%%     parse(Bin, []).
%% parse(<<>>, Acc) ->
%%     Acc;
%% parse(<<R,G,B,_,Rest/binary>>, Acc) ->
%%     parse(Rest, [{R,G,B}|Acc]).


%% fix_cmap(T) ->
%%     T1 = setelement(1, T, {255, 0, 0}),
%%     setelement(256, T1, {0, 0, 255}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% shuffle(List1) -> List2
%% Takes a list and randomly shuffles it. Relies on random:uniform
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffle(List) ->
%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.


%% WIP
%% update_list(CallList, NewPoints) ->
    
lines(undefined) ->
    ok;

%% lines({Point, _Col}) ->
%%     lines(Point);
lines({X, Y, Z}) ->
    gl:color3ub(255, 0, 255),
    gl:pointSize(?PTSIZE),
    gl:'begin'(?GL_LINES),
    gl:vertex3f(-1.0, Y, Z),
    gl:vertex3f(+1.0, Y, Z),
    gl:vertex3f(X, -1.0, Z),
    gl:vertex3f(X, +1.0, Z),
    gl:vertex3f(X, Y, -1.0),
    gl:vertex3f(X, Y, +1.0),
    gl:'end'().
