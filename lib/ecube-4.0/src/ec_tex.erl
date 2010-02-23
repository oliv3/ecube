%%%-------------------------------------------------------------------
%%% File    : ec_tex.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Texture server running N binious
%%%-------------------------------------------------------------------
-module(ec_tex).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get/1]).

-export([process/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).


-record(biniou, {pid, frame}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get(N) ->
    gen_server:call(?SERVER, {get, N}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    N = ec:get_env(n),
    %% ?D_F("init: N= ~p~n", [N]),
    _BSize = ec:get_env(biniou_size),
    if
	N >= 1 andalso N =< 6 ->
	    ?D_F("running ~p binious at ~p~n", [N, _BSize]),
	    Binious = launch(N),
	    _Pids = [B#biniou.pid || B <- tuple_to_list(Binious)],
	    ?D_F("binious= ~p~n", [_Pids]),
	    {ok, Binious};
	true ->
	    {stop, {error, badarg}}
    end.


handle_call({get, FaceNo}, _From, Binious) ->
    Biniou = element(FaceNo, Binious),
    {Frame, Data} = ec_biniou:data(Biniou#biniou.pid),

    OldFrame = Biniou#biniou.frame,
    {Reply, NewBiniou} = if
			     Frame == OldFrame ->
				 {unchanged, Biniou};

			     true ->
				 {{data, Data}, Biniou#biniou{frame=Frame}}
			 end,
    NewBinious = setelement(FaceNo, Binious, NewBiniou),
    {reply, Reply, NewBinious}.


handle_info({'EXIT', Pid, _Reason}, State) ->
    ?D_F("got EXIT from ~p with reason: ~p~n", [Pid, _Reason]),
    NewState = restart(Pid, State),
    {noreply, NewState}.


terminate(_Reason, Binious) ->
    ?D_TERMINATE(_Reason),
    [ec_biniou:stop(B#biniou.pid) || B <- tuple_to_list(Binious)],
    ?D_F("stopped binious~n", []).


%%====================================================================
%% Internal functions
%%====================================================================
launch(N) ->
    launch(N, []).
launch(0, List) ->
    list_to_tuple(lists:reverse(List));
launch(N, Pids) ->
    Pid = start(N),
    launch(N-1, [#biniou{pid=Pid}|Pids]).


process(_N, undefined) ->
    undefined;
process(_N, <<Vsn:32/integer, Width:16/integer, Height:16/integer, Data/binary>>) when Vsn == 1 ->
    #img{w=Width, h=Height, data=Data};
process(N, <<Vsn:32/integer, V:16/integer, Width:16/integer, Height:16/integer, Data/binary>>) when Vsn == 2 ->
    set_volume(N, V),
    #img{w=Width, h=Height, data=Data};
process(N, <<Vsn:32/integer, V:8/integer, Width:16/integer, Height:16/integer,
	     PF:16/integer, PT:16/integer, Data/binary>>) when Vsn == 3 ->
    set_volume(N, V),
    #img{w=Width, h=Height, data=Data, pf=PF, pt=PT};
process(_N, _Other) ->
    ?D_F("process/2 got martian data, leaving undefined.~n", []),
    undefined.


set_volume(1, V) -> %% Set volume only from the first biniou
    Vol = 1.0 + V / 200.0,
    ec_vol:set(Vol);
set_volume(_, _) ->
    ok.


idx(Pid, List) ->
    idx(Pid, List, 1).
idx(Pid, [#biniou{pid=Pid}|_T], Pos) ->
    Pos;
idx(Pid, [_H|T], Pos) ->
    idx(Pid, T, Pos+1).


start(Id) ->
    spawn_link(ec_biniou, start, [Id]).


restart(Pid, Binious0) ->
    Binious1 = tuple_to_list(Binious0),
    Idx = idx(Pid, Binious1),
    NewPid = start(Idx),
    NewBiniou = #biniou{pid=NewPid},
    Binious2 = lists:keyreplace(Pid, 2, Binious1, NewBiniou),
    list_to_tuple(Binious2).
