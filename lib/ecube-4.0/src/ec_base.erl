%%%-------------------------------------------------------------------
%%% File    : ec_base.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : OpenGL demo
%%%-------------------------------------------------------------------
-module(ec_base).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1, system_code_change/4, system_continue/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%%====================================================================
%% Callbacks
%%====================================================================
init(Parent) ->
    process_flag(trap_exit, true),
    Env = ec_gui:get_env(),
    wx:set_env(Env),
    Self = self(),
    ?D_REGISTER(?SERVER, Self), %% not needed
    ec_gui:register(Self),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, Self}),
    loop(Parent, Debug, #state{}).


%% loop(Parent, Debug, State) ->
%%     receive
%% 	{Pid, Ref, {draw, GL}} ->
%% 	    N = ec:get_env(n),
%%             wxGLCanvas:setCurrent(GL),
%% 	    ec_cube:cube(),
%% 	    Pid ! {Ref, ok},
%% 	    loop(Parent, Debug, State);

%%         {system, From, Request} ->
%% 	    ?D_F("code v3 system message(1): From ~p Request: ~p", [From, Request]),
%%             sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);

%% 	_Other ->
%% 	    ?D_UNHANDLED(_Other),
%% 	    loop(Parent, Debug, State)
%%     end;

loop(Parent, Debug, State) ->
    receive
	{Pid, Ref, {draw, GL}} ->
            wxGLCanvas:setCurrent(GL),
	    ec_cube:cube(),
	    Pid ! {Ref, ok},
	    loop(Parent, Debug, State);

	{'EXIT', _Pid, Reason} ->
	    ?D_F("got EXIT from ~p with reason: ~p", [_Pid, Reason]),
	    exit(Reason);

        {system, From, Request} ->
	    ?D_F("code v3 system message(2): From ~p Request: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end.


system_code_change(_State, _Module, ?V1 = _OldVsn, _Extra) ->
    ?D_F("code v3 system_code_change from ~p (~p, ~p, ~p)", [_OldVsn, _State, _Module, _Extra]),
    {ok, #state{}};
system_code_change(_State, _Module, {down, ?V1} = _OldVsn, _Extra) ->
    ?D_F("code v3 system_code_change to ~p (~p, ~p, ~p)", [_OldVsn, _State, _Module, _Extra]),
    {ok, undefined}.


system_continue(Parent, Debug, State) ->
    ?D_F("code v3 system_continue(~p, ~p, ~p)", [Parent, Debug, State]),
    loop(Parent, Debug, State).

%%====================================================================
%% Internal functions
%%====================================================================
