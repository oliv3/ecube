%%%-------------------------------------------------------------------
%%% File    : ec_cf.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Keeps 3d parameters and options
%%%-------------------------------------------------------------------
-module(ec_cf).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([fov/0, dec_fov/0, inc_fov/0]).          %% FOV 
-export([rot/0, rot/1, reset_rot/0, no_spin/0]). %% Rotations
-export([opt/1, toggle/1, set/1, unset/1]).      %% Options

%% Default active options
%% -define(OPTIONS, [?O_SPIN, ?O_EDGES, ?O_MUTE, ?O_OSD]).
-define(OPTIONS, [?O_MUTE]).

-define(NULL_ROT,    {0, 0, 0}).
-define(DEFAULT_ROT, {30, 56, 0}).
-define(DEFAULT_FOV, 50).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3]).

-record(state, {rot=?DEFAULT_ROT,
		fov=?DEFAULT_FOV,
		opts=sets:from_list(?OPTIONS)}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


fov() ->
    gen_server:call(?SERVER, fov).


dec_fov() ->
    gen_server:cast(?SERVER, dec_fov).


inc_fov() ->
    gen_server:cast(?SERVER, inc_fov).


rot() ->
    gen_server:call(?SERVER, rot).


rot(R) ->
    gen_server:cast(?SERVER, {rot, R}).


reset_rot() ->
    gen_server:cast(?SERVER, reset_rot).


no_spin() ->
    gen_server:cast(?SERVER, no_spin).


opt(Option) ->
    gen_server:call(?SERVER, {opt, Option}).


toggle(Option) ->
    gen_server:cast(?SERVER, {toggle, Option}).


set(Option) ->
    gen_server:cast(?SERVER, {set, Option}).


unset(Option) ->
    gen_server:cast(?SERVER, {unset, Option}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {ok, #state{}}.


handle_call(fov, _From, State) ->
    {reply, State#state.fov, State};

handle_call(rot, _From, State) ->
    {reply, State#state.rot, State};

handle_call({opt, Option}, _From, #state{opts=Opts} = State) ->
    Reply = sets:is_element(Option, Opts),
    {reply, Reply, State}.


handle_cast(dec_fov, #state{fov=Fov} = State) ->
    {noreply, State#state{fov=Fov-1}};

handle_cast(inc_fov, #state{fov=Fov} = State) ->
    {noreply, State#state{fov=Fov+1}};

handle_cast({rot, R}, State) ->
    {noreply, State#state{rot=R}};

handle_cast(reset_rot, State) ->
    {noreply, State#state{rot=?NULL_ROT}};

handle_cast(no_spin, State) ->
    Opts = State#state.opts,
    NOpts = sets:del_element(?O_SPIN, Opts),
    {noreply, State#state{opts=NOpts}};

handle_cast({toggle, Option}, #state{opts=Opts} = State) ->
    NOpts = case sets:is_element(Option, Opts) of
		true ->
		    sets:del_element(Option, Opts);
		false ->
		    sets:add_element(Option, Opts)
	    end,
    {noreply, State#state{opts=NOpts}};

handle_cast({set, Option}, State) ->
    NewState = set2(Option, State),
    {noreply, NewState};

handle_cast({unset, Option}, State) ->
    NewState = unset2(Option, State),
    {noreply, NewState}.


code_change(?V1 = _OldVsn, State, _Extra) ->
    ?D_F("code_change from ~p (~p, ~p)", [_OldVsn, State, _Extra]),
    NewState0 = set2(?O_EDGES, State),
    NewState1 = set2(?O_TEXT, NewState0),
    NewState2 = unset2(?O_SPIN, NewState1),
    NewState3 = set2(?O_MUTE, NewState2),
    ?D_F("changed state to ~p", [NewState3]),
    {ok, NewState3};

code_change({down, ?V1} = _OldVsn, State, _Extra) ->
    ?D_F("code_change to ~p (~p, ~p)", [_OldVsn, State, _Extra]),
    NewState0 = unset2(?O_EDGES, State),
    NewState1 = unset2(?O_TEXT, NewState0),
    NewState2 = set2(?O_SPIN, NewState1),
    NewState3 = unset2(?O_MUTE, NewState2),
    ?D_F("changed state to ~p", [NewState3]),
    {ok, NewState3}.


set2(Option, #state{opts=Opts} = State) ->
    NOpts = sets:add_element(Option, Opts),
    State#state{opts=NOpts}.


unset2(Option, #state{opts=Opts} = State) ->
    NOpts = sets:del_element(Option, Opts),
    State#state{opts=NOpts}.
