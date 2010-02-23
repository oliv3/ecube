%%%-------------------------------------------------------------------
%%% File    : ec_vol.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Volume server
%%%-------------------------------------------------------------------
-module(ec_vol).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

-behaviour(gen_server).

%% gen_server API
-export([start_link/0]).

%% module API
-export([get/0, set/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

%%====================================================================
%% gen_server API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get() ->
    gen_server:call(?SERVER, get).


set(Volume) ->
    gen_server:cast(?SERVER, {set, Volume}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {ok, ?DEFAULT_VOLUME}.


handle_call(get, _From, State) ->
    {reply, State, State}.


handle_cast({set, Volume}, _State) ->
    {noreply, Volume}.
