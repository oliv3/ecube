%%%-------------------------------------------------------------------
%%% File    : ec_sup.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Top-level supervisor
%%%-------------------------------------------------------------------
-module(ec_sup).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

-behaviour(supervisor).

%% Supervisor API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


modules(biniou) ->
    VolSrv = {ec_vol, {ec_vol, start_link, []},
	      permanent, brutal_kill, worker, [ec_vol]},

    TexSrv = {ec_tex, {ec_tex, start_link, []},
	      permanent, ?TIMEOUT, worker, [ec_tex]},

    Demo = {ec_demo, {ec_demo, start_link, []},
	    permanent, ?TIMEOUT, worker, [ec_demo]},

    OSD = {ec_osd, {ec_osd, start_link, []},
	   permanent, ?TIMEOUT, worker, [ec_osd]},

    [VolSrv, TexSrv, Demo, OSD];
modules(base) ->
    Base = {ec_base, {ec_base, start_link, []},
	    permanent, ?TIMEOUT, worker, [ec_base]},
    [Base];
modules(crystal) ->
    CRYSTAL = {ec_crystal, {ec_crystal, start_link, []},
	    permanent, ?TIMEOUT, worker, [ec_crystal]},
    [modules(base), CRYSTAL].


%% -define(DEMO, biniou).
%% -define(DEMO, base).
-define(DEMO, crystal).

init([]) ->
    CfgSrv = {ec_cf, {ec_cf, start_link, []},
	      permanent, brutal_kill, worker, [ec_cf]},

    GUI = {ec_gui, {ec_gui, start_link, []},
	   permanent, ?TIMEOUT, worker, [ec_gui]},

    %% PS = {ec_ps, {ec_ps, start_link, []},
    %%	  permanent, ?TIMEOUT, worker, [ec_ps]},

    Mods = modules(?DEMO),
    Children = lists:flatten([CfgSrv, GUI, Mods]),
    %% io:format("Children: ~p~n", [Children]),

    {ok, {{one_for_one, 10, 1}, Children}}.

