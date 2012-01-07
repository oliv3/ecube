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

    [VolSrv, TexSrv, Demo];
modules(_OtherDemo) ->
    [].

-define(DEMO, biniou).

init([]) ->
    CfgSrv = {ec_cf, {ec_cf, start_link, []},
	      permanent, brutal_kill, worker, [ec_cf]},

    GUI = {ec_gui, {ec_gui, start_link, []},
	   permanent, ?TIMEOUT, worker, [ec_gui]},

    OSD = {ec_osd, {ec_osd, start_link, []},
	   permanent, ?TIMEOUT, worker, [ec_osd]},

%%    M3D = {ec_m3d, {ec_m3d, start_link, []},
%%	   permanent, brutal_kill, worker, [ec_m3d]},

  %%  PCAP = {ec_pcap, {ec_pcap, start_link, []},
%%	    permanent, ?TIMEOUT, worker, [ec_pcap]},

  %%  PT3D = {ec_pt3d, {ec_pt3d, start_link, []},
%%	    permanent, ?TIMEOUT, worker, [ec_pt3d]},

  %%  PS = {ec_ps, {ec_ps, start_link, []},
%%	  permanent, ?TIMEOUT, worker, [ec_ps]},

    Mods = modules(?DEMO),

    {ok, {{one_for_one, 10, 1}, [CfgSrv, GUI, OSD] ++ Mods}}.
