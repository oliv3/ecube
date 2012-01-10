%%%-------------------------------------------------------------------
%%% File    : ec_app.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : wxBiniou application
%%%-------------------------------------------------------------------
-module(ec_app).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


start(_Type, _StartArgs) ->
    ec_sup:start_link().

stop(_State) ->
    init:stop().
