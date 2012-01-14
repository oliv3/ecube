%%%-------------------------------------------------------------------
%%% @author Olivier Girondel <olivier@biniou.info>
%%% @copyright (C) 2012, Olivier Girondel
%%% Created : 13 Jan 2012 by Olivier Girondel <olivier@biniou.info>
%%%-------------------------------------------------------------------
-module(ec_crystal_test).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, ec_crystal).


is_su_test() ->
    true = ?MOD:is_su(0.0),
    true = ?MOD:is_su(1.0),
    true = ?MOD:is_su(-1.0),
    false = ?MOD:is_su(2.0),
    false = ?MOD:is_su(-2.0),
    {error, badarg} = ?MOD:is_su(foo).

is_uu_test() ->
    true = ?MOD:is_uu(0.0),
    true = ?MOD:is_uu(1.0),
    false = ?MOD:is_uu(-1.0),
    false = ?MOD:is_uu(2.0),
    {error, badarg} = ?MOD:is_uu(foo).
    
