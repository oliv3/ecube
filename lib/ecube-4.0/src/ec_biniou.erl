-module(ec_biniou).
-author('olivier@biniou.info').
-vsn("3.0").

-include("ec.hrl").

-export([start/1, stop/1]).
-export([data/1]).

-define(STOP_CHAR, <<"S">>).

-record(state, {port, frame=0, data}).

-define(BINIOU, "Biniou").
-define(INPUT,  "pulseaudio").
-define(OUTPUT, "erlang").
-define(I2L(X), integer_to_list(X)).

%% Available Biniou input plugins
-define(INPUTS, ["alsa", "devurandom", "esound", "oss", "pulseaudio"]).


args(Id) ->
    Input = case ec:get_env(input) of
		undefined ->
		    ?INPUT;
		I ->
		    check_input(I)
	    end,
    ?D_F("input= ~s~n", [Input]),
    {W, H} = ec:get_env(biniou_size),
    lists:flatten([" -i ", Input, " -o ", ?OUTPUT
		   " -x ", ?I2L(W), " -y ", ?I2L(H),
		   " -m ", ?I2L(ec:get_env(biniou_fps)), " -r 2 ",
		   " --pidfile=/tmp/biniou" ++ ?I2L(Id),
		   " > /dev/null 2>&1"]).


start(Id) ->
    ?D_F("new biniou id: ~p~n", [Id]),
    process_flag(trap_exit, true),
    Cmd = os:find_executable(?BINIOU) ++ args(Id),
    Env = {env, [{"BINIOU_ERLANG_PROTO", "3"}]},
    %% can't redirect stdin/stdout with spawn_executable
    Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, binary, Env]),
    loop(#state{port=Port}).


data(Pid) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, data},
    receive
	{Ref, Result} ->
	    Result
    end.


stop(Pid) ->
    ?D_TERMINATE(pid_to_list(Pid) ++ " exiting"),
    Pid ! stop.


loop(#state{frame=Frame, port=Port} = State) ->
    receive
	{Port, {data, Data}} ->
	    loop(State#state{frame=Frame+1, data=Data})

    after 0 ->
	    receive
		{Pid, Ref, data} ->
		    Pid ! {Ref, {Frame, State#state.data}},
		    loop(State);

		{'EXIT', Port, _Reason} ->
		    ?D_F("~p exiting with reason: ~p~n", [Port, _Reason]);

		stop ->
		    stop_biniou(Port);

        	{Port, {data, Data}} ->
	            loop(State#state{frame=Frame+1, data=Data});

		_Other ->
                    stop_biniou(Port),
		    ?D_UNHANDLED(_Other)
	    end
    end.


stop_biniou(Port) ->
    port_command(Port, ?STOP_CHAR),
    port_close(Port).


%% is_input(A) when is_atom(A) ->
%%     is_input(atom_to_list(A));
%% is_input(I) when is_list(I) ->
%%     lists:member(I, ?INPUTS).


%% to_list(A) when is_atom(A) ->
%%     atom_to_list(A);
%% to_list(L) when is_list(L) ->
%%     L.


check_input(A) when is_atom(A) ->
    check_input(atom_to_list(A));
check_input(L) ->
    ?D_F("Check input ~p~n", [L]),
    case lists:member(L, ?INPUTS) of
	true ->
	    L;
	false ->
	    exit({error, bad_input})
    end.
