%%
%% Debugging
%%
-ifdef(d3bug).
-define(D_LOG(Fmt, Args),
	error_logger:info_msg(Fmt, Args)).
-define(D_TERMINATE(Reason),
	case is_list(Reason) of
	    true ->
		?D_LOG("~p ~s: terminating with reason: ~s~n", [self(), ?MODULE, Reason]);
	    false ->
		?D_LOG("~p ~s: terminating with reason: ~p~n", [self(), ?MODULE, Reason])
	end).
-define(D_F(Format, Args),
	?D_LOG("~p ~s: " ++ Format, [self(), ?MODULE] ++ Args)).
-define(D_REGISTER(Alias, Pid),
	?D_LOG("~p ~s: registering ~p as ~p~n", [self(), ?MODULE, Pid, Alias]),
	register(Alias, Pid)).
-define(D_UNHANDLED(Msg),
        error_logger:error_msg("~p ~s: unhandled message: ~p~n", [self(), ?MODULE, Msg])).
-define(D_YOUPI,
	?D_LOG("~p ~s: Youpi line ~p~n", [self(), ?MODULE, ?LINE])).
-else.
-define(D_TERMINATE(_Reason),   ok).
-define(D_F(_F, _A),            ok).
-define(D_REGISTER(Alias, Pid), register(Alias, Pid)).
-define(D_UNHANDLED(_Msg),      ok).
-define(D_YOUPI,                ok).
-endif.
