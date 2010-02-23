-define(NULL, {0.0, 0.0, 0.0}).
-define(TTL, 3.0). %% TODO f(FPS) ?
-define(MICRO, 1000000).
-define(WHITE, {255, 255, 255}).
-define(PTSIZE, 1.0).

-record(part, {ttl=?TTL*?MICRO, pos=?NULL, vel=?NULL, acc,
	       col=?WHITE, tid,
	       born=erlang:now(), last=erlang:now()}).
