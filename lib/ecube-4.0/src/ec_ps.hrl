-define(NULL, {0.0, 0.0, 0.0}).
-define(TTL, 2.0). %% TODO f(FPS) ?
-define(MICRO, 1000000).
-define(WHITE, {255, 255, 255}).
-define(PTSIZE, 1.1).

-record(part, {ttl=?TTL*?MICRO, pos=?NULL, vel=?NULL,
	       col=?WHITE, tid,
	       born=now(), last=now()}).
