%%%-------------------------------------------------------------------
%%% File    : ec_crystal.erl
%%% Author  : Olivier Girondel <olivier@biniou.info>
%%% Description : Packard-Takens 7d reconstruction
%%%-------------------------------------------------------------------
-module(ec_crystal).
-author('olivier@biniou.info').
-vsn("1.0").

-include("ec.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Module API
-export([points/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {rec, points=test()}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

points(Points) ->
    gen_server:cast(?SERVER, {points, Points}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),

    Env = ec_gui:get_env(),
    wx:set_env(Env),

    ec_gui:register(self()),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({points, Points}, State) ->
    {noreply, State#state{points=Points}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Pid, Ref, {draw, GL}}, #state{points=Ps} = State) ->
    wxGLCanvas:setCurrent(GL),
    %% io:format("[i] ~s:draw(~p)~n", [?MODULE, GL]),
    Res = draw(Ps),
    Pid ! {Ref, Res},
    {noreply, State};

handle_info(_Info, State) ->
    ?D_UNHANDLED(_Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?D_TERMINATE(_Reason),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% rescale dans [0.0..4.0] pour du glColor3fv
%% ouiiiii le max c'est 1.0 mais on estime que les composantes sont au
%% 25% de l'espace possible
to_comp(Val) ->
    abs(Val). %% * 4.0.


draw([]) ->
    ok;
draw(Points) ->
    %% gl:lineWidth(2.0),
    gl:pointSize(2.0),
    gl:'begin'(?GL_POINTS), %% LINE_STRIP),
    Draw = fun(P = {X, Y, Z}) ->
		   C = {to_comp(X), to_comp(Y), to_comp(Z)},
		   gl:color3fv(C),
		   gl:vertex3fv(P)
	   end,
    [Draw(P) || P <- Points],
    gl:'end'().


%%
%% Testing
-define(N, 10).

test() ->
    Seq = fun(N) -> lists:seq(0, N) end,
    NSeq = fun(N) -> lists:seq(N, 0, -1) end,
    [{-1.0*norm(X),norm(Y),norm(Z)} || X<-Seq(?N), Y<-NSeq(?N), Z<-Seq(?N)].

norm(X) ->
    (X/?N)*2-1.
