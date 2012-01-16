%%%-------------------------------------------------------------------
%%% @author Olivier <olivier@biniou.info>
%%% @copyright (C) 2012, Olivier Girondel
%%% Created : 11 Jan 2012 by Olivier Girondel <olivier@biniou.info>
%%%-------------------------------------------------------------------
-module(ec_rec).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {port}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    gen_server:cast(?SERVER, {start}).

stop() ->
    gen_server:cast(?SERVER, {stop}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% handle_call(_Request, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.
handle_call(_Request, _From, State) ->
    {stop, internal_error, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({start}, #state{port=undefined} = State) ->
    Port = rec(),
    %% ec_crystal:format(signed, 16, little),
    %% ec_crystal:format(unsigned, 8, little),
    ec_crystal:format(signed, 16, little),
    ec_osd:message("[REC]", 5000),
    {noreply, State#state{port=Port}};

handle_cast({stop}, #state{port=Port} = State) ->
    port_close(Port),
    {noreply, State#state{port=undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, Bin}}, #state{port=Port} = State) ->
    %% io:format("~s: got ~p bytes~n", [?MODULE, size(Bin)]),
    ec_crystal:clear(),
    ec_crystal:feed(Bin),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rec() ->
    %% Cmd = "arecord -f cd 2> /dev/null",
    %% Cmd = "arecord -c2 -r4410 2> /dev/null",
    Cmd = "arecord -c2 -f S16_LE -r4410 2> /dev/null",
    open_port({spawn, Cmd}, [stream, binary]).
