%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@cyberlync-laptop>
%%% @copyright (C) 2009, PEAK6 LP
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2009 by Eric Merritt <cyberlync@cyberlync-laptop>
%%%-------------------------------------------------------------------
-module(pe1_py_drv).

-behaviour(gen_server).

%% API
-export([start_link/0, send_msg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port}).

%%%===================================================================
%%% API
%%%===================================================================
send_msg(Msg) ->
    gen_server:cast(?SERVER, {msg, Msg}).

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
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Port = create_port(),
    {ok, #state{port=Port}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({msg, Msg}, State = #state{port=Port}) ->
    erlang:port_command(Port, Msg ++ "\n"),
    {noreply, State}.

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
handle_info({Port, {data, {_, Data}}}, State = #state{port=Port}) ->
    io:format("~p~n", [Data]),
    {noreply, State};
handle_info({Port, {exit_status, Status}}, #state{port=Port})
  when Status == 1 ->
    io:format("BAD SHUTDOWN:~p~n", [Status]),
    {noreply, #state{port=create_port()}};
handle_info({Port, {exit_status, Status}}, #state{port=Port})
  when Status == 0 ->
    io:format("GOOD SHUTDOWN:~p~n", [Status]),
    {noreply, #state{port=create_port()}}.

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
create_port() ->
    PrivDir = code:priv_dir(port_example_1),
    open_port({spawn, filename:join([PrivDir, "test.py"])},
	      [stream, {line, 1024}, exit_status]).

