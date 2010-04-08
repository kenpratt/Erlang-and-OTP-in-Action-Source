%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Stores a single cache element.
%%% @end
%%% Created : 14 Dec 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(sc_element).

-behaviour(gen_server).

%% API
-export([
	 start_link/1,
	 create/1,
	 fetch/1,
	 replace/2,
	 delete/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {value}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Value) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Value) ->
    gen_server:start_link(?MODULE, [Value], []).

%%--------------------------------------------------------------------
%% @doc
%%  Create an element in the cache
%%
%% @spec create(Value) -> void()
%% @end
%%--------------------------------------------------------------------
create(Value) ->
    sc_element_sup:start_child(Value).

%%--------------------------------------------------------------------
%% @doc
%% Fetch an element from the cache.
%%
%% @spec fetch(Pid) -> {ok, Value} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
fetch(Pid) ->
    gen_server:call(Pid, fetch).

%%--------------------------------------------------------------------
%% @doc
%% Replace an element in the cache.
%%
%% @spec replace(Pid, Value) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Delete an element from the cache.
%%
%% @spec delete(Pid) -> ok
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    gen_server:cast(Pid, delete).

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
init([Value]) ->
    {ok, #state{value = Value}}.

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
handle_call(fetch, _From, #state{value = Value} = State) ->
    {reply, {ok, Value}, State}.

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
handle_cast({replace, Value}, State) ->
    {noreply, State#state{value = Value}};
handle_cast(delete, State) ->
    {stop, normal, State}.

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
handle_info(_Info, State) ->
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
    sc_store:delete(self()),
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
