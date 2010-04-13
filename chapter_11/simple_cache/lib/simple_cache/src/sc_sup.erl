%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  A simple supervisor for the elements in the cache.
%%% @end
%%% Created : 11 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(sc_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0,
	 start_child/1
	]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Start a child process, an sc_element.
%%
%% @spec start_child(Value) -> void()
%% @end
%%--------------------------------------------------------------------
start_child(Value) ->
    supervisor:start_child(?SERVER, [Value]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    ElementSup = {sc_element_sup, {sc_element_sup, start_link, []},
		  Restart, Shutdown, supervisor, [sc_element]},

    Event = {sc_event, {sc_event, start_link, []},
	     Restart, Shutdown, Type, [sc_event]},

    {ok, {SupFlags, [Event, ElementSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
