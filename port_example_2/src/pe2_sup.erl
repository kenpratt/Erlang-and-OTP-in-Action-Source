%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@cyberlync-laptop>
%%% @copyright (C) 2009, PEAK6 LP
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2009 by Eric Merritt <cyberlync@cyberlync-laptop>
%%%-------------------------------------------------------------------
-module(pe2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    PortServer = {pe2_c_driver, {pe2_c_driver, start_link, []},
	      Restart, Shutdown, Type, [pe2_c_driver]},

    {ok, {SupFlags, [PortServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
