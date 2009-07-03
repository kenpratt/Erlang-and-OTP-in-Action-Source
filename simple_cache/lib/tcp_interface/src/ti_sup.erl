%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  simple one for one supervisor for handling the tcp server.
%%% @end
%%% Created : 13 May 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(ti_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Port) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @equiv start_link(Port) 
start_link() ->
    case application:get_env(tcp_rpc, port) of
	{ok, Port} -> start_link(Port);
	undefined  -> start_link(?DEFAULT_PORT)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Start a child process, an sc_connection.
%%
%% @spec start_child() -> void()
%% @end
%%--------------------------------------------------------------------
start_child() ->
    supervisor:start_child(?SERVER, []).

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
init([Port]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,
    
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),

    AChild = {ti_server, {ti_server, start_link, [LSock]},
	      Restart, Shutdown, Type, [ti_server]},
    
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
