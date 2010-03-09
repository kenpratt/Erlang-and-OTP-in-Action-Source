%%%-------------------------------------------------------------------
%%% @doc
%%%  The callback module that handles all incoming requests at the
%%%  application level
%%% @end
%%%-------------------------------------------------------------------
-module(ri_web_fsm_impl).

%% API
-export([start_link/1]).

%% Callbacks
-export([get/3, put/3, delete/3, post/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a ri_web_fsm process 
%%
%% @spec start_link(SocketManager::pid()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(SocketManager) ->
    ri_web_fsm:start_link(?MODULE, SocketManager).

%%%===================================================================
%%% ri_web_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Handles http GET requests
%% @spec (InitialRequestLine, Head, Body) -> Response
%% @end
%%--------------------------------------------------------------------
get({_, _, {abs_path, [$/|Key]}, _}, _Head, _Body) ->
    case simple_cache:lookup(Key) of
	{ok, Value} ->
	    Headers = [{"Content-Type", "text/html"}],
	    ri_web_fsm:http_message(200, Headers, Value);
	{error, not_found} ->
	    ri_web_fsm:http_message(404)
    end.

%%--------------------------------------------------------------------
%% @doc Handles http PUT requests
%% @spec (InitialRequestLine, Head, Body) -> Response
%% @end
%%--------------------------------------------------------------------
put({_, _, {abs_path, [$/|Key]}, _}, _Head, Body) ->
    simple_cache:insert(Key, Body),
    ri_web_fsm:http_message(200).

%%--------------------------------------------------------------------
%% @doc Handles http DELETE requests
%% @spec (InitialRequestLine, Head, Body) -> Response
%% @end
%%--------------------------------------------------------------------
delete({_, _, {abs_path, [$/|Key]}, _}, _Head, _Body) ->
    simple_cache:delete(Key),
    ri_web_fsm:http_message(200).

%%--------------------------------------------------------------------
%% @doc Handles http POST requests
%% @spec (InitialRequestLine, Head, Body) -> Response
%% @end
%%--------------------------------------------------------------------
post({_, _, _, _}, _Head, _Body) ->
    ri_web_fsm:http_message(501).

%%%===================================================================
%%% Internal functions
%%%===================================================================
