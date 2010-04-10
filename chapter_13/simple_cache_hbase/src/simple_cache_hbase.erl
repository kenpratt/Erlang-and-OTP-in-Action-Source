%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@cyberlync-laptop>
%%% @copyright (C) 2009, PEAK6 LP
%%% @doc
%%%
%%% @end
%%% Created : 16 Sep 2009 by Eric Merritt <cyberlync@cyberlync-laptop>
%%%-------------------------------------------------------------------
-module(simple_cache_hbase).

%% API
-export([put, get]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
put(Node, Key, Value) ->
    Ref = make_ref(),
    {hbase_server, Node} ! {put, self(), Ref, Key, term_to_binary(Value)},
    receive
	{put_result, Ref, ok} ->
	    ok
    after 1000 ->
	    error
    end.

get(Node, Key) ->
    Ref = make_ref(),
    {hbase_server, Node} ! {get, self(), Ref, Key},
    receive
	{get_result, Ref, Value} ->
	    {ok, Value}
    after 1000 ->
	    error
    end.

delete(Node, Key) ->
    Ref = make_ref(),
    {hbase_server, Node} ! {delete, self(), Ref, Key},
    receive
	{delete_result, Ref, ok} ->
	    ok
    after 1000 ->
	    error
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
