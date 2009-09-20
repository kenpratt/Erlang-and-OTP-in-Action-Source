%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  The main programmers API to the simple cache.
%%% @end
%%% Created : 11 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(simple_cache).

%% API
-export([
	 insert/2,
	 delete/1,
	 lookup/1
	]).

-define(NODE, "simple_cache_hbase").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc insert an element into the cache.
%% @spec insert(Key, Value) -> ok
%% where
%%  Key = term()
%%  Value = term()
%% @end
%%--------------------------------------------------------------------
insert(Key, Value) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_event:replace(Key, Value),
	    sc_element:replace(Pid, Value),
	    simple_cache_hbase:put(?NODE, Key, Value);
	{error, _Reason} ->
	    {ok, Pid} = sc_element:create(Value),
	    sc_store:insert(Key, Pid),
	    sc_event:create(Key, Value)
    end.

%%--------------------------------------------------------------------
%% @doc lookup an element in the cache.
%% @spec lookup(Key) -> {ok, Value} | {error, not_found}
%% where
%%  Key = term()
%%  Value = term()
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    sc_event:lookup(Key),
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:fetch(Pid);
	{error, not_found} ->
	    case simple_cache_hbase:get(?NODE, Key) of
		{ok, Value} ->
		    insert(Key, Value),
		    Value;
		_ ->
		    {error, not_found}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc delete an element into the cache.
%% @spec delete(Key) -> ok
%% where
%%  Key = term()
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
    sc_event:delete(Key),
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:delete(Pid),
	    simple_cache_hbase:delete(?NODE, Key);
	{error, _Reason} ->
	    ok
    end.
