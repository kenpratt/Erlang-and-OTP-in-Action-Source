%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  holds assorted storage functions for our cache
%%% @end
%%% Created : 11 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(sc_store).

%% API
-export([
         init/0,
         insert/2,
         delete/1,
         lookup/1
        ]).

-define(TABLE_ID, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This initializes the registrar.
%% @spec init() -> void()
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @doc Insert a key and pid.
%% @spec insert(Key, Pid) -> void()
%% @end
%%--------------------------------------------------------------------
insert(Key, Pid) when is_pid(Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

%%--------------------------------------------------------------------
%% @doc Find a pid given a key.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Pid}] -> {ok, Pid};
        []           -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Delete an element by pid from the registrar.
%% @spec delete(Pid) -> void()
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
