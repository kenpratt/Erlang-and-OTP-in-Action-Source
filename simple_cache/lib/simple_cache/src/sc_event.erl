%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Provides a nice interface to the eventing system.
%%% @end
%%% @copyright (C) 2009, Erlware
%%%-------------------------------------------------------------------
-module(sc_event).

%% API
-export([start_link/0,
         event_name/0,
         lookup/1,
         create/2,
	 replace/2,
         delete/1,
         add_handler/2,
         add_sup_handler/2]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Starts the gen_event with the sc_event name.
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%%  Get the event name from the system.
%% @spec () -> EventName
%% @end
%%--------------------------------------------------------------------
event_name() ->
    ?SERVER.

%%--------------------------------------------------------------------
%% @doc
%%  Event fired when a new element is inserted
%% @spec (Key, Value) -> ok
%% @end
%%--------------------------------------------------------------------
create(Key, Value) ->
    gen_event:notify(?SERVER, {create, Key, Value}).


%%--------------------------------------------------------------------
%% @doc
%%  Event fired when a key is looked up
%% @spec (Key) -> ok
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    gen_event:notify(?SERVER, {lookup, Key}).

%%--------------------------------------------------------------------
%% @doc
%%  Event fired when a key is deleted
%% @spec (Key) -> ok
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
    gen_event:notify(?SERVER, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc
%%  Event fired when a key is replaced
%% @spec (Key, Value) -> ok
%% @end
%%--------------------------------------------------------------------
replace(Key, Value) ->
    gen_event:notify(?SERVER, {replace, Key, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Add a handler for this event system.
%% @spec (Handler, Args) -> ok
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Add a handler for this event system.
%% @spec (Handler, Args) -> ok
%% @end
%%--------------------------------------------------------------------
add_sup_handler(Handler, Args) ->
    gen_event:add_sup_handler(?SERVER, Handler, Args).
