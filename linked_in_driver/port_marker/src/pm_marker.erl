%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@Eric-Merritts-MacBook-Pro.local>
%%% @copyright (C) 2010, PEAK6 LP
%%% @doc
%%%
%%% @end
%%% Created : 14 Jan 2010 by Eric Merritt <cyberlync@Eric-Merritts-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(pm_marker).

-behaviour(gen_server).

%% API
-export([start_link/0, mark_image/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port, work_ids, cur_count, read_state, data_state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

mark_image(Msg) ->
    gen_server:call(?SERVER, {mark_image, Msg}).

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
    {ok, #state{port = create_port(),
		work_ids = [],
		read_state = initial,
		cur_count = 0}}.

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
handle_call({mark_image, Msg}, From, State = #state{port=Port,
						   work_ids=Ids,
						   cur_count=WorkId}) ->
    Len = size(Msg),
    NewWorkId = incr_work_id(WorkId),
    erlang:port_command(Port, <<NewWorkId:4/little-unsigned-integer-unit:8,
			       Len:4/little-unsigned-integer-unit:8,
			       Msg/binary>>),
    {noreply, State#state{cur_count = NewWorkId,
			  work_ids = [{NewWorkId, From} | Ids]}}.


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
handle_cast(_Request, State) ->
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
handle_info({Port, {exit_status, Status}}, State = #state{port=Port}) ->
    io_lib:format("Port exited with status ~p, restarting", [Status]),
    {noreply, State#state{port=create_port()}};

handle_info({_Port, {data, Data}},
	    State = #state{work_ids=Ids, read_state=initial}) ->
    <<WorkId:4/little-unsigned-integer-unit:8, Data1/binary>> = Data,
    <<Len:4/little-unsigned-integer-unit:8, Data2/binary>> = Data1,
    case size(Data2) < Len of
	false ->
	    {ok, Pid, NewIds} = get_work_id(WorkId, Ids, []),
	    gen_server:reply(Pid, Data2),
	    {noreply, State#state{work_ids=NewIds, read_state=initial}};
	true ->
	    {noreply, State#state{read_state=reading,
				  data_state = {WorkId, Len, size(Data2), [Data2]}}}
    end;

handle_info({_Port, {data, Data}},
	    State = #state{work_ids = Ids, read_state=reading,
			   data_state = InData}) ->
    {Id, Len, CurrentLen, OldData} = InData,
    case CurrentLen + size(Data) < Len of
	true ->
	    {ok, Pid, NewIds} = get_work_id(Id, Ids, []),
	    NewData = list_to_binary(lists:reverse([Data | OldData])),
	    gen_server:reply(Pid, NewData),
	    {noreply, State#state{work_ids=NewIds, read_state=initial,
				  data_state = none}};
	false ->
	    {noreply, State#state{data_state = {Id, Len, CurrentLen + size(Data),
						[Data | OldData]}}}
    end.


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
    PrivDir = code:priv_dir(port_marker),
    open_port({spawn, filename:join([PrivDir, "marker"])},
	      [stream,exit_status,binary]).

incr_work_id(Id) ->
    case Id >= 4294967294 of
	true ->
	    1;
	false ->
	    Id + 1
    end.


get_work_id(WorkId, [{WorkId, Pid} | T], Acc)->
    {ok, Pid, Acc ++ T};
get_work_id(WorkId, [H | T], Acc) ->
    get_work_id(WorkId, T, [H | Acc]);
get_work_id(_, [], Acc) ->
    {none, Acc}.


