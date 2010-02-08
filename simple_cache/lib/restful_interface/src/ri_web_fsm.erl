%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  This is a skeleton simple little very basic webserver.
%%%  - insert into the cache is done with PUT
%%%    example: curl -T <filename> http://localhost:1156/key 
%%%  - delete is done with DELETE
%%%  - lookup is done with a GET
%%%  and that is it. 
%%% @end
%%% Created : 18 Jul 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(ri_web_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2, packet/2]).

-export([http_message/3, http_message/1]).

%% gen_fsm callbacks
-export([
	 init/1,
	 state_name/3, 
	 handle_event/3, 
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

%% States
-export([
	 build_message_head/2, 
	 build_message_body/2,
	 reply/2
	]).

-include("eunit.hrl").

-record(state, {socket_manager, head = [<<>>], body = <<>>, content_length, callback}).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link(CallBack::atom(), SocketManager::pid()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(CallBack, SocketManager) ->
    gen_fsm:start_link(?MODULE, [SocketManager, CallBack], []).

%%--------------------------------------------------------------------
%% @doc create a new packet event of data has been received
%%
%% @spec packet(FSMPid, Packet) -> ok
%% @end
%%--------------------------------------------------------------------
packet(FSMPid, Packet) ->
    gen_fsm:send_event(FSMPid, {packet, Packet}).

%%%===================================================================
%%% container helper functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc helper function for creating a very minimally specified
%%      http message
%% @spec (Code, Headers, Body) -> ok
%% @end
%%--------------------------------------------------------------------
http_message(Code, Headers, Body) when is_list(Body) ->
    http_message(Code, Headers, list_to_binary(Body));
http_message(Code, Headers, Body) ->
    ["HTTP/1.1 ", code_to_binary(Code), "\r\n",
     format_headers(Headers),
     "Content-Length: ", integer_to_list(size(Body)), 
     "\r\n\r\n",
     Body].

%% @spec (Code) -> ok
%% @equiv http_message(Code, [{"Content-Type", "text/html"}], "") -> ok
http_message(Code) ->
    http_message(Code, [{"Content-Type", "text/html"}], "").

format_headers([{Header, Value}|T]) ->
    [Header, ": ", Value, "\r\n"|format_headers(T)];
format_headers([]) ->
    [].

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([SocketManager, CallBack]) ->
    error_logger:info_msg("ri_web_fsm:init/1~n"),
    {ok, build_message_head, #state{socket_manager = SocketManager, callback=CallBack}}.

%%% All states below here - they are accept, build_message_head,
%%% continue, build packet_body and reply

build_message_head({packet, Packet}, #state{socket_manager = SM, head = Head} = State) -> 
    NewHead = decode_initial_request_line_and_header(Head, Packet),
    case NewHead of
	[http_eoh|Headers] ->
	    CL = list_to_integer(header_value_search('Content-Length', Headers, "0")),
	    NewState = State#state{head = lists:reverse(Headers), % put headers in recieved order
				   content_length = CL},
	    send_continue(SM, Headers),
	    case CL of
		0 ->
		    {next_state, reply, NewState, 0};
		CL ->
		    {next_state, build_message_body, NewState}
	    end;
	_ ->
	    NewState = State#state{head = NewHead},
	    {next_state, build_message_head, NewState}
    end.


build_message_body({packet, Packet}, State) -> 
    #state{body           = Body,
	   content_length = ContentLength} = State,
    NewBody  = list_to_binary([Body, Packet]),
    NewState = State#state{body = NewBody},

    case ContentLength - byte_size(NewBody) of
	0 ->
	    {next_state, reply, NewState, 0};
	ContentLeftOver when ContentLeftOver > 0 ->
	    {next_state, build_message_body, NewState}
    end.

reply(timeout, State) -> 
    #state{socket_manager = SocketManager,
	   head           = Head,
	   body           = Body} = State,
    Reply = handle_message(Head, Body, State#state.callback),
    ri_web_socket:send(SocketManager, Reply),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _StateName, _State) ->
    ok;
terminate(_Reason, _StateName, #state{socket_manager = SocketManager}) ->
    Err = http_message(500, [{"Content-Type", "text/html"}], "500 Internal Server Error"),
    ri_web_socket:send(SocketManager, Err),
    ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_message([{http_request, 'GET', _, _} = InitialRequestLine|Head], Body, CallBack) ->
    CallBack:get(InitialRequestLine, Head, Body);
handle_message([{http_request, 'POST', _, _} = InitialRequestLine|Head], Body, CallBack) ->
    CallBack:post(InitialRequestLine, Head, Body);
handle_message([{http_request,'PUT',_,_} = InitialRequestLine|Head], Body, CallBack) ->
    CallBack:put(InitialRequestLine, Head, Body);
handle_message([{http_request, 'DELETE', _, _} = InitialRequestLine|Head], Body, CallBack) ->
    CallBack:delete(InitialRequestLine, Head, Body);
handle_message([{http_request, 'HEAD', _, _} = InitialRequestLine|Head], Body, CallBack) ->
    CallBack:head(InitialRequestLine, Head, Body);
handle_message([{http_request, 'OPTIONS', _, _} = InitialRequestLine|Head], Body, CallBack) ->
    CallBack:options(InitialRequestLine, Head, Body).


decode_initial_request_line_and_header([Unparsed], Packet) ->
    decode_initial_request_line(list_to_binary([Unparsed, Packet]));
decode_initial_request_line_and_header([Unparsed|T], Packet) ->
    decode_header([list_to_binary([Unparsed, Packet])|T]).

decode_initial_request_line(Packet) ->
    case erlang:decode_packet(http, Packet, []) of
	{more, _} ->
	    [Packet];
	{ok, IRLine, Rest} ->
	    decode_header([Rest, IRLine]);
	Error ->
	    throw({bad_initial_request_line, Error})
    end.

decode_header([Unparsed|Parsed] = Headers) ->
    case erlang:decode_packet(httph, Unparsed, []) of
	{ok, http_eoh, <<>>} ->
	    [http_eoh|Parsed];
	{more, _} ->
	    Headers;
	{ok, MoreParsed, Rest} ->
	    decode_header([Rest, MoreParsed|Parsed]);
	{error, Reason} ->
	    throw({bad_header, Reason})
    end.


header_value_search(Key, List, Default) ->
    case lists:keysearch(Key, 3, List) of
	{value, {_, _, _, _, CL}} -> CL;
	false                     -> Default
    end.

%% @private
%% @doc send a 100 continue packet if the client expects it
%% @end
send_continue(SocketManager, Headers) ->
    case lists:keymember("100-continue", 5, Headers) of
	true  -> ri_web_socket:send(SocketManager, http_message(100));
	false -> ok
    end.

%% @private
%% @doc Given a number of a standard HTTP response code, return
%% a binary (string) of the number and name.
%%
%% Example:
%% ```code_to_binary(404) => <<"404 Not Found">>
%% '''
%%
%% The supported status codes are taken from:
%%   ["http://en.wikipedia.org/wiki/List_of_HTTP_status_codes"]
%%
%% @spec (integer()) -> binary()
code_to_binary(100) -> <<"100 Continue">>;
code_to_binary(101) -> <<"101 Switching Protocols">>;
code_to_binary(102) -> <<"102 Processing">>;
code_to_binary(200) -> <<"200 OK">>;
code_to_binary(201) -> <<"201 Created">>;
code_to_binary(202) -> <<"202 Accepted">>;
code_to_binary(203) -> <<"203 Non-Authoritative Information">>;
code_to_binary(204) -> <<"204 No Content">>;
code_to_binary(205) -> <<"205 Reset Content">>;
code_to_binary(206) -> <<"206 Partial Content">>;
code_to_binary(207) -> <<"207 Multi-Status">>;
code_to_binary(300) -> <<"300 Multiple Choices">>;
code_to_binary(301) -> <<"301 Moved Permanently">>;
code_to_binary(302) -> <<"302 Found">>;
code_to_binary(303) -> <<"303 See Other">>;
code_to_binary(304) -> <<"304 Not Modified">>;
code_to_binary(305) -> <<"305 Use Proxy">>;
code_to_binary(307) -> <<"307 Temporary Redirect">>;
code_to_binary(400) -> <<"400 Bad Request">>;
code_to_binary(401) -> <<"401 Unauthorized">>;
code_to_binary(402) -> <<"402 Payment Required">>;
code_to_binary(403) -> <<"403 Forbidden">>;
code_to_binary(404) -> <<"404 Not Found">>;
code_to_binary(405) -> <<"405 Method Not Allowed">>;
code_to_binary(406) -> <<"406 Not Acceptable">>;
code_to_binary(407) -> <<"407 Proxy Authentication Required">>;
code_to_binary(408) -> <<"408 Request Time-out">>;
code_to_binary(409) -> <<"409 Conflict">>;
code_to_binary(410) -> <<"410 Gone">>;
code_to_binary(411) -> <<"411 Length Required">>;
code_to_binary(412) -> <<"412 Precondition Failed">>;
code_to_binary(413) -> <<"413 Request Entity Too Large">>;
code_to_binary(414) -> <<"414 Request-URI Too Large">>;
code_to_binary(415) -> <<"415 Unsupported Media Type">>;
code_to_binary(416) -> <<"416 Requested range not satisfiable">>;
code_to_binary(417) -> <<"417 Expectation Failed">>;
code_to_binary(421) ->
    <<"421 There are too many connections from your internet address">>;
code_to_binary(422) -> <<"422 Unprocessable Entity">>;
code_to_binary(423) -> <<"423 Locked">>;
code_to_binary(424) -> <<"424 Failed Dependency">>;
code_to_binary(425) -> <<"425 Unordered Collection">>;
code_to_binary(426) -> <<"426 Upgrade Required">>;
code_to_binary(449) -> <<"449 Retry With">>;
code_to_binary(500) -> <<"500 Internal Server Error">>;
code_to_binary(501) -> <<"501 Not Implemented">>;
code_to_binary(502) -> <<"502 Bad Gateway">>;
code_to_binary(503) -> <<"503 Service Unavailable">>;
code_to_binary(504) -> <<"504 Gateway Time-out">>;
code_to_binary(505) -> <<"505 HTTP Version not supported">>;
code_to_binary(506) -> <<"506 Variant Also Negotiates">>;
code_to_binary(507) -> <<"507 Insufficient Storage">>;
code_to_binary(509) -> <<"509 Bandwidth Limit Exceeded">>;
code_to_binary(510) -> <<"510 Not Extended">>;
code_to_binary(Code) -> Code.
