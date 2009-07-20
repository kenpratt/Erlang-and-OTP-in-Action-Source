%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  This is a skeleton simple little very basic webserver
%%% @end
%%% Created : 18 Jul 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(pi_web_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([
	 init/1,
	 state_name/3, 
	 handle_event/3, 
	 accept/2, 
	 continue/2, 
	 build_packet_head/2, 
	 build_packet_body/2,
	 reply/2,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4
	]).

-include("eunit.hrl").

-record(state, {lsock, socket, head = [<<>>], body = <<>>, content_length}).

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
%% @spec start_link(LSock) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_fsm:start_link(?MODULE, [LSock], []).

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
init([LSock]) ->
    {ok, accept, #state{lsock = LSock}, 0}.

%%% All states below here - they are accept, build_packet_head,
%%% continue, build packet_body and reply

accept(timeout, #state{lsock = LSock} = State) ->
    error_logger:info_msg("waiting to accept an incoming connection~n"),
    {ok, Socket} = gen_tcp:accept(LSock),
    error_logger:info_msg("connection received on socket ~p~n", [Socket]),
    pi_sup:start_child(),
    {next_state, build_packet_head, State#state{socket = Socket}, 0}.

build_packet_head(timeout, #state{socket = Socket, head = Head} = State) -> 
	case gen_tcp:recv(Socket, 0) of
	   {ok, Packet} -> 
		NewHead = decode_header(Head, Packet),
		case NewHead of
		    [http_eoh|Headers] ->
			CL = header_value_search('Content-Length', Headers, "0"),
			NewState = State#state{head = lists:reverse(Headers), % put headers in recieved order
					       content_length = list_to_integer(CL)},
			{next_state, go_to_continue_or_build_packet_body(Headers), NewState, 0};
		    _ ->
			NewState = State#state{head = NewHead},
			{next_state, build_packet_head, NewState, 0}
		end;
	    {error, closed} ->
		{stop, normal, State}
	end.


continue(timeout, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, create_http_message(100)),
    {next_state, build_packet_body, State, 0}.

build_packet_body(timeout, State) -> 
    #state{socket         = Socket,
	   body           = Body,
	   content_length = ContentLength} = State,

    case ContentLength - size(Body) of
	0 ->
	    {next_state, reply, State, 0};
	ContentLeftOver when ContentLeftOver > 0 ->
	    {ok, Packet} = gen_tcp:recv(Socket, ContentLeftOver),
	    NewState = State#state{body = list_to_binary([Body, Packet])},
	    {next_state, build_packet_body, NewState, 0}
    end.

reply(timeout, State) -> 
    #state{socket         = Socket,
	   head           = Headers,
	   body           = Body} = State,
    Reply = handle_message(Headers, Body),
    gen_tcp:send(Socket, Reply),
    NewState = State#state{head = [<<>>], body = <<>>, content_length = undefined},
    {next_state, build_packet_head, NewState, 0}.

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
terminate(normal, _StateName, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok;
terminate(_Reason, _StateName, #state{socket = Socket}) ->
    Err = create_http_message(500, "text/html", "500 Internal Server Error"),
    gen_tcp:send(Socket, Err),
    gen_tcp:close(Socket),
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

handle_message([{http_request, 'GET', {abs_path, [$/|Key]}, _}|_Headers], _Body) ->
    case simple_cache:lookup(Key) of
	{ok, Value}        -> create_http_message(200, "text/html", Value);
	{error, not_found} -> create_http_message(404)
    end;
handle_message([{http_request,'PUT',{abs_path, [$/|Key]},_}|_Headers], Body) ->
    simple_cache:insert(Key, Body),
    create_http_message(200);
handle_message([{http_request, 'DELETE', {abs_path, [$/|Key]}, _}|_Headers], _Body) ->
    simple_cache:delete(Key),
    create_http_message(200).

create_http_message(Code) ->
    create_http_message(Code, "text/html", "").

create_http_message(Code, ContentType, Body) when is_list(Body) ->
    create_http_message(Code, ContentType, list_to_binary(Body));
create_http_message(Code, ContentType, Body) ->
    ["HTTP/1.1 ", code_to_binary(Code), "\r\n"
     "Content-Type: ", ContentType, "\r\n"
     "Content-Length: ", integer_to_list(size(Body)), 
     "\r\n\r\n",
     Body].
%application/x-protobuf  ) ->

decode_header([Unparsed|T], Packet) ->
    decode_header([list_to_binary([Unparsed, Packet])|T]).

decode_header([_Unparsed] = Headers) ->
    decode_packet(http, Headers);
decode_header(Headers) ->
    decode_packet(httph, Headers).

decode_packet(Type, [Unparsed|Parsed] = Headers) ->
    case erlang:decode_packet(Type, Unparsed, []) of
	{ok, http_eoh, <<>>} ->
	    [http_eoh|Parsed];
	{more, _} ->
	    Headers;
	{ok, MoreParsed, Rest} ->
	    decode_header([Rest, MoreParsed|Parsed]);
	{error, Reason} ->
	    throw({bad_header, Reason})
    end.

go_to_continue_or_build_packet_body(Headers) ->
    case lists:keymember("100-continue", 5, Headers) of
	true  -> continue;
	false -> build_packet_body
    end.

header_value_search(Key, List, Default) ->
    case lists:keysearch(Key, 3, List) of
	{value, {_, _, _, _, CL}} -> CL;
	false                     -> Default
    end.

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
