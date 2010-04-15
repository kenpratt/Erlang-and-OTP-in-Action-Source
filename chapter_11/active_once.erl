-module(active_once).
-export([start/0]).

start() ->
    {ok, LSock} = gen_tcp:listen(1055, [binary, {active, false}]),
    accept(LSock).

accept(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    inet:setopts(Socket,[{active,once}]),
    loop().

loop() ->
    receive
	{tcp, Socket, Packet} ->
	    io:format("got ~p~n", [Packet]),
	    inet:setopts(Socket,[{active,once}]),
	    loop();
	{tcp_closed, _Socket} ->
	    ok
    end.

