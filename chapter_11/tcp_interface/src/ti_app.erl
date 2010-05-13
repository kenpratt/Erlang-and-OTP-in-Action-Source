-module(ti_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1055).

start(_StartType, _StartArgs) ->
    Port = case application:get_env(tcp_interface, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    case ti_sup:start_link(LSock) of
        {ok, Pid} ->
            ti_sup:start_child(),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
