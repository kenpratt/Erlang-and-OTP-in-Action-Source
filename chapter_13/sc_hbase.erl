-module(sc_hbase).

-export([put/3, get/2, delete/2]).

put(Node, Key, Value) ->
  Ref = make_ref(),
  {hbase_server, Node} ! {put, self(), Ref, Key, term_to_binary(Value)},
  receive
    {reply, Ref, ok} ->
      ok
  after 3000 ->
      {error, timeout}
  end.

get(Node, Key) ->
  Ref = make_ref(),
  {hbase_server, Node} ! {get, self(), Ref, Key},
  receive
    {reply, Ref, Value} ->
      {ok, Value}
  after 3000 ->
      {error, timeout}
  end.

delete(Node, Key) ->
  Ref = make_ref(),
  {hbase_server, Node} ! {delete, self(), Ref, Key},
  receive
    {reply, Ref, ok} ->
      ok
  after 3000 ->
      {error, timeout}
  end.
