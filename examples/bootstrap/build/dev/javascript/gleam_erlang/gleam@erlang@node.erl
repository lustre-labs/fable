-module(gleam@erlang@node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([self/0, visible/0, connect/1, send/3, to_atom/1]).
-export_type([node_/0, do_not_leak/0, connect_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type node_() :: any().

-type do_not_leak() :: any().

-type connect_error() :: failed_to_connect | local_node_is_not_alive.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 10).
?DOC(" Return the current node.\n").
-spec self() -> node_().
self() ->
    erlang:node().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 23).
?DOC(
    " Return a list of all visible nodes in the cluster, not including the current\n"
    " node.\n"
    "\n"
    " The current node can be included by calling `self()` and prepending the\n"
    " result.\n"
    "\n"
    " ```gleam\n"
    " let all_nodes = [node.self(), ..node.visible()]\n"
    " ```\n"
).
-spec visible() -> list(node_()).
visible() ->
    erlang:nodes().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 44).
?DOC(
    " Establish a connection to a node, so the nodes can send messages to each\n"
    " other and any other connected nodes.\n"
    "\n"
    " Returns `Error(FailedToConnect)` if the node is not reachable.\n"
    "\n"
    " Returns `Error(LocalNodeIsNotAlive)` if the local node is not alive, meaning\n"
    " it is not running in distributed mode.\n"
).
-spec connect(gleam@erlang@atom:atom_()) -> {ok, node_()} |
    {error, connect_error()}.
connect(Node) ->
    gleam_erlang_ffi:connect_node(Node).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 51).
?DOC(
    " Send a message to a named process on a given node.\n"
    "\n"
    " These messages are untyped, like regular Erlang messages.\n"
).
-spec send(node_(), gleam@erlang@atom:atom_(), any()) -> nil.
send(Node, Name, Message) ->
    erlang:send({Name, Node}, Message),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/node.gleam", 62).
?DOC(" Convert a node to the atom of its name.\n").
-spec to_atom(node_()) -> gleam@erlang@atom:atom_().
to_atom(Node) ->
    gleam_erlang_ffi:identity(Node).
