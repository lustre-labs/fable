-module(gleam@erlang@port).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([port_from_dynamic/1]).
-export_type([port_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type port_() :: any().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/port.gleam", 27).
?DOC(
    " Checks to see whether a `Dynamic` value is a port, and return the port if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    "    > import gleam/dynamic\n"
    "    > port_from_dynamic(dynamic.from(process.self()))\n"
    "    Ok(process.self())\n"
    "\n"
    "    > port_from_dynamic(dynamic.from(123))\n"
    "    Error([DecodeError(expected: \"Port\", found: \"Int\", path: [])])\n"
).
-spec port_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, port_()} |
    {error, list(gleam@dynamic@decode:decode_error())}.
port_from_dynamic(From) ->
    gleam_erlang_ffi:port_from_dynamic(From).
