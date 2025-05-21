-module(gleam@erlang@charlist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_string/1, from_string/1]).
-export_type([charlist/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A charlist is a list of integers where all the integers are valid code\n"
    " points.\n"
    "\n"
    " In practice, you will not come across them often, except perhaps when\n"
    " interfacing with Erlang, in particular when using older libraries that do\n"
    " not accept binaries as arguments.\n"
).

-type charlist() :: any().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/charlist.gleam", 14).
?DOC(" Transform a charlist to a string\n").
-spec to_string(charlist()) -> binary().
to_string(A) ->
    unicode:characters_to_binary(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/charlist.gleam", 22).
?DOC(" Transform a string to a charlist\n").
-spec from_string(binary()) -> charlist().
from_string(A) ->
    unicode:characters_to_list(A).
