-module(gleam@erlang@atom).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_string/1, create_from_string/1, to_string/1, from_dynamic/1]).
-export_type([atom_/0, from_string_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type atom_() :: any().

-type from_string_error() :: atom_not_loaded.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/atom.gleam", 43).
?DOC(
    " Finds an existing Atom for the given String.\n"
    "\n"
    " If no atom is found in the virtual machine's atom table for the String then\n"
    " an error is returned.\n"
    "\n"
    " ## Examples\n"
    " ```gleam\n"
    " from_string(\"ok\")\n"
    " // -> Ok(create_from_string(\"ok\"))\n"
    " ```\n"
    " ```gleam\n"
    " from_string(\"some_new_atom\")\n"
    " // -> Error(AtomNotLoaded)\n"
    " ```\n"
).
-spec from_string(binary()) -> {ok, atom_()} | {error, from_string_error()}.
from_string(A) ->
    gleam_erlang_ffi:atom_from_string(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/atom.gleam", 55).
?DOC(
    " Creates an atom from a string, inserting a new value into the virtual\n"
    " machine's atom table if an atom does not already exist for the given\n"
    " string.\n"
    "\n"
    " We must be careful when using this function as there is a limit to the\n"
    " number of atom that can fit in the virtual machine's atom table. Never\n"
    " convert user input into atoms as filling the atom table will cause the\n"
    " virtual machine to crash!\n"
).
-spec create_from_string(binary()) -> atom_().
create_from_string(A) ->
    erlang:binary_to_atom(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/atom.gleam", 68).
?DOC(
    " Returns a `String` corresponding to the text representation of the given\n"
    " `Atom`.\n"
    "\n"
    " ## Examples\n"
    " ```gleam\n"
    " let ok_atom = create_from_string(\"ok\")\n"
    " to_string(ok_atom)\n"
    " // -> \"ok\"\n"
    " ```\n"
).
-spec to_string(atom_()) -> binary().
to_string(A) ->
    erlang:atom_to_binary(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/atom.gleam", 85).
?DOC(
    " Checks to see whether a `Dynamic` value is an atom, and return the atom if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    " ```gleam\n"
    " import gleam/dynamic\n"
    " from_dynamic(dynamic.from(create_from_string(\"hello\")))\n"
    " // -> Ok(create_from_string(\"hello\"))\n"
    " ```\n"
    " ```gleam\n"
    " from_dynamic(dynamic.from(123))\n"
    " // -> Error([DecodeError(expected: \"Atom\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec from_dynamic(gleam@dynamic:dynamic_()) -> {ok, atom_()} |
    {error, list(gleam@dynamic@decode:decode_error())}.
from_dynamic(From) ->
    gleam_erlang_ffi:atom_from_dynamic(From).
