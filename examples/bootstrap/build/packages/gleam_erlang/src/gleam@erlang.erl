-module(gleam@erlang).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([format/1, term_to_binary/1, get_line/1, system_time/1, erlang_timestamp/0, rescue/1, binary_to_term/1, unsafe_binary_to_term/1, start_arguments/0, ensure_all_started/1, make_reference/0, reference_from_dynamic/1, priv_directory/1, bounded_phash2/2, phash2/1]).
-export_type([safe/0, get_line_error/0, time_unit/0, crash/0, ensure_all_started_error/0, reference_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type safe() :: safe.

-type get_line_error() :: eof | no_data.

-type time_unit() :: second | millisecond | microsecond | nanosecond.

-type crash() :: {exited, gleam@dynamic:dynamic_()} |
    {thrown, gleam@dynamic:dynamic_()} |
    {errored, gleam@dynamic:dynamic_()}.

-type ensure_all_started_error() :: {unknown_application,
        gleam@erlang@atom:atom_()} |
    {application_failed_to_start,
        gleam@erlang@atom:atom_(),
        gleam@dynamic:dynamic_()}.

-type reference_() :: any().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 18).
?DOC(
    " Return a string representation of any term\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " erlang.format(input)\n"
    " // -> {ok,<<\"Gleam\\n\">>}%\n"
    " ```\n"
).
-spec format(any()) -> binary().
format(Term) ->
    unicode:characters_to_binary(io_lib:format(<<"~p"/utf8>>, [Term])).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 28).
?DOC(
    " Returns a `BitArray` representing given value as an [Erlang external term][1].\n"
    "\n"
    " <https://www.erlang.org/doc/apps/erts/erlang.html#term_to_binary/1>\n"
    "\n"
    " [1]: https://www.erlang.org/doc/apps/erts/erl_ext_dist\n"
).
-spec term_to_binary(any()) -> bitstring().
term_to_binary(A) ->
    erlang:term_to_binary(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 81).
?DOC(
    " Reads a line from standard input with the given prompt.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " get_line(\"Language: \")\n"
    " // > Language: <- Gleam\n"
    " // -> Ok(\"Gleam\\n\")\n"
    " ```\n"
).
-spec get_line(binary()) -> {ok, binary()} | {error, get_line_error()}.
get_line(Prompt) ->
    gleam_erlang_ffi:get_line(Prompt).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 94).
?DOC(
    " Returns the current OS system time.\n"
    "\n"
    " <https://erlang.org/doc/apps/erts/time_correction.html#OS_System_Time>\n"
).
-spec system_time(time_unit()) -> integer().
system_time(A) ->
    os:system_time(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 100).
?DOC(
    " Returns the current OS system time as a tuple of Ints\n"
    "\n"
    " <http://erlang.org/doc/man/os.html#timestamp-0>\n"
).
-spec erlang_timestamp() -> {integer(), integer(), integer()}.
erlang_timestamp() ->
    os:timestamp().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 110).
?DOC(
    " Gleam doesn't offer any way to raise exceptions, but they may still occur\n"
    " due to bugs when working with unsafe code, such as when calling Erlang\n"
    " function.\n"
    "\n"
    " This function will catch any error thrown and convert it into a result\n"
    " rather than crashing the process.\n"
).
-spec rescue(fun(() -> EZC)) -> {ok, EZC} | {error, crash()}.
rescue(A) ->
    gleam_erlang_ffi:rescue(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 42).
?DOC(
    " Decodes a value from a `BitArray` representing an [Erlang external term][1].\n"
    "\n"
    " <https://www.erlang.org/doc/apps/erts/erlang.html#binary_to_term/1>\n"
    "\n"
    " [1]: https://www.erlang.org/doc/apps/erts/erl_ext_dist\n"
).
-spec binary_to_term(bitstring()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, nil}.
binary_to_term(Binary) ->
    case gleam_erlang_ffi:rescue(
        fun() -> erlang:binary_to_term(Binary, [safe]) end
    ) of
        {ok, Term} ->
            {ok, Term};

        {error, _} ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 57).
?DOC(
    " Decodes a value from a trusted `BitArray` representing an\n"
    " [Erlang external term][1].\n"
    "\n"
    " *Warning*: Do not use this function with untrusted input, this can lead to\n"
    " Denial-of-Service. More information in the [Erlang documentation][2].\n"
    "\n"
    " [1]: https://www.erlang.org/doc/apps/erts/erl_ext_dist\n"
    " [2]: https://www.erlang.org/doc/apps/erts/erlang.html#binary_to_term/1\n"
).
-spec unsafe_binary_to_term(bitstring()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, nil}.
unsafe_binary_to_term(Binary) ->
    case gleam_erlang_ffi:rescue(fun() -> erlang:binary_to_term(Binary, []) end) of
        {ok, Term} ->
            {ok, Term};

        {error, _} ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 125).
?DOC(
    " Get the arguments given to the program when it was started.\n"
    "\n"
    " This is sometimes called `argv` in other languages.\n"
).
-spec start_arguments() -> list(binary()).
start_arguments() ->
    _pipe = init:get_plain_arguments(),
    gleam@list:map(_pipe, fun unicode:characters_to_binary/1).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 148).
?DOC(
    " Starts an OTP application's process tree in the background, as well as\n"
    " the trees of any applications that the given application depends upon. An\n"
    " OTP application typically maps onto a Gleam or Hex package.\n"
    "\n"
    " Returns a list of the applications that were started. Calling this function\n"
    " for application that have already been started is a no-op so you do not need\n"
    " to check the application state beforehand.\n"
    "\n"
    " In Gleam we prefer to not use these implicit background process trees, but\n"
    " you will likely still need to start the trees of OTP applications written in\n"
    " other BEAM languages such as Erlang or Elixir, including those included by\n"
    " default with Erlang/OTP.\n"
    "\n"
    " For more information see the OTP documentation.\n"
    " - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>\n"
    " - <https://www.erlang.org/doc/man/application.html#start-1>\n"
).
-spec ensure_all_started(gleam@erlang@atom:atom_()) -> {ok,
        list(gleam@erlang@atom:atom_())} |
    {error, ensure_all_started_error()}.
ensure_all_started(Application) ->
    gleam_erlang_ffi:ensure_all_started(Application).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 171).
?DOC(" Create a new unique reference.\n").
-spec make_reference() -> reference_().
make_reference() ->
    erlang:make_ref().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 192).
?DOC(
    " Checks to see whether a `Dynamic` value is a Reference, and return the Reference if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dynamic\n"
    "\n"
    " reference_from_dynamic(dynamic.from(make_reference()))\n"
    " // -> Ok(Reference)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " import gleam/dynamic\n"
    "\n"
    " reference_from_dynamic(dynamic.from(123))\n"
    " // -> Error([DecodeError(expected: \"Reference\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec reference_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, reference_()} |
    {error, list(gleam@dynamic@decode:decode_error())}.
reference_from_dynamic(From) ->
    gleam_erlang_ffi:reference_from_dynamic(From).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 209).
?DOC(
    " Returns the path of a package's `priv` directory, where extra non-Gleam\n"
    " or Erlang files are typically kept.\n"
    "\n"
    " Returns an error if no package was found with the given name.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " erlang.priv_directory(\"my_app\")\n"
    " // -> Ok(\"/some/location/my_app/priv\")\n"
    " ```\n"
).
-spec priv_directory(binary()) -> {ok, binary()} | {error, nil}.
priv_directory(Name) ->
    gleam_erlang_ffi:priv_directory(Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 220).
?DOC(
    " Portable hash function that gives the same hash for the\n"
    " same Gleam/Erlang term regardless of machine architecture and\n"
    " ERTS version.\n"
    "\n"
    " The function returns a hash value for Term within the range 0..limit-1.\n"
    " The maximum value for limit is 2^32.\n"
    "\n"
    " <https://www.erlang.org/doc/apps/erts/erlang.html#phash2/2>\n"
).
-spec bounded_phash2(any(), integer()) -> integer().
bounded_phash2(Term, Limit) ->
    erlang:phash2(Term, Limit).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang.gleam", 224).
?DOC(" Equivalent to `bounded_phash2`, with a upper limit of 2^27-1\n").
-spec phash2(any()) -> integer().
phash2(Term) ->
    erlang:phash2(Term).
