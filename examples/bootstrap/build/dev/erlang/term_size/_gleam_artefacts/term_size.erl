-module(term_size).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/0, rows/0, columns/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " All functions return `Error(Nil)` if the terminal's size could not be\n"
    " determined for any reason.\n"
).

-file("src/term_size.gleam", 21).
?DOC(
    " Get the current terminal's size. The return tuple is of the form\n"
    " `#(rows, columns)`.\n"
    "\n"
    "\n"
    " ```\n"
    " import term_size\n"
    "\n"
    " pub fn main() {\n"
    "   case term_size.get() {\n"
    "     Ok(#(rows, columns)) -> // ...\n"
    "     Error(Nil) -> // ...\n"
    "   }\n"
    " }\n"
    " ```\n"
).
-spec get() -> {ok, {integer(), integer()}} | {error, nil}.
get() ->
    term_size_ffi:terminal_size().

-file("src/term_size.gleam", 25).
?DOC(" Get the number of rows (lines) visible in the terminal.\n").
-spec rows() -> {ok, integer()} | {error, nil}.
rows() ->
    case term_size_ffi:terminal_size() of
        {ok, {Rows, _}} ->
            {ok, Rows};

        {error, nil} ->
            {error, nil}
    end.

-file("src/term_size.gleam", 34).
?DOC(" Get the character width of the terminal.\n").
-spec columns() -> {ok, integer()} | {error, nil}.
columns() ->
    case term_size_ffi:terminal_size() of
        {ok, {_, Columns}} ->
            {ok, Columns};

        {error, nil} ->
            {error, nil}
    end.
