-module(gleam@erlang@os).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([family/0]).
-export_type([os_family/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type os_family() :: windows_nt | linux | darwin | free_bsd | {other, binary()}.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/os.gleam", 36).
?DOC(
    " Returns the kernel of the host operating system.\n"
    "\n"
    " Unknown kernels are reported as `Other(String)`; e.g. `Other(\"sunos\")`.\n"
    "\n"
    " ## Examples\n"
    " ```gleam\n"
    " family()\n"
    " // -> Linux\n"
    " ```\n"
    " \n"
    " ```gleam\n"
    " family()\n"
    " // -> Darwin\n"
    " ```\n"
    " \n"
    " ```gleam\n"
    " family()\n"
    " // -> Other(\"sunos\")\n"
    " ```\n"
).
-spec family() -> os_family().
family() ->
    gleam_erlang_ffi:os_family().
