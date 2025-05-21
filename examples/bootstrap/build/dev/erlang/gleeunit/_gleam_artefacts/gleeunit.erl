-module(gleeunit).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([atom_/0, encoding/0, report_module_name/0, gleeunit_progress_option/0, eunit_option/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type atom_() :: any().

-type encoding() :: utf8.

-type report_module_name() :: gleeunit_progress.

-type gleeunit_progress_option() :: {colored, boolean()}.

-type eunit_option() :: verbose |
    no_tty |
    {report, {report_module_name(), list(gleeunit_progress_option())}}.

-file("src/gleeunit.gleam", 36).
-spec gleam_to_erlang_module_name(binary()) -> binary().
gleam_to_erlang_module_name(Path) ->
    _pipe = Path,
    _pipe@1 = gleam@string:replace(_pipe, <<".gleam"/utf8>>, <<""/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<".erl"/utf8>>, <<""/utf8>>),
    gleam@string:replace(_pipe@2, <<"/"/utf8>>, <<"@"/utf8>>).

-file("src/gleeunit.gleam", 17).
-spec do_main() -> nil.
do_main() ->
    Options = [verbose,
        no_tty,
        {report, {gleeunit_progress, [{colored, true}]}}],
    Result = begin
        _pipe = gleeunit_ffi:find_files(
            <<"**/*.{erl,gleam}"/utf8>>,
            <<"test"/utf8>>
        ),
        _pipe@1 = gleam@list:map(_pipe, fun gleam_to_erlang_module_name/1),
        _pipe@2 = gleam@list:map(
            _pipe@1,
            fun(_capture) -> erlang:binary_to_atom(_capture, utf8) end
        ),
        gleeunit_ffi:run_eunit(_pipe@2, Options)
    end,
    Code = case Result of
        {ok, _} ->
            0;

        {error, _} ->
            1
    end,
    erlang:halt(Code).

-file("src/gleeunit.gleam", 12).
?DOC(
    " Find and run all test functions for the current project using Erlang's EUnit\n"
    " test framework.\n"
    "\n"
    " Any Erlang or Gleam function in the `test` directory with a name ending in\n"
    " `_test` is considered a test function and will be run.\n"
    "\n"
    " If running on JavaScript tests will be run with a custom test runner.\n"
).
-spec main() -> nil.
main() ->
    do_main().
