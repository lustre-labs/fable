-module(gleeunit@should).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([equal/2, not_equal/2, be_ok/1, be_error/1, be_some/1, be_none/1, be_true/1, be_false/1, fail/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A module for testing your Gleam code. The functions found here are\n"
    " compatible with the Erlang eunit test framework.\n"
    "\n"
    " More information on running eunit can be found in [the rebar3\n"
    " documentation](https://rebar3.org/docs/testing/eunit/).\n"
).

-file("src/gleeunit/should.gleam", 11).
-spec equal(KZN, KZN) -> nil.
equal(A, B) ->
    gleeunit_ffi:should_equal(A, B).

-file("src/gleeunit/should.gleam", 25).
-spec not_equal(KZO, KZO) -> nil.
not_equal(A, B) ->
    gleeunit_ffi:should_not_equal(A, B).

-file("src/gleeunit/should.gleam", 39).
-spec be_ok({ok, KZP} | {error, any()}) -> KZP.
be_ok(A) ->
    gleeunit_ffi:should_be_ok(A).

-file("src/gleeunit/should.gleam", 47).
-spec be_error({ok, any()} | {error, KZU}) -> KZU.
be_error(A) ->
    gleeunit_ffi:should_be_error(A).

-file("src/gleeunit/should.gleam", 54).
-spec be_some(gleam@option:option(KZX)) -> KZX.
be_some(A) ->
    case A of
        {some, Value} ->
            Value;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => erlang:list_to_binary(
                        [<<"\n"/utf8>>,
                            gleam@string:inspect(A),
                            <<"\nshould be some"/utf8>>]
                    ),
                    module => <<"gleeunit/should"/utf8>>,
                    function => <<"be_some"/utf8>>,
                    line => 57})
    end.

-file("src/gleeunit/should.gleam", 61).
-spec be_none(gleam@option:option(any())) -> nil.
be_none(A) ->
    case A of
        none ->
            nil;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => erlang:list_to_binary(
                        [<<"\n"/utf8>>,
                            gleam@string:inspect(A),
                            <<"\nshould be none"/utf8>>]
                    ),
                    module => <<"gleeunit/should"/utf8>>,
                    function => <<"be_none"/utf8>>,
                    line => 64})
    end.

-file("src/gleeunit/should.gleam", 68).
-spec be_true(boolean()) -> nil.
be_true(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, true).

-file("src/gleeunit/should.gleam", 73).
-spec be_false(boolean()) -> nil.
be_false(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, false).

-file("src/gleeunit/should.gleam", 78).
-spec fail() -> nil.
fail() ->
    be_true(false).
