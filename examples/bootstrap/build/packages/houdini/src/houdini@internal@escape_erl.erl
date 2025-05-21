-module(houdini@internal@escape_erl).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([escape/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/houdini/internal/escape_erl.gleam", 74).
?DOC(false).
-spec do_escape_normal(
    bitstring(),
    integer(),
    bitstring(),
    bitstring(),
    integer()
) -> bitstring().
do_escape_normal(Bin, Skip, Original, Acc, Len) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            Acc@1 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&lt;"/utf8>>,
            do_escape(Rest, (Skip + Len) + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            Acc@2 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&gt;"/utf8>>,
            do_escape(Rest@1, (Skip + Len) + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            Acc@3 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&amp;"/utf8>>,
            do_escape(Rest@2, (Skip + Len) + 1, Original, Acc@3);

        <<"\""/utf8, Rest@3/bitstring>> ->
            Acc@4 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&quot;"/utf8>>,
            do_escape(Rest@3, (Skip + Len) + 1, Original, Acc@4);

        <<"'"/utf8, Rest@4/bitstring>> ->
            Acc@5 = <<Acc/bitstring,
                (binary:part(Original, Skip, Len))/bitstring,
                "&#39;"/utf8>>,
            do_escape(Rest@4, (Skip + Len) + 1, Original, Acc@5);

        <<_, Rest@5/bitstring>> ->
            do_escape_normal(Rest@5, Skip, Original, Acc, Len + 1);

        <<>> ->
            case Skip of
                0 ->
                    Original;

                _ ->
                    <<Acc/bitstring,
                        (binary:part(Original, Skip, Len))/bitstring>>
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"non byte aligned string, all strings should be byte aligned"/utf8>>,
                    module => <<"houdini/internal/escape_erl"/utf8>>,
                    function => <<"do_escape_normal"/utf8>>,
                    line => 142})
    end.

-file("src/houdini/internal/escape_erl.gleam", 28).
?DOC(false).
-spec do_escape(bitstring(), integer(), bitstring(), bitstring()) -> bitstring().
do_escape(Bin, Skip, Original, Acc) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            Acc@1 = <<Acc/bitstring, "&lt;"/utf8>>,
            do_escape(Rest, Skip + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            Acc@2 = <<Acc/bitstring, "&gt;"/utf8>>,
            do_escape(Rest@1, Skip + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            Acc@3 = <<Acc/bitstring, "&amp;"/utf8>>,
            do_escape(Rest@2, Skip + 1, Original, Acc@3);

        <<"\""/utf8, Rest@3/bitstring>> ->
            Acc@4 = <<Acc/bitstring, "&quot;"/utf8>>,
            do_escape(Rest@3, Skip + 1, Original, Acc@4);

        <<"'"/utf8, Rest@4/bitstring>> ->
            Acc@5 = <<Acc/bitstring, "&#39;"/utf8>>,
            do_escape(Rest@4, Skip + 1, Original, Acc@5);

        <<_, Rest@5/bitstring>> ->
            do_escape_normal(Rest@5, Skip, Original, Acc, 1);

        <<>> ->
            Acc;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"non byte aligned string, all strings should be byte aligned"/utf8>>,
                    module => <<"houdini/internal/escape_erl"/utf8>>,
                    function => <<"do_escape"/utf8>>,
                    line => 69})
    end.

-file("src/houdini/internal/escape_erl.gleam", 2).
?DOC(false).
-spec escape(binary()) -> binary().
escape(Text) ->
    Bits = <<Text/binary>>,
    Result = do_escape(Bits, 0, Bits, <<>>),
    houdini_ffi:coerce(Result).
