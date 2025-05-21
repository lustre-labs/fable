-module(houdini@internal@escape_generic).
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

-file("src/houdini/internal/escape_generic.gleam", 13).
?DOC(false).
-spec do_escape(binary()) -> binary().
do_escape(Text) ->
    _pipe = Text,
    _pipe@1 = gleam@string:replace(_pipe, <<">"/utf8>>, <<"&gt;"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"<"/utf8>>, <<"&lt;"/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"&"/utf8>>, <<"&amp;"/utf8>>),
    _pipe@4 = gleam@string:replace(_pipe@3, <<"'"/utf8>>, <<"&#39;"/utf8>>),
    gleam@string:replace(_pipe@4, <<"\""/utf8>>, <<"&quot;"/utf8>>).

-file("src/houdini/internal/escape_generic.gleam", 8).
?DOC(false).
-spec escape(binary()) -> binary().
escape(Text) ->
    do_escape(Text).
