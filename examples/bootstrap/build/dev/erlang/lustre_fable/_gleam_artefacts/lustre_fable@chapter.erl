-module(lustre_fable@chapter).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([story/2, init/4]).
-export_type([chapter/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type chapter() :: {chapter, binary(), list(lustre_fable@story:story())}.

-file("src/lustre_fable/chapter.gleam", 25).
?DOC(false).
-spec story(chapter(), binary()) -> {ok, lustre_fable@story:story()} |
    {error, nil}.
story(Chapter, Slug) ->
    gleam@list:find(
        erlang:element(3, Chapter),
        fun(Story) -> erlang:element(3, Story) =:= Slug end
    ).

-file("src/lustre_fable/chapter.gleam", 12).
?DOC(false).
-spec init(
    binary(),
    list(lustre_fable@story:story_config()),
    list(binary()),
    list(binary())
) -> {ok, chapter()} | {error, lustre:error()}.
init(Title, Stories, Stylesheets, External_stylesheets) ->
    _pipe = Stories,
    _pipe@1 = gleam@list:try_map(
        _pipe,
        fun(_capture) ->
            lustre_fable@story:register(
                _capture,
                Stylesheets,
                External_stylesheets
            )
        end
    ),
    gleam@result:map(
        _pipe@1,
        fun(_capture@1) -> {chapter, Title, _capture@1} end
    ).
