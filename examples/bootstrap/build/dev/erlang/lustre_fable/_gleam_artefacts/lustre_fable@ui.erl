-module(lustre_fable@ui).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([breadcrumb/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_fable/ui.gleam", 9).
?DOC(false).
-spec breadcrumb(
    lustre_fable@route:route(),
    gleam@dict:dict(binary(), lustre_fable@chapter:chapter())
) -> lustre@vdom@vnode:element(any()).
breadcrumb(Route, Chapters) ->
    Separator = lustre@element@html:span(
        [lustre@attribute:class(<<"text-stone-400 user-select-none"/utf8>>)],
        [lustre@element@html:text(<<"/"/utf8>>)]
    ),
    Link = fun(Route@1, Label) ->
        lustre@element@html:a(
            [lustre_fable@route:href(Route@1)],
            [lustre@element@html:text(Label)]
        )
    end,
    Current = fun(Label@1) ->
        lustre@element@html:a(
            [lustre@attribute:href(<<"#"/utf8>>),
                lustre@attribute:aria_current(<<"page"/utf8>>)],
            [lustre@element@html:text(Label@1)]
        )
    end,
    lustre@element@html:nav(
        [lustre@attribute:class(<<"flex gap-4"/utf8>>)],
        case Route of
            index ->
                [];

            not_found ->
                [];

            {chapter, Chapter} ->
                case gleam_stdlib:map_get(Chapters, Chapter) of
                    {error, _} ->
                        [];

                    {ok, C} ->
                        [Separator, Current(erlang:element(2, C))]
                end;

            {story, Chapter@1, Story} ->
                C@1 = gleam_stdlib:map_get(Chapters, Chapter@1),
                S = begin
                    _pipe = C@1,
                    gleam@result:then(
                        _pipe,
                        fun(_capture) ->
                            lustre_fable@chapter:story(_capture, Story)
                        end
                    )
                end,
                case {C@1, S} of
                    {{ok, C@2}, {ok, S@1}} ->
                        [Separator,
                            Link({chapter, Chapter@1}, erlang:element(2, C@2)),
                            Separator,
                            Current(erlang:element(2, S@1))];

                    {{ok, C@3}, _} ->
                        [Separator, Current(erlang:element(2, C@3))];

                    {_, _} ->
                        []
                end
        end
    ).
