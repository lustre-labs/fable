-module(lustre_fable@icon).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([sidebar/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_fable/icon.gleam", 6).
?DOC(false).
-spec sidebar(list(lustre@vdom@vattr:attribute(DEO))) -> lustre@vdom@vnode:element(DEO).
sidebar(Attributes) ->
    lustre@element@html:svg(
        [lustre@attribute:attribute(<<"width"/utf8>>, <<"24"/utf8>>),
            lustre@attribute:attribute(<<"height"/utf8>>, <<"24"/utf8>>),
            lustre@attribute:attribute(<<"viewBox"/utf8>>, <<"0 0 24 24"/utf8>>),
            lustre@attribute:attribute(<<"fill"/utf8>>, <<"none"/utf8>>),
            lustre@attribute:attribute(
                <<"stroke"/utf8>>,
                <<"currentColor"/utf8>>
            ),
            lustre@attribute:attribute(<<"stroke-width"/utf8>>, <<"2"/utf8>>),
            lustre@attribute:attribute(
                <<"stroke-linecap"/utf8>>,
                <<"round"/utf8>>
            ),
            lustre@attribute:attribute(
                <<"stroke-linejoin"/utf8>>,
                <<"round"/utf8>>
            ) |
            Attributes],
        [lustre@element@svg:rect(
                [lustre@attribute:attribute(<<"width"/utf8>>, <<"18"/utf8>>),
                    lustre@attribute:attribute(<<"height"/utf8>>, <<"18"/utf8>>),
                    lustre@attribute:attribute(<<"x"/utf8>>, <<"3"/utf8>>),
                    lustre@attribute:attribute(<<"y"/utf8>>, <<"3"/utf8>>),
                    lustre@attribute:attribute(<<"rx"/utf8>>, <<"2"/utf8>>)]
            ),
            lustre@element@svg:path(
                [lustre@attribute:attribute(<<"d"/utf8>>, <<"M15 3v18"/utf8>>)]
            )]
    ).
