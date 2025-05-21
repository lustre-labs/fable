-module(lustre_fable@route).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/1, href/1]).
-export_type([route/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type route() :: index |
    {chapter, binary()} |
    {story, binary(), binary()} |
    not_found.

-file("src/lustre_fable/route.gleam", 13).
?DOC(false).
-spec parse(gleam@uri:uri()) -> route().
parse(Uri) ->
    case gleam@uri:path_segments(erlang:element(6, Uri)) of
        [] ->
            index;

        [<<"chapter"/utf8>>, Chapter] ->
            {chapter, Chapter};

        [<<"chapter"/utf8>>, Chapter@1, <<"story"/utf8>>, Story] ->
            {story, Chapter@1, Story};

        _ ->
            not_found
    end.

-file("src/lustre_fable/route.gleam", 24).
?DOC(false).
-spec href(route()) -> lustre@vdom@vattr:attribute(any()).
href(Route) ->
    lustre@attribute:href(case Route of
            index ->
                <<"/"/utf8>>;

            {chapter, Chapter} ->
                <<"/chapter/"/utf8, Chapter/binary>>;

            {story, Chapter@1, Story} ->
                <<<<<<"/chapter/"/utf8, Chapter@1/binary>>/binary,
                        "/story/"/utf8>>/binary,
                    Story/binary>>;

            not_found ->
                <<"#"/utf8>>
        end).
