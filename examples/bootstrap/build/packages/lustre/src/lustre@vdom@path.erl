-module(lustre@vdom@path).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([add/3, to_string/1, matches/2, event/2]).
-export_type([path/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque path() :: root | {key, binary(), path()} | {index, integer(), path()}.

-file("src/lustre/vdom/path.gleam", 45).
?DOC(false).
-spec do_matches(binary(), list(binary())) -> boolean().
do_matches(Path, Candidates) ->
    case Candidates of
        [] ->
            false;

        [Candidate | Rest] ->
            case gleam_stdlib:string_starts_with(Path, Candidate) of
                true ->
                    true;

                false ->
                    do_matches(Path, Rest)
            end
    end.

-file("src/lustre/vdom/path.gleam", 60).
?DOC(false).
-spec add(path(), integer(), binary()) -> path().
add(Parent, Index, Key) ->
    case Key of
        <<""/utf8>> ->
            {index, Index, Parent};

        _ ->
            {key, Key, Parent}
    end.

-file("src/lustre/vdom/path.gleam", 81).
?DOC(false).
-spec do_to_string(path(), list(binary())) -> binary().
do_to_string(Path, Acc) ->
    case Path of
        root ->
            case Acc of
                [] ->
                    <<""/utf8>>;

                [_ | Segments] ->
                    erlang:list_to_binary(Segments)
            end;

        {key, Key, Parent} ->
            do_to_string(Parent, [<<"\t"/utf8>>, Key | Acc]);

        {index, Index, Parent@1} ->
            do_to_string(
                Parent@1,
                [<<"\n"/utf8>>, erlang:integer_to_binary(Index) | Acc]
            )
    end.

-file("src/lustre/vdom/path.gleam", 77).
?DOC(false).
-spec to_string(path()) -> binary().
to_string(Path) ->
    do_to_string(Path, []).

-file("src/lustre/vdom/path.gleam", 38).
?DOC(false).
-spec matches(path(), list(binary())) -> boolean().
matches(Path, Candidates) ->
    case Candidates of
        [] ->
            false;

        _ ->
            do_matches(to_string(Path), Candidates)
    end.

-file("src/lustre/vdom/path.gleam", 71).
?DOC(false).
-spec event(path(), binary()) -> binary().
event(Path, Event) ->
    do_to_string(Path, [<<"\f"/utf8>>, Event]).
