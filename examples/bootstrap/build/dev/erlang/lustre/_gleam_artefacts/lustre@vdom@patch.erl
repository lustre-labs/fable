-module(lustre@vdom@patch).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/4, is_empty/1, add_child/2, to_json/1, replace_text/1, replace_inner_html/1, update/2, move/3, remove_key/2, replace/3, insert/2, remove/2]).
-export_type([diff/1, patch/1, change/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type diff(SQF) :: {diff, patch(SQF), lustre@vdom@events:events(SQF)}.

-type patch(SQG) :: {patch,
        integer(),
        integer(),
        list(change(SQG)),
        list(patch(SQG))}.

-type change(SQH) :: {replace_text, integer(), binary()} |
    {replace_inner_html, integer(), binary()} |
    {update,
        integer(),
        list(lustre@vdom@vattr:attribute(SQH)),
        list(lustre@vdom@vattr:attribute(SQH))} |
    {move, integer(), binary(), integer(), integer()} |
    {remove_key, integer(), binary(), integer()} |
    {replace, integer(), integer(), integer(), lustre@vdom@vnode:element(SQH)} |
    {insert, integer(), list(lustre@vdom@vnode:element(SQH)), integer()} |
    {remove, integer(), integer(), integer()}.

-file("src/lustre/vdom/patch.gleam", 86).
?DOC(false).
-spec new(integer(), integer(), list(change(SQI)), list(patch(SQI))) -> patch(SQI).
new(Index, Removed, Changes, Children) ->
    {patch, Index, Removed, Changes, Children}.

-file("src/lustre/vdom/patch.gleam", 159).
?DOC(false).
-spec is_empty(patch(any())) -> boolean().
is_empty(Patch) ->
    case Patch of
        {patch, _, 0, [], []} ->
            true;

        _ ->
            false
    end.

-file("src/lustre/vdom/patch.gleam", 168).
?DOC(false).
-spec add_child(patch(SRN), patch(SRN)) -> patch(SRN).
add_child(Parent, Child) ->
    case is_empty(Child) of
        true ->
            Parent;

        false ->
            _record = Parent,
            {patch,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                [Child | erlang:element(5, Parent)]}
    end.

-file("src/lustre/vdom/patch.gleam", 200).
?DOC(false).
-spec replace_text_to_json(integer(), binary()) -> gleam@json:json().
replace_text_to_json(Kind, Content) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:string(
        _pipe,
        <<"content"/utf8>>,
        Content
    ),
    lustre@internals@json_object_builder:build(_pipe@1).

-file("src/lustre/vdom/patch.gleam", 206).
?DOC(false).
-spec replace_inner_html_to_json(integer(), binary()) -> gleam@json:json().
replace_inner_html_to_json(Kind, Inner_html) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:string(
        _pipe,
        <<"inner_html"/utf8>>,
        Inner_html
    ),
    lustre@internals@json_object_builder:build(_pipe@1).

-file("src/lustre/vdom/patch.gleam", 212).
?DOC(false).
-spec update_to_json(
    integer(),
    list(lustre@vdom@vattr:attribute(any())),
    list(lustre@vdom@vattr:attribute(any()))
) -> gleam@json:json().
update_to_json(Kind, Added, Removed) ->
    _pipe = lustre@internals@json_object_builder:tagged(Kind),
    _pipe@1 = lustre@internals@json_object_builder:list(
        _pipe,
        <<"added"/utf8>>,
        Added,
        fun lustre@vdom@vattr:to_json/1
    ),
    _pipe@2 = lustre@internals@json_object_builder:list(
        _pipe@1,
        <<"removed"/utf8>>,
        Removed,
        fun lustre@vdom@vattr:to_json/1
    ),
    lustre@internals@json_object_builder:build(_pipe@2).

-file("src/lustre/vdom/patch.gleam", 219).
?DOC(false).
-spec move_to_json(integer(), binary(), integer(), integer()) -> gleam@json:json().
move_to_json(Kind, Key, Before, Count) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"key"/utf8>>, gleam@json:string(Key)},
            {<<"before"/utf8>>, gleam@json:int(Before)},
            {<<"count"/utf8>>, gleam@json:int(Count)}]
    ).

-file("src/lustre/vdom/patch.gleam", 228).
?DOC(false).
-spec remove_key_to_json(integer(), binary(), integer()) -> gleam@json:json().
remove_key_to_json(Kind, Key, Count) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"key"/utf8>>, gleam@json:string(Key)},
            {<<"count"/utf8>>, gleam@json:int(Count)}]
    ).

-file("src/lustre/vdom/patch.gleam", 236).
?DOC(false).
-spec replace_to_json(
    integer(),
    integer(),
    integer(),
    lustre@vdom@vnode:element(any())
) -> gleam@json:json().
replace_to_json(Kind, From, Count, With) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"from"/utf8>>, gleam@json:int(From)},
            {<<"count"/utf8>>, gleam@json:int(Count)},
            {<<"with"/utf8>>, lustre@vdom@vnode:to_json(With)}]
    ).

-file("src/lustre/vdom/patch.gleam", 245).
?DOC(false).
-spec insert_to_json(
    integer(),
    list(lustre@vdom@vnode:element(any())),
    integer()
) -> gleam@json:json().
insert_to_json(Kind, Children, Before) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"children"/utf8>>,
                gleam@json:array(Children, fun lustre@vdom@vnode:to_json/1)},
            {<<"before"/utf8>>, gleam@json:int(Before)}]
    ).

-file("src/lustre/vdom/patch.gleam", 253).
?DOC(false).
-spec remove_to_json(integer(), integer(), integer()) -> gleam@json:json().
remove_to_json(Kind, From, Count) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"from"/utf8>>, gleam@json:int(From)},
            {<<"count"/utf8>>, gleam@json:int(Count)}]
    ).

-file("src/lustre/vdom/patch.gleam", 186).
?DOC(false).
-spec change_to_json(change(any())) -> gleam@json:json().
change_to_json(Change) ->
    case Change of
        {replace_text, Kind, Content} ->
            replace_text_to_json(Kind, Content);

        {replace_inner_html, Kind@1, Inner_html} ->
            replace_inner_html_to_json(Kind@1, Inner_html);

        {update, Kind@2, Added, Removed} ->
            update_to_json(Kind@2, Added, Removed);

        {move, Kind@3, Key, Before, Count} ->
            move_to_json(Kind@3, Key, Before, Count);

        {remove_key, Kind@4, Key@1, Count@1} ->
            remove_key_to_json(Kind@4, Key@1, Count@1);

        {replace, Kind@5, From, Count@2, With} ->
            replace_to_json(Kind@5, From, Count@2, With);

        {insert, Kind@6, Children, Before@1} ->
            insert_to_json(Kind@6, Children, Before@1);

        {remove, Kind@7, From@1, Count@3} ->
            remove_to_json(Kind@7, From@1, Count@3)
    end.

-file("src/lustre/vdom/patch.gleam", 177).
?DOC(false).
-spec to_json(patch(any())) -> gleam@json:json().
to_json(Patch) ->
    _pipe = lustre@internals@json_object_builder:new(),
    _pipe@1 = lustre@internals@json_object_builder:int(
        _pipe,
        <<"index"/utf8>>,
        erlang:element(2, Patch)
    ),
    _pipe@2 = lustre@internals@json_object_builder:int(
        _pipe@1,
        <<"removed"/utf8>>,
        erlang:element(3, Patch)
    ),
    _pipe@3 = lustre@internals@json_object_builder:list(
        _pipe@2,
        <<"changes"/utf8>>,
        erlang:element(4, Patch),
        fun change_to_json/1
    ),
    _pipe@4 = lustre@internals@json_object_builder:list(
        _pipe@3,
        <<"children"/utf8>>,
        erlang:element(5, Patch),
        fun to_json/1
    ),
    lustre@internals@json_object_builder:build(_pipe@4).

-file("src/lustre/vdom/patch.gleam", 97).
?DOC(false).
-spec replace_text(binary()) -> change(any()).
replace_text(Content) ->
    {replace_text, 0, Content}.

-file("src/lustre/vdom/patch.gleam", 103).
?DOC(false).
-spec replace_inner_html(binary()) -> change(any()).
replace_inner_html(Inner_html) ->
    {replace_inner_html, 1, Inner_html}.

-file("src/lustre/vdom/patch.gleam", 109).
?DOC(false).
-spec update(
    list(lustre@vdom@vattr:attribute(SQS)),
    list(lustre@vdom@vattr:attribute(SQS))
) -> change(SQS).
update(Added, Removed) ->
    {update, 2, Added, Removed}.

-file("src/lustre/vdom/patch.gleam", 118).
?DOC(false).
-spec move(binary(), integer(), integer()) -> change(any()).
move(Key, Before, Count) ->
    {move, 3, Key, Before, Count}.

-file("src/lustre/vdom/patch.gleam", 128).
?DOC(false).
-spec remove_key(binary(), integer()) -> change(any()).
remove_key(Key, Count) ->
    {remove_key, 4, Key, Count}.

-file("src/lustre/vdom/patch.gleam", 134).
?DOC(false).
-spec replace(integer(), integer(), lustre@vdom@vnode:element(SRC)) -> change(SRC).
replace(From, Count, With) ->
    {replace, 5, From, Count, With}.

-file("src/lustre/vdom/patch.gleam", 144).
?DOC(false).
-spec insert(list(lustre@vdom@vnode:element(SRF)), integer()) -> change(SRF).
insert(Children, Before) ->
    {insert, 6, Children, Before}.

-file("src/lustre/vdom/patch.gleam", 153).
?DOC(false).
-spec remove(integer(), integer()) -> change(any()).
remove(From, Count) ->
    {remove, 7, From, Count}.
