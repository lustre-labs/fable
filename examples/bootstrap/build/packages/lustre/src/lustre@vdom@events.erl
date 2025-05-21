-module(lustre@vdom@events).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, tick/1, remove_event/3, handle/4, has_dispatched_events/2, add_event/5, compose_mapper/2, remove_child/4, add_child/5, from_node/1, add_children/5]).
-export_type([events/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque events(OZC) :: {events,
        lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(OZC)),
        list(binary()),
        list(binary())}.

-file("src/lustre/vdom/events.gleam", 43).
?DOC(false).
-spec new() -> events(any()).
new() ->
    {events, gleam@dict:new(), [], []}.

-file("src/lustre/vdom/events.gleam", 55).
?DOC(false).
-spec tick(events(OZI)) -> events(OZI).
tick(Events) ->
    {events, erlang:element(2, Events), erlang:element(4, Events), []}.

-file("src/lustre/vdom/events.gleam", 99).
?DOC(false).
-spec do_remove_event(
    lustre@internals@mutable_map:mutable_map(binary(), PAA),
    lustre@vdom@path:path(),
    binary()
) -> lustre@internals@mutable_map:mutable_map(binary(), PAA).
do_remove_event(Handlers, Path, Name) ->
    gleam@dict:delete(Handlers, lustre@vdom@path:event(Path, Name)).

-file("src/lustre/vdom/events.gleam", 90).
?DOC(false).
-spec remove_event(events(OZX), lustre@vdom@path:path(), binary()) -> events(OZX).
remove_event(Events, Path, Name) ->
    Handlers = do_remove_event(erlang:element(2, Events), Path, Name),
    _record = Events,
    {events, Handlers, erlang:element(3, _record), erlang:element(4, _record)}.

-file("src/lustre/vdom/events.gleam", 236).
?DOC(false).
-spec remove_attributes(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PCA)),
    lustre@vdom@path:path(),
    list(lustre@vdom@vattr:attribute(PCA))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PCA)).
remove_attributes(Handlers, Path, Attributes) ->
    gleam@list:fold(
        Attributes,
        Handlers,
        fun(Events, Attribute) -> case Attribute of
                {event, _, Name, _, _, _, _, _, _, _} ->
                    do_remove_event(Events, Path, Name);

                _ ->
                    Events
            end end
    ).

-file("src/lustre/vdom/events.gleam", 268).
?DOC(false).
-spec handle(events(PCS), binary(), binary(), gleam@dynamic:dynamic_()) -> {events(PCS),
    {ok, PCS} | {error, list(gleam@dynamic@decode:decode_error())}}.
handle(Events, Path, Name, Event) ->
    Next_dispatched_paths = [Path | erlang:element(4, Events)],
    Events@1 = begin
        _record = Events,
        {events,
            erlang:element(2, _record),
            erlang:element(3, _record),
            Next_dispatched_paths}
    end,
    case gleam@dict:get(
        erlang:element(2, Events@1),
        <<<<Path/binary, (<<"\f"/utf8>>)/binary>>/binary, Name/binary>>
    ) of
        {ok, Handler} ->
            {Events@1, gleam@dynamic@decode:run(Event, Handler)};

        {error, _} ->
            {Events@1, {error, []}}
    end.

-file("src/lustre/vdom/events.gleam", 283).
?DOC(false).
-spec has_dispatched_events(events(any()), lustre@vdom@path:path()) -> boolean().
has_dispatched_events(Events, Path) ->
    lustre@vdom@path:matches(Path, erlang:element(3, Events)).

-file("src/lustre/vdom/events.gleam", 76).
?DOC(false).
-spec do_add_event(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(OZP)),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    binary(),
    gleam@dynamic@decode:decoder(OZP)
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(OZP)).
do_add_event(Handlers, Mapper, Path, Name, Handler) ->
    gleam@dict:insert(
        Handlers,
        lustre@vdom@path:event(Path, Name),
        gleam@dynamic@decode:map(Handler, gleam@function:identity(Mapper))
    ).

-file("src/lustre/vdom/events.gleam", 65).
?DOC(false).
-spec add_event(
    events(OZL),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    binary(),
    gleam@dynamic@decode:decoder(OZL)
) -> events(OZL).
add_event(Events, Mapper, Path, Name, Handler) ->
    Handlers = do_add_event(
        erlang:element(2, Events),
        Mapper,
        Path,
        Name,
        Handler
    ),
    _record = Events,
    {events, Handlers, erlang:element(3, _record), erlang:element(4, _record)}.

-file("src/lustre/vdom/events.gleam", 153).
?DOC(false).
-spec add_attributes(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PAR)),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    list(lustre@vdom@vattr:attribute(PAR))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PAR)).
add_attributes(Handlers, Mapper, Path, Attributes) ->
    gleam@list:fold(
        Attributes,
        Handlers,
        fun(Events, Attribute) -> case Attribute of
                {event, _, Name, Handler, _, _, _, _, _, _} ->
                    do_add_event(Events, Mapper, Path, Name, Handler);

                _ ->
                    Events
            end end
    ).

-file("src/lustre/vdom/events.gleam", 292).
?DOC(false).
-spec is_reference_equal(PDD, PDD) -> boolean().
is_reference_equal(A, B) ->
    A =:= B.

-file("src/lustre/vdom/events.gleam", 28).
?DOC(false).
-spec compose_mapper(
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_())
) -> fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()).
compose_mapper(Mapper, Child_mapper) ->
    case {is_reference_equal(Mapper, fun gleam@function:identity/1),
        is_reference_equal(Child_mapper, fun gleam@function:identity/1)} of
        {_, true} ->
            Mapper;

        {true, false} ->
            Child_mapper;

        {false, false} ->
            fun(Msg) -> Mapper(Child_mapper(Msg)) end
    end.

-file("src/lustre/vdom/events.gleam", 248).
?DOC(false).
-spec do_remove_children(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PCJ)),
    lustre@vdom@path:path(),
    integer(),
    list(lustre@vdom@vnode:element(PCJ))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PCJ)).
do_remove_children(Handlers, Path, Child_index, Children) ->
    case Children of
        [] ->
            Handlers;

        [Child | Rest] ->
            _pipe = Handlers,
            _pipe@1 = do_remove_child(_pipe, Path, Child_index, Child),
            do_remove_children(
                _pipe@1,
                Path,
                Child_index + lustre@vdom@vnode:advance(Child),
                Rest
            )
    end.

-file("src/lustre/vdom/events.gleam", 208).
?DOC(false).
-spec do_remove_child(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PBS)),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(PBS)
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PBS)).
do_remove_child(Handlers, Parent, Child_index, Child) ->
    case Child of
        {element, _, _, _, _, _, Attributes, Children, _, _, _} ->
            Path = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            _pipe = Handlers,
            _pipe@1 = remove_attributes(_pipe, Path, Attributes),
            do_remove_children(_pipe@1, Path, 0, Children);

        {fragment, _, _, _, Children@1, _, _} ->
            do_remove_children(Handlers, Parent, Child_index + 1, Children@1);

        {unsafe_inner_html, _, _, _, _, _, Attributes@1, _} ->
            Path@1 = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            remove_attributes(Handlers, Path@1, Attributes@1);

        {text, _, _, _, _} ->
            Handlers
    end.

-file("src/lustre/vdom/events.gleam", 198).
?DOC(false).
-spec remove_child(
    events(PBO),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(PBO)
) -> events(PBO).
remove_child(Events, Parent, Child_index, Child) ->
    Handlers = do_remove_child(
        erlang:element(2, Events),
        Parent,
        Child_index,
        Child
    ),
    _record = Events,
    {events, Handlers, erlang:element(3, _record), erlang:element(4, _record)}.

-file("src/lustre/vdom/events.gleam", 181).
?DOC(false).
-spec do_add_children(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PBF)),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    list(lustre@vdom@vnode:element(PBF))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PBF)).
do_add_children(Handlers, Mapper, Path, Child_index, Children) ->
    case Children of
        [] ->
            Handlers;

        [Child | Rest] ->
            _pipe = Handlers,
            _pipe@1 = do_add_child(_pipe, Mapper, Path, Child_index, Child),
            do_add_children(
                _pipe@1,
                Mapper,
                Path,
                Child_index + lustre@vdom@vnode:advance(Child),
                Rest
            )
    end.

-file("src/lustre/vdom/events.gleam", 118).
?DOC(false).
-spec do_add_child(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PAJ)),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(PAJ)
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(PAJ)).
do_add_child(Handlers, Mapper, Parent, Child_index, Child) ->
    case Child of
        {element, _, _, _, _, _, Attributes, Children, _, _, _} ->
            Path = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            Composed_mapper = compose_mapper(Mapper, erlang:element(4, Child)),
            _pipe = Handlers,
            _pipe@1 = add_attributes(_pipe, Composed_mapper, Path, Attributes),
            do_add_children(_pipe@1, Composed_mapper, Path, 0, Children);

        {fragment, _, _, _, Children@1, _, _} ->
            Composed_mapper@1 = compose_mapper(Mapper, erlang:element(4, Child)),
            Child_index@1 = Child_index + 1,
            do_add_children(
                Handlers,
                Composed_mapper@1,
                Parent,
                Child_index@1,
                Children@1
            );

        {unsafe_inner_html, _, _, _, _, _, Attributes@1, _} ->
            Path@1 = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            Composed_mapper@2 = compose_mapper(Mapper, erlang:element(4, Child)),
            add_attributes(Handlers, Composed_mapper@2, Path@1, Attributes@1);

        {text, _, _, _, _} ->
            Handlers
    end.

-file("src/lustre/vdom/events.gleam", 107).
?DOC(false).
-spec add_child(
    events(PAF),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(PAF)
) -> events(PAF).
add_child(Events, Mapper, Parent, Index, Child) ->
    Handlers = do_add_child(
        erlang:element(2, Events),
        Mapper,
        Parent,
        Index,
        Child
    ),
    _record = Events,
    {events, Handlers, erlang:element(3, _record), erlang:element(4, _record)}.

-file("src/lustre/vdom/events.gleam", 51).
?DOC(false).
-spec from_node(lustre@vdom@vnode:element(OZF)) -> events(OZF).
from_node(Root) ->
    add_child(new(), fun gleam@function:identity/1, root, 0, Root).

-file("src/lustre/vdom/events.gleam", 169).
?DOC(false).
-spec add_children(
    events(PBA),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    list(lustre@vdom@vnode:element(PBA))
) -> events(PBA).
add_children(Events, Mapper, Path, Child_index, Children) ->
    Handlers = do_add_children(
        erlang:element(2, Events),
        Mapper,
        Path,
        Child_index,
        Children
    ),
    _record = Events,
    {events, Handlers, erlang:element(3, _record), erlang:element(4, _record)}.
