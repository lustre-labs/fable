-module(lustre@runtime@transport).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([client_message_to_json/1, mount/5, reconcile/1, emit/2, attribute_changed/2, event_fired/3, property_changed/2, batch/1, server_message_decoder/0]).
-export_type([client_message/1, server_message/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type client_message(SVT) :: {mount,
        integer(),
        boolean(),
        boolean(),
        list(binary()),
        list(binary()),
        lustre@vdom@vnode:element(SVT)} |
    {reconcile, integer(), lustre@vdom@patch:patch(SVT)} |
    {emit, integer(), binary(), gleam@json:json()}.

-type server_message() :: {batch, integer(), list(server_message())} |
    {attribute_changed, integer(), binary(), binary()} |
    {property_changed, integer(), binary(), gleam@dynamic:dynamic_()} |
    {event_fired, integer(), binary(), binary(), gleam@dynamic:dynamic_()}.

-file("src/lustre/runtime/transport.gleam", 123).
?DOC(false).
-spec mount_to_json(
    integer(),
    boolean(),
    boolean(),
    list(binary()),
    list(binary()),
    lustre@vdom@vnode:element(any())
) -> gleam@json:json().
mount_to_json(
    Kind,
    Open_shadow_root,
    Will_adopt_styles,
    Observed_attributes,
    Observed_properties,
    Vdom
) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"open_shadow_root"/utf8>>, gleam@json:bool(Open_shadow_root)},
            {<<"will_adopt_styles"/utf8>>, gleam@json:bool(Will_adopt_styles)},
            {<<"observed_attributes"/utf8>>,
                gleam@json:array(Observed_attributes, fun gleam@json:string/1)},
            {<<"observed_properties"/utf8>>,
                gleam@json:array(Observed_properties, fun gleam@json:string/1)},
            {<<"vdom"/utf8>>, lustre@vdom@vnode:to_json(Vdom)}]
    ).

-file("src/lustre/runtime/transport.gleam", 141).
?DOC(false).
-spec reconcile_to_json(integer(), lustre@vdom@patch:patch(any())) -> gleam@json:json().
reconcile_to_json(Kind, Patch) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"patch"/utf8>>, lustre@vdom@patch:to_json(Patch)}]
    ).

-file("src/lustre/runtime/transport.gleam", 145).
?DOC(false).
-spec emit_to_json(integer(), binary(), gleam@json:json()) -> gleam@json:json().
emit_to_json(Kind, Name, Data) ->
    gleam@json:object(
        [{<<"kind"/utf8>>, gleam@json:int(Kind)},
            {<<"name"/utf8>>, gleam@json:string(Name)},
            {<<"data"/utf8>>, Data}]
    ).

-file("src/lustre/runtime/transport.gleam", 100).
?DOC(false).
-spec client_message_to_json(client_message(any())) -> gleam@json:json().
client_message_to_json(Message) ->
    case Message of
        {mount,
            Kind,
            Open_shadow_root,
            Will_adopt_styles,
            Observed_attributes,
            Observed_properties,
            Vdom} ->
            mount_to_json(
                Kind,
                Open_shadow_root,
                Will_adopt_styles,
                Observed_attributes,
                Observed_properties,
                Vdom
            );

        {reconcile, Kind@1, Patch} ->
            reconcile_to_json(Kind@1, Patch);

        {emit, Kind@2, Name, Data} ->
            emit_to_json(Kind@2, Name, Data)
    end.

-file("src/lustre/runtime/transport.gleam", 35).
?DOC(false).
-spec mount(
    boolean(),
    boolean(),
    list(binary()),
    list(binary()),
    lustre@vdom@vnode:element(SVW)
) -> client_message(SVW).
mount(
    Open_shadow_root,
    Will_adopt_styles,
    Observed_attributes,
    Observed_properties,
    Vdom
) ->
    {mount,
        0,
        Open_shadow_root,
        Will_adopt_styles,
        Observed_attributes,
        Observed_properties,
        Vdom}.

-file("src/lustre/runtime/transport.gleam", 54).
?DOC(false).
-spec reconcile(lustre@vdom@patch:patch(SVZ)) -> client_message(SVZ).
reconcile(Patch) ->
    {reconcile, 1, Patch}.

-file("src/lustre/runtime/transport.gleam", 60).
?DOC(false).
-spec emit(binary(), gleam@json:json()) -> client_message(any()).
emit(Name, Data) ->
    {emit, 2, Name, Data}.

-file("src/lustre/runtime/transport.gleam", 66).
?DOC(false).
-spec attribute_changed(binary(), binary()) -> server_message().
attribute_changed(Name, Value) ->
    {attribute_changed, 0, Name, Value}.

-file("src/lustre/runtime/transport.gleam", 167).
?DOC(false).
-spec attribute_changed_decoder() -> gleam@dynamic@decode:decoder(server_message()).
attribute_changed_decoder() ->
    gleam@dynamic@decode:field(
        <<"name"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Name) ->
            gleam@dynamic@decode:field(
                <<"value"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Value) ->
                    gleam@dynamic@decode:success(attribute_changed(Name, Value))
                end
            )
        end
    ).

-file("src/lustre/runtime/transport.gleam", 75).
?DOC(false).
-spec event_fired(binary(), binary(), gleam@dynamic:dynamic_()) -> server_message().
event_fired(Path, Name, Event) ->
    {event_fired, 1, Path, Name, Event}.

-file("src/lustre/runtime/transport.gleam", 181).
?DOC(false).
-spec event_fired_decoder() -> gleam@dynamic@decode:decoder(server_message()).
event_fired_decoder() ->
    gleam@dynamic@decode:field(
        <<"path"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Path) ->
            gleam@dynamic@decode:field(
                <<"name"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Name) ->
                    gleam@dynamic@decode:field(
                        <<"event"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                        fun(Event) ->
                            gleam@dynamic@decode:success(
                                event_fired(Path, Name, Event)
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre/runtime/transport.gleam", 85).
?DOC(false).
-spec property_changed(binary(), gleam@dynamic:dynamic_()) -> server_message().
property_changed(Name, Value) ->
    {property_changed, 2, Name, Value}.

-file("src/lustre/runtime/transport.gleam", 174).
?DOC(false).
-spec property_changed_decoder() -> gleam@dynamic@decode:decoder(server_message()).
property_changed_decoder() ->
    gleam@dynamic@decode:field(
        <<"name"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Name) ->
            gleam@dynamic@decode:field(
                <<"value"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                fun(Value) ->
                    gleam@dynamic@decode:success(property_changed(Name, Value))
                end
            )
        end
    ).

-file("src/lustre/runtime/transport.gleam", 94).
?DOC(false).
-spec batch(list(server_message())) -> server_message().
batch(Messages) ->
    {batch, 3, Messages}.

-file("src/lustre/runtime/transport.gleam", 189).
?DOC(false).
-spec batch_decoder() -> gleam@dynamic@decode:decoder(server_message()).
batch_decoder() ->
    gleam@dynamic@decode:field(
        <<"messages"/utf8>>,
        gleam@dynamic@decode:list(server_message_decoder()),
        fun(Messages) -> gleam@dynamic@decode:success(batch(Messages)) end
    ).

-file("src/lustre/runtime/transport.gleam", 155).
?DOC(false).
-spec server_message_decoder() -> gleam@dynamic@decode:decoder(server_message()).
server_message_decoder() ->
    gleam@dynamic@decode:field(
        <<"kind"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        fun(Kind) -> case Kind of
                _ when Kind =:= 0 ->
                    attribute_changed_decoder();

                _ when Kind =:= 2 ->
                    property_changed_decoder();

                _ when Kind =:= 1 ->
                    event_fired_decoder();

                _ when Kind =:= 3 ->
                    batch_decoder();

                _ ->
                    gleam@dynamic@decode:failure(batch([]), <<""/utf8>>)
            end end
    ).
