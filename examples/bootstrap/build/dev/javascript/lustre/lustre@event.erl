-module(lustre@event).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([emit/2, on/2, prevent_default/1, stop_propagation/1, debounce/2, throttle/2, on_click/1, on_mouse_down/1, on_mouse_up/1, on_mouse_enter/1, on_mouse_leave/1, on_mouse_over/1, on_mouse_out/1, on_keypress/1, on_keydown/1, on_keyup/1, on_input/1, on_change/1, on_check/1, on_submit/1, on_focus/1, on_blur/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/event.gleam", 21).
?DOC(
    " Dispatches a custom message from a Lustre component. This lets components\n"
    " communicate with their parents the same way native DOM elements do.\n"
    "\n"
    " Any JSON-serialisable payload can be attached as additional data for any\n"
    " event listeners to decode. This data will be on the event's `detail` property.\n"
).
-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-file("src/lustre/event.gleam", 52).
-spec is_immediate_event(binary()) -> boolean().
is_immediate_event(Name) ->
    case Name of
        <<"input"/utf8>> ->
            true;

        <<"change"/utf8>> ->
            true;

        <<"focus"/utf8>> ->
            true;

        <<"focusin"/utf8>> ->
            true;

        <<"focusout"/utf8>> ->
            true;

        <<"blur"/utf8>> ->
            true;

        <<"select"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/lustre/event.gleam", 39).
?DOC(
    " Listens for the given event and then runs the given decoder on the event\n"
    " object. If the decoder succeeds, the decoded event is dispatched to your\n"
    " application's `update` function. If it fails, the event is silently ignored.\n"
    "\n"
    " The event name is typically an all-lowercase string such as \"click\" or \"mousemove\".\n"
    " If you're listening for non-standard events (like those emitted by a custom\n"
    " element) their event names might be slightly different.\n"
    "\n"
    " > **Note**: if you are developing a server component, it is important to also\n"
    " > use [`server_component.include`](./server_component.html#include) to state\n"
    " > which properties of the event you need to be sent to the server.\n"
).
-spec on(binary(), gleam@dynamic@decode:decoder(UUQ)) -> lustre@vdom@vattr:attribute(UUQ).
on(Name, Handler) ->
    lustre@vdom@vattr:event(
        Name,
        Handler,
        [],
        false,
        false,
        is_immediate_event(Name),
        0,
        0
    ).

-file("src/lustre/event.gleam", 63).
?DOC(
    " Indicate that the event should have its default behaviour cancelled. This is\n"
    " equivalent to calling `event.preventDefault()` in JavaScript.\n"
).
-spec prevent_default(lustre@vdom@vattr:attribute(UUT)) -> lustre@vdom@vattr:attribute(UUT).
prevent_default(Event) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            _record = Event,
            {event,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                true,
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record),
                erlang:element(10, _record)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 73).
?DOC(
    " Indicate that the event should not propagate to parent elements. This is\n"
    " equivalent to calling `event.stopPropagation()` in JavaScript.\n"
).
-spec stop_propagation(lustre@vdom@vattr:attribute(UUW)) -> lustre@vdom@vattr:attribute(UUW).
stop_propagation(Event) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            _record = Event,
            {event,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                true,
                erlang:element(8, _record),
                erlang:element(9, _record),
                erlang:element(10, _record)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 96).
?DOC(
    " Use Lustre's built-in event debouncing to wait a delay after a burst of\n"
    " events before dispatching the most recent one. You can visualise debounced\n"
    " events like so:\n"
    "\n"
    " ```\n"
    "  original : --a-b-cd--e----------f--------\n"
    " debounced : ---------------e----------f---\n"
    " ```\n"
    "\n"
    " This is particularly useful for server components where many events in quick\n"
    " succession can introduce problems because of network latency.\n"
    "\n"
    " > **Note**: debounced events inherently introduce latency. Try to consider\n"
    " > typical interaction patterns and experiment with different delays to balance\n"
    " > responsiveness and update frequency.\n"
).
-spec debounce(lustre@vdom@vattr:attribute(UUZ), integer()) -> lustre@vdom@vattr:attribute(UUZ).
debounce(Event, Delay) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            _record = Event,
            {event,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                gleam@int:max(0, Delay),
                erlang:element(10, _record)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 119).
?DOC(
    " Use Lustre's built-in event throttling to restrict the number of events\n"
    " that can be dispatched in a given time period. You can visualise throttled\n"
    " events like so:\n"
    "\n"
    " ```\n"
    " original : --a-b-cd--e----------f--------\n"
    " throttled : -a------ e----------e--------\n"
    " ```\n"
    "\n"
    " This is particularly useful for server components where many events in quick\n"
    " succession can introduce problems because of network latency.\n"
    "\n"
    " > **Note**: throttled events inherently reduce precision. Try to consider\n"
    " > typical interaction patterns and experiment with different delays to balance\n"
    " > responsiveness and update frequency.\n"
).
-spec throttle(lustre@vdom@vattr:attribute(UVC), integer()) -> lustre@vdom@vattr:attribute(UVC).
throttle(Event, Delay) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            _record = Event,
            {event,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record),
                gleam@int:max(0, Delay)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 129).
?DOC("\n").
-spec on_click(UVF) -> lustre@vdom@vattr:attribute(UVF).
on_click(Msg) ->
    on(<<"click"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 134).
?DOC("\n").
-spec on_mouse_down(UVH) -> lustre@vdom@vattr:attribute(UVH).
on_mouse_down(Msg) ->
    on(<<"mousedown"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 139).
?DOC("\n").
-spec on_mouse_up(UVJ) -> lustre@vdom@vattr:attribute(UVJ).
on_mouse_up(Msg) ->
    on(<<"mouseup"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 144).
?DOC("\n").
-spec on_mouse_enter(UVL) -> lustre@vdom@vattr:attribute(UVL).
on_mouse_enter(Msg) ->
    on(<<"mouseenter"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 149).
?DOC("\n").
-spec on_mouse_leave(UVN) -> lustre@vdom@vattr:attribute(UVN).
on_mouse_leave(Msg) ->
    on(<<"mouseleave"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 154).
?DOC("\n").
-spec on_mouse_over(UVP) -> lustre@vdom@vattr:attribute(UVP).
on_mouse_over(Msg) ->
    on(<<"mouseover"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 159).
?DOC("\n").
-spec on_mouse_out(UVR) -> lustre@vdom@vattr:attribute(UVR).
on_mouse_out(Msg) ->
    on(<<"mouseout"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 168).
?DOC(
    " Listens for key presses on an element, and dispatches a message with the\n"
    " current key being pressed.\n"
).
-spec on_keypress(fun((binary()) -> UVT)) -> lustre@vdom@vattr:attribute(UVT).
on_keypress(Msg) ->
    on(
        <<"keypress"/utf8>>,
        begin
            gleam@dynamic@decode:field(
                <<"key"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Key) -> _pipe = Key,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ).

-file("src/lustre/event.gleam", 179).
?DOC(
    " Listens for key down events on an element, and dispatches a message with the\n"
    " current key being pressed.\n"
).
-spec on_keydown(fun((binary()) -> UVV)) -> lustre@vdom@vattr:attribute(UVV).
on_keydown(Msg) ->
    on(
        <<"keydown"/utf8>>,
        begin
            gleam@dynamic@decode:field(
                <<"key"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Key) -> _pipe = Key,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ).

-file("src/lustre/event.gleam", 190).
?DOC(
    " Listens for key up events on an element, and dispatches a message with the\n"
    " current key being released.\n"
).
-spec on_keyup(fun((binary()) -> UVX)) -> lustre@vdom@vattr:attribute(UVX).
on_keyup(Msg) ->
    on(
        <<"keyup"/utf8>>,
        begin
            gleam@dynamic@decode:field(
                <<"key"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Key) -> _pipe = Key,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ).

-file("src/lustre/event.gleam", 205).
?DOC(
    " Listens for input events on elements such as `<input>`, `<textarea>` and\n"
    " `<select>`. This handler automatically decodes the string value of the input\n"
    " and passes it to the given message function. This is commonly used to\n"
    " implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
).
-spec on_input(fun((binary()) -> UVZ)) -> lustre@vdom@vattr:attribute(UVZ).
on_input(Msg) ->
    on(
        <<"input"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"target"/utf8>>, <<"value"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Value) -> gleam@dynamic@decode:success(Msg(Value)) end
            )
        end
    ).

-file("src/lustre/event.gleam", 218).
?DOC(
    " Listens for change events on elements such as `<input>`, `<textarea>` and\n"
    " `<select>`. This handler automatically decodes the string value of the input\n"
    " and passes it to the given message function. This is commonly used to\n"
    " implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
).
-spec on_change(fun((binary()) -> UWB)) -> lustre@vdom@vattr:attribute(UWB).
on_change(Msg) ->
    on(
        <<"change"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"target"/utf8>>, <<"value"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Value) -> gleam@dynamic@decode:success(Msg(Value)) end
            )
        end
    ).

-file("src/lustre/event.gleam", 231).
?DOC(
    " Listens for change events on `<input type=\"checkbox\">` elements. This handler\n"
    " automatically decodes the boolean value of the checkbox and passes it to\n"
    " the given message function. This is commonly used to implement\n"
    " [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
).
-spec on_check(fun((boolean()) -> UWD)) -> lustre@vdom@vattr:attribute(UWD).
on_check(Msg) ->
    on(
        <<"change"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"target"/utf8>>, <<"checked"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_bool/1},
                fun(Checked) -> gleam@dynamic@decode:success(Msg(Checked)) end
            )
        end
    ).

-file("src/lustre/event.gleam", 262).
-spec formdata_decoder() -> gleam@dynamic@decode:decoder(list({binary(),
    binary()})).
formdata_decoder() ->
    String_value_decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Key) ->
                gleam@dynamic@decode:field(
                    1,
                    gleam@dynamic@decode:one_of(
                        gleam@dynamic@decode:map(
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            fun(Field@0) -> {ok, Field@0} end
                        ),
                        [gleam@dynamic@decode:success({error, nil})]
                    ),
                    fun(Value) -> _pipe = Value,
                        _pipe@1 = gleam@result:map(
                            _pipe,
                            fun(_capture) -> gleam@pair:new(Key, _capture) end
                        ),
                        gleam@dynamic@decode:success(_pipe@1) end
                )
            end
        )
    end,
    _pipe@2 = String_value_decoder,
    _pipe@3 = gleam@dynamic@decode:list(_pipe@2),
    gleam@dynamic@decode:map(_pipe@3, fun gleam@result:values/1).

-file("src/lustre/event.gleam", 251).
?DOC(
    " Listens for submit events on a `<form>` element and receives a list of\n"
    " name/value pairs for each field in the form. Files are not included in this\n"
    " list: if you need them, you can write your own handler for the `\"submit\"`\n"
    " event and decode the non-standard `detail.formData` property manually.\n"
    "\n"
    " This handler is best paired with the [`formal`](https://hexdocs.pm/formal/)\n"
    " package which lets you process form submissions in a type-safe way.\n"
    "\n"
    " This will automatically call [`prevent_default`](#prevent_default) to stop\n"
    " the browser's native form submission. In a Lustre app you'll want to handle\n"
    " that yourself as an [`Effect`](./effect.html#Effect).\n"
).
-spec on_submit(fun((list({binary(), binary()})) -> UWG)) -> lustre@vdom@vattr:attribute(UWG).
on_submit(Msg) ->
    _pipe@2 = on(
        <<"submit"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"detail"/utf8>>, <<"formData"/utf8>>],
                formdata_decoder(),
                fun(Formdata) -> _pipe = Formdata,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ),
    prevent_default(_pipe@2).

-file("src/lustre/event.gleam", 286).
-spec on_focus(UWK) -> lustre@vdom@vattr:attribute(UWK).
on_focus(Msg) ->
    on(<<"focus"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 290).
-spec on_blur(UWM) -> lustre@vdom@vattr:attribute(UWM).
on_blur(Msg) ->
    on(<<"blur"/utf8>>, gleam@dynamic@decode:success(Msg)).
