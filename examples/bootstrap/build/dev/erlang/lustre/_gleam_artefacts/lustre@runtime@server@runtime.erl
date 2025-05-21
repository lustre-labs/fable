-module(lustre@runtime@server@runtime).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/4]).
-export_type([state/2, config/1, message/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type state(TKR, TKS) :: {state,
        gleam@erlang@process:subject(message(TKS)),
        gleam@erlang@process:selector(message(TKS)),
        gleam@erlang@process:selector(message(TKS)),
        TKR,
        fun((TKR, TKS) -> {TKR, lustre@effect:effect(TKS)}),
        fun((TKR) -> lustre@vdom@vnode:element(TKS)),
        config(TKS),
        lustre@vdom@vnode:element(TKS),
        lustre@vdom@events:events(TKS),
        gleam@dict:dict(gleam@erlang@process:subject(lustre@runtime@transport:client_message(TKS)), gleam@erlang@process:process_monitor()),
        gleam@set:set(fun((lustre@runtime@transport:client_message(TKS)) -> nil))}.

-type config(TKT) :: {config,
        boolean(),
        boolean(),
        gleam@dict:dict(binary(), fun((binary()) -> {ok, TKT} | {error, nil})),
        gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(TKT))}.

-type message(TKU) :: {client_dispatched_message,
        lustre@runtime@transport:server_message()} |
    {client_registered_subject,
        gleam@erlang@process:subject(lustre@runtime@transport:client_message(TKU))} |
    {client_deregistered_subject,
        gleam@erlang@process:subject(lustre@runtime@transport:client_message(TKU))} |
    {client_registered_callback,
        fun((lustre@runtime@transport:client_message(TKU)) -> nil)} |
    {client_deregistered_callback,
        fun((lustre@runtime@transport:client_message(TKU)) -> nil)} |
    {effect_added_selector, gleam@erlang@process:selector(message(TKU))} |
    {effect_dispatched_message, TKU} |
    {effect_emit_event, binary(), gleam@json:json()} |
    {self_dispatched_messages, list(TKU), lustre@effect:effect(TKU)} |
    system_requested_shutdown.

-file("src/lustre/runtime/server/runtime.gleam", 315).
?DOC(false).
-spec handle_attribute_change(
    gleam@dict:dict(binary(), fun((binary()) -> {ok, TLY} | {error, nil})),
    binary(),
    binary()
) -> {ok, TLY} | {error, nil}.
handle_attribute_change(Attributes, Name, Value) ->
    case gleam_stdlib:map_get(Attributes, Name) of
        {error, _} ->
            {error, nil};

        {ok, Handler} ->
            Handler(Value)
    end.

-file("src/lustre/runtime/server/runtime.gleam", 326).
?DOC(false).
-spec handle_property_change(
    gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(TMF)),
    binary(),
    gleam@dynamic:dynamic_()
) -> {ok, TMF} | {error, nil}.
handle_property_change(Properties, Name, Value) ->
    case gleam_stdlib:map_get(Properties, Name) of
        {error, _} ->
            {error, nil};

        {ok, Decoder} ->
            _pipe = gleam@dynamic@decode:run(Value, Decoder),
            gleam@result:replace_error(_pipe, nil)
    end.

-file("src/lustre/runtime/server/runtime.gleam", 338).
?DOC(false).
-spec handle_effect(
    gleam@erlang@process:subject(message(TML)),
    lustre@effect:effect(TML)
) -> nil.
handle_effect(Self, Effect) ->
    Send = fun(_capture) -> gleam@erlang@process:send(Self, _capture) end,
    Dispatch = fun(Message) -> Send({effect_dispatched_message, Message}) end,
    Emit = fun(Name, Data) -> Send({effect_emit_event, Name, Data}) end,
    Select = fun(Selector) -> _pipe = Selector,
        _pipe@1 = gleam_erlang_ffi:map_selector(
            _pipe,
            fun(Field@0) -> {effect_dispatched_message, Field@0} end
        ),
        _pipe@2 = {effect_added_selector, _pipe@1},
        Send(_pipe@2) end,
    Internals = fun() -> gleam@dynamic:nil() end,
    lustre@effect:perform(Effect, Dispatch, Emit, Select, Internals).

-file("src/lustre/runtime/server/runtime.gleam", 266).
?DOC(false).
-spec handle_client_message(
    state(TLS, TLT),
    lustre@runtime@transport:server_message()
) -> state(TLS, TLT).
handle_client_message(State, Message) ->
    case Message of
        {batch, _, Messages} ->
            gleam@list:fold(Messages, State, fun handle_client_message/2);

        {attribute_changed, _, Name, Value} ->
            case handle_attribute_change(
                erlang:element(4, erlang:element(8, State)),
                Name,
                Value
            ) of
                {error, _} ->
                    State;

                {ok, Msg} ->
                    {Model, Effect} = (erlang:element(6, State))(
                        erlang:element(5, State),
                        Msg
                    ),
                    Vdom = (erlang:element(7, State))(Model),
                    handle_effect(erlang:element(2, State), Effect),
                    _record = State,
                    {state,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        erlang:element(4, _record),
                        Model,
                        erlang:element(6, _record),
                        erlang:element(7, _record),
                        erlang:element(8, _record),
                        Vdom,
                        erlang:element(10, _record),
                        erlang:element(11, _record),
                        erlang:element(12, _record)}
            end;

        {property_changed, _, Name@1, Value@1} ->
            case handle_property_change(
                erlang:element(5, erlang:element(8, State)),
                Name@1,
                Value@1
            ) of
                {error, _} ->
                    State;

                {ok, Msg@1} ->
                    {Model@1, Effect@1} = (erlang:element(6, State))(
                        erlang:element(5, State),
                        Msg@1
                    ),
                    Vdom@1 = (erlang:element(7, State))(Model@1),
                    handle_effect(erlang:element(2, State), Effect@1),
                    _record@1 = State,
                    {state,
                        erlang:element(2, _record@1),
                        erlang:element(3, _record@1),
                        erlang:element(4, _record@1),
                        Model@1,
                        erlang:element(6, _record@1),
                        erlang:element(7, _record@1),
                        erlang:element(8, _record@1),
                        Vdom@1,
                        erlang:element(10, _record@1),
                        erlang:element(11, _record@1),
                        erlang:element(12, _record@1)}
            end;

        {event_fired, _, Path, Name@2, Event} ->
            case lustre@vdom@events:handle(
                erlang:element(10, State),
                Path,
                Name@2,
                Event
            ) of
                {Events, {error, _}} ->
                    _record@2 = State,
                    {state,
                        erlang:element(2, _record@2),
                        erlang:element(3, _record@2),
                        erlang:element(4, _record@2),
                        erlang:element(5, _record@2),
                        erlang:element(6, _record@2),
                        erlang:element(7, _record@2),
                        erlang:element(8, _record@2),
                        erlang:element(9, _record@2),
                        Events,
                        erlang:element(11, _record@2),
                        erlang:element(12, _record@2)};

                {Events@1, {ok, Message@1}} ->
                    {Model@2, Effect@2} = (erlang:element(6, State))(
                        erlang:element(5, State),
                        Message@1
                    ),
                    Vdom@2 = (erlang:element(7, State))(Model@2),
                    handle_effect(erlang:element(2, State), Effect@2),
                    _record@3 = State,
                    {state,
                        erlang:element(2, _record@3),
                        erlang:element(3, _record@3),
                        erlang:element(4, _record@3),
                        Model@2,
                        erlang:element(6, _record@3),
                        erlang:element(7, _record@3),
                        erlang:element(8, _record@3),
                        Vdom@2,
                        Events@1,
                        erlang:element(11, _record@3),
                        erlang:element(12, _record@3)}
            end
    end.

-file("src/lustre/runtime/server/runtime.gleam", 356).
?DOC(false).
-spec broadcast(
    gleam@dict:dict(gleam@erlang@process:subject(lustre@runtime@transport:client_message(TMP)), gleam@erlang@process:process_monitor()),
    gleam@set:set(fun((lustre@runtime@transport:client_message(TMP)) -> nil)),
    lustre@runtime@transport:client_message(TMP)
) -> nil.
broadcast(Clients, Callbacks, Message) ->
    gleam@dict:each(
        Clients,
        fun(Client, _) -> gleam@erlang@process:send(Client, Message) end
    ),
    gleam@set:each(Callbacks, fun(Callback) -> Callback(Message) end).

-file("src/lustre/runtime/server/runtime.gleam", 114).
?DOC(false).
-spec loop(message(TLI), state(TLK, TLI)) -> gleam@otp@actor:next(message(TLI), state(TLK, TLI)).
loop(Message, State) ->
    case Message of
        {client_dispatched_message, Message@1} ->
            Next = handle_client_message(State, Message@1),
            {diff, Patch, Events} = lustre@vdom@diff:diff(
                erlang:element(10, State),
                erlang:element(9, State),
                erlang:element(9, Next)
            ),
            broadcast(
                erlang:element(11, State),
                erlang:element(12, State),
                lustre@runtime@transport:reconcile(Patch)
            ),
            gleam@otp@actor:continue(
                begin
                    _record = Next,
                    {state,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        erlang:element(4, _record),
                        erlang:element(5, _record),
                        erlang:element(6, _record),
                        erlang:element(7, _record),
                        erlang:element(8, _record),
                        erlang:element(9, _record),
                        Events,
                        erlang:element(11, _record),
                        erlang:element(12, _record)}
                end
            );

        {client_registered_subject, Client} ->
            case gleam@dict:has_key(erlang:element(11, State), Client) of
                true ->
                    gleam@otp@actor:continue(State);

                false ->
                    Monitor = gleam@erlang@process:monitor_process(
                        gleam@erlang@process:subject_owner(Client)
                    ),
                    Subscribers = gleam@dict:insert(
                        erlang:element(11, State),
                        Client,
                        Monitor
                    ),
                    Selector@1 = begin
                        gleam@dict:fold(
                            Subscribers,
                            erlang:element(4, State),
                            fun(Selector, Client@1, Monitor@1) ->
                                gleam@erlang@process:selecting_process_down(
                                    Selector,
                                    Monitor@1,
                                    fun(_) ->
                                        {client_deregistered_subject, Client@1}
                                    end
                                )
                            end
                        )
                    end,
                    gleam@erlang@process:send(
                        Client,
                        lustre@runtime@transport:mount(
                            erlang:element(2, erlang:element(8, State)),
                            erlang:element(3, erlang:element(8, State)),
                            maps:keys(
                                erlang:element(4, erlang:element(8, State))
                            ),
                            maps:keys(
                                erlang:element(5, erlang:element(8, State))
                            ),
                            erlang:element(9, State)
                        )
                    ),
                    {continue,
                        begin
                            _record@1 = State,
                            {state,
                                erlang:element(2, _record@1),
                                Selector@1,
                                erlang:element(4, _record@1),
                                erlang:element(5, _record@1),
                                erlang:element(6, _record@1),
                                erlang:element(7, _record@1),
                                erlang:element(8, _record@1),
                                erlang:element(9, _record@1),
                                erlang:element(10, _record@1),
                                Subscribers,
                                erlang:element(12, _record@1)}
                        end,
                        {some, Selector@1}}
            end;

        {client_deregistered_subject, Client@2} ->
            case gleam_stdlib:map_get(erlang:element(11, State), Client@2) of
                {error, _} ->
                    gleam@otp@actor:continue(State);

                {ok, Monitor@2} ->
                    _ = gleam@erlang@process:demonitor_process(Monitor@2),
                    Subscribers@1 = gleam@dict:delete(
                        erlang:element(11, State),
                        Client@2
                    ),
                    Selector@3 = begin
                        gleam@dict:fold(
                            Subscribers@1,
                            erlang:element(4, State),
                            fun(Selector@2, Client@3, Monitor@3) ->
                                Unsubscribe = fun(_) ->
                                    {client_deregistered_subject, Client@3}
                                end,
                                gleam@erlang@process:selecting_process_down(
                                    Selector@2,
                                    Monitor@3,
                                    Unsubscribe
                                )
                            end
                        )
                    end,
                    {continue,
                        begin
                            _record@2 = State,
                            {state,
                                erlang:element(2, _record@2),
                                Selector@3,
                                erlang:element(4, _record@2),
                                erlang:element(5, _record@2),
                                erlang:element(6, _record@2),
                                erlang:element(7, _record@2),
                                erlang:element(8, _record@2),
                                erlang:element(9, _record@2),
                                erlang:element(10, _record@2),
                                Subscribers@1,
                                erlang:element(12, _record@2)}
                        end,
                        {some, Selector@3}}
            end;

        {client_registered_callback, Callback} ->
            case gleam@set:contains(erlang:element(12, State), Callback) of
                true ->
                    gleam@otp@actor:continue(State);

                false ->
                    Callbacks = gleam@set:insert(
                        erlang:element(12, State),
                        Callback
                    ),
                    Callback(
                        lustre@runtime@transport:mount(
                            erlang:element(2, erlang:element(8, State)),
                            erlang:element(3, erlang:element(8, State)),
                            maps:keys(
                                erlang:element(4, erlang:element(8, State))
                            ),
                            maps:keys(
                                erlang:element(5, erlang:element(8, State))
                            ),
                            erlang:element(9, State)
                        )
                    ),
                    gleam@otp@actor:continue(
                        begin
                            _record@3 = State,
                            {state,
                                erlang:element(2, _record@3),
                                erlang:element(3, _record@3),
                                erlang:element(4, _record@3),
                                erlang:element(5, _record@3),
                                erlang:element(6, _record@3),
                                erlang:element(7, _record@3),
                                erlang:element(8, _record@3),
                                erlang:element(9, _record@3),
                                erlang:element(10, _record@3),
                                erlang:element(11, _record@3),
                                Callbacks}
                        end
                    )
            end;

        {client_deregistered_callback, Callback@1} ->
            case gleam@set:contains(erlang:element(12, State), Callback@1) of
                false ->
                    gleam@otp@actor:continue(State);

                true ->
                    Callbacks@1 = gleam@set:delete(
                        erlang:element(12, State),
                        Callback@1
                    ),
                    gleam@otp@actor:continue(
                        begin
                            _record@4 = State,
                            {state,
                                erlang:element(2, _record@4),
                                erlang:element(3, _record@4),
                                erlang:element(4, _record@4),
                                erlang:element(5, _record@4),
                                erlang:element(6, _record@4),
                                erlang:element(7, _record@4),
                                erlang:element(8, _record@4),
                                erlang:element(9, _record@4),
                                erlang:element(10, _record@4),
                                erlang:element(11, _record@4),
                                Callbacks@1}
                        end
                    )
            end;

        {effect_added_selector, Selector@4} ->
            Base_selector = gleam_erlang_ffi:merge_selector(
                erlang:element(4, State),
                Selector@4
            ),
            Selector@5 = gleam_erlang_ffi:merge_selector(
                erlang:element(3, State),
                Selector@4
            ),
            {continue,
                begin
                    _record@5 = State,
                    {state,
                        erlang:element(2, _record@5),
                        Selector@5,
                        Base_selector,
                        erlang:element(5, _record@5),
                        erlang:element(6, _record@5),
                        erlang:element(7, _record@5),
                        erlang:element(8, _record@5),
                        erlang:element(9, _record@5),
                        erlang:element(10, _record@5),
                        erlang:element(11, _record@5),
                        erlang:element(12, _record@5)}
                end,
                {some, Selector@5}};

        {effect_dispatched_message, Message@2} ->
            {Model, Effect} = (erlang:element(6, State))(
                erlang:element(5, State),
                Message@2
            ),
            Vdom = (erlang:element(7, State))(Model),
            {diff, Patch@1, Events@1} = lustre@vdom@diff:diff(
                erlang:element(10, State),
                erlang:element(9, State),
                Vdom
            ),
            handle_effect(erlang:element(2, State), Effect),
            broadcast(
                erlang:element(11, State),
                erlang:element(12, State),
                lustre@runtime@transport:reconcile(Patch@1)
            ),
            gleam@otp@actor:continue(
                begin
                    _record@6 = State,
                    {state,
                        erlang:element(2, _record@6),
                        erlang:element(3, _record@6),
                        erlang:element(4, _record@6),
                        Model,
                        erlang:element(6, _record@6),
                        erlang:element(7, _record@6),
                        erlang:element(8, _record@6),
                        Vdom,
                        Events@1,
                        erlang:element(11, _record@6),
                        erlang:element(12, _record@6)}
                end
            );

        {effect_emit_event, Name, Data} ->
            broadcast(
                erlang:element(11, State),
                erlang:element(12, State),
                lustre@runtime@transport:emit(Name, Data)
            ),
            gleam@otp@actor:continue(State);

        {self_dispatched_messages, [], Effect@1} ->
            Vdom@1 = (erlang:element(7, State))(erlang:element(5, State)),
            {diff, Patch@2, Events@2} = lustre@vdom@diff:diff(
                erlang:element(10, State),
                erlang:element(9, State),
                Vdom@1
            ),
            handle_effect(erlang:element(2, State), Effect@1),
            broadcast(
                erlang:element(11, State),
                erlang:element(12, State),
                lustre@runtime@transport:reconcile(Patch@2)
            ),
            gleam@otp@actor:continue(
                begin
                    _record@7 = State,
                    {state,
                        erlang:element(2, _record@7),
                        erlang:element(3, _record@7),
                        erlang:element(4, _record@7),
                        erlang:element(5, _record@7),
                        erlang:element(6, _record@7),
                        erlang:element(7, _record@7),
                        erlang:element(8, _record@7),
                        Vdom@1,
                        Events@2,
                        erlang:element(11, _record@7),
                        erlang:element(12, _record@7)}
                end
            );

        {self_dispatched_messages, [Message@3 | Messages], Effect@2} ->
            {Model@1, More_effects} = (erlang:element(6, State))(
                erlang:element(5, State),
                Message@3
            ),
            Effect@3 = lustre@effect:batch([Effect@2, More_effects]),
            State@1 = begin
                _record@8 = State,
                {state,
                    erlang:element(2, _record@8),
                    erlang:element(3, _record@8),
                    erlang:element(4, _record@8),
                    Model@1,
                    erlang:element(6, _record@8),
                    erlang:element(7, _record@8),
                    erlang:element(8, _record@8),
                    erlang:element(9, _record@8),
                    erlang:element(10, _record@8),
                    erlang:element(11, _record@8),
                    erlang:element(12, _record@8)}
            end,
            loop({self_dispatched_messages, Messages, Effect@3}, State@1);

        system_requested_shutdown ->
            gleam@dict:each(
                erlang:element(11, State),
                fun(_, Monitor@4) ->
                    gleam@erlang@process:demonitor_process(Monitor@4)
                end
            ),
            {stop, normal}
    end.

-file("src/lustre/runtime/server/runtime.gleam", 55).
?DOC(false).
-spec start(
    {TKY, lustre@effect:effect(TKZ)},
    fun((TKY, TKZ) -> {TKY, lustre@effect:effect(TKZ)}),
    fun((TKY) -> lustre@vdom@vnode:element(TKZ)),
    config(TKZ)
) -> {ok, gleam@erlang@process:subject(message(TKZ))} |
    {error, gleam@otp@actor:start_error()}.
start(Init, Update, View, Config) ->
    gleam@otp@actor:start_spec(
        begin
            {spec,
                fun() ->
                    Self = gleam@erlang@process:new_subject(),
                    Base_selector = begin
                        _pipe = gleam_erlang_ffi:new_selector(),
                        gleam@erlang@process:selecting(
                            _pipe,
                            Self,
                            fun gleam@function:identity/1
                        )
                    end,
                    Vdom = View(erlang:element(1, Init)),
                    Events = lustre@vdom@events:from_node(Vdom),
                    State = {state,
                        Self,
                        Base_selector,
                        Base_selector,
                        erlang:element(1, Init),
                        Update,
                        View,
                        Config,
                        Vdom,
                        Events,
                        maps:new(),
                        gleam@set:new()},
                    handle_effect(Self, erlang:element(2, Init)),
                    {ready, State, Base_selector}
                end,
                1000,
                fun loop/2}
        end
    ).
