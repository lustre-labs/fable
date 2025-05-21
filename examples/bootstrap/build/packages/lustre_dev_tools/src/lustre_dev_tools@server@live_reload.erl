-module(lustre_dev_tools@server@live_reload).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([inject/1, start/3]).
-export_type([watcher_msg/0, socket_msg/0, live_reloading_error/0, event/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type watcher_msg() :: {add, gleam@erlang@process:subject(socket_msg())} |
    {remove, gleam@erlang@process:subject(socket_msg())} |
    broadcast |
    {unknown, gleam@dynamic:dynamic_()}.

-type socket_msg() :: reload | {show_error, lustre_dev_tools@error:error()}.

-type live_reloading_error() :: no_file_watcher_supported_for_os |
    {no_file_watcher_installed, gleam@dynamic:dynamic_()}.

-type event() :: created | modified | deleted.

-file("src/lustre_dev_tools/server/live_reload.gleam", 71).
?DOC(false).
-spec inject(binary()) -> binary().
inject(Html) ->
    _assert_subject = gleam_erlang_ffi:priv_directory(
        <<"lustre_dev_tools"/utf8>>
    ),
    {ok, Priv} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/server/live_reload"/utf8>>,
                        function => <<"inject"/utf8>>,
                        line => 72})
    end,
    _assert_subject@1 = simplifile:read(
        <<Priv/binary, "/server/live-reload.js"/utf8>>
    ),
    {ok, Source} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre_dev_tools/server/live_reload"/utf8>>,
                        function => <<"inject"/utf8>>,
                        line => 73})
    end,
    Script = <<<<"<script>"/utf8, Source/binary>>/binary, "</script>"/utf8>>,
    _pipe = Html,
    gleam@string:replace(
        _pipe,
        <<"</head>"/utf8>>,
        <<Script/binary, "</head>"/utf8>>
    ).

-file("src/lustre_dev_tools/server/live_reload.gleam", 82).
?DOC(false).
-spec init_socket(
    gleam@erlang@process:subject(watcher_msg()),
    mist@internal@websocket:websocket_connection()
) -> {{gleam@erlang@process:subject(socket_msg()),
        gleam@erlang@process:subject(watcher_msg())},
    gleam@option:option(gleam@erlang@process:selector(socket_msg()))}.
init_socket(Watcher, _) ->
    Self = gleam@erlang@process:new_subject(),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(_pipe, Self, fun(Msg) -> Msg end)
    end,
    State = {Self, Watcher},
    gleam@erlang@process:send(Watcher, {add, Self}),
    {State, {some, Selector}}.

-file("src/lustre_dev_tools/server/live_reload.gleam", 97).
?DOC(false).
-spec loop_socket(
    {gleam@erlang@process:subject(socket_msg()),
        gleam@erlang@process:subject(watcher_msg())},
    mist@internal@websocket:websocket_connection(),
    mist:websocket_message(socket_msg())
) -> gleam@otp@actor:next(socket_msg(), {gleam@erlang@process:subject(socket_msg()),
    gleam@erlang@process:subject(watcher_msg())}).
loop_socket(State, Connection, Msg) ->
    case Msg of
        {text, _} ->
            gleam@otp@actor:continue(State);

        {binary, _} ->
            gleam@otp@actor:continue(State);

        {custom, reload} ->
            _assert_subject = mist:send_text_frame(
                Connection,
                begin
                    _pipe = gleam@json:object(
                        [{<<"$"/utf8>>, gleam@json:string(<<"reload"/utf8>>)}]
                    ),
                    gleam@json:to_string(_pipe)
                end
            ),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/server/live_reload"/utf8>>,
                                function => <<"loop_socket"/utf8>>,
                                line => 106})
            end,
            gleam@otp@actor:continue(State);

        {custom, {show_error, Error}} ->
            _assert_subject@1 = mist:send_text_frame(
                Connection,
                begin
                    _pipe@1 = gleam@json:object(
                        [{<<"$"/utf8>>, gleam@json:string(<<"error"/utf8>>)},
                            {<<"error"/utf8>>,
                                gleam@json:string(
                                    lustre_dev_tools@error:explain(Error)
                                )}]
                    ),
                    gleam@json:to_string(_pipe@1)
                end
            ),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"lustre_dev_tools/server/live_reload"/utf8>>,
                                function => <<"loop_socket"/utf8>>,
                                line => 115})
            end,
            gleam@otp@actor:continue(State);

        closed ->
            gleam@erlang@process:send(
                erlang:element(2, State),
                {remove, erlang:element(1, State)}
            ),
            {stop, normal};

        shutdown ->
            gleam@erlang@process:send(
                erlang:element(2, State),
                {remove, erlang:element(1, State)}
            ),
            {stop, normal}
    end.

-file("src/lustre_dev_tools/server/live_reload.gleam", 134).
?DOC(false).
-spec close_socket(
    {gleam@erlang@process:subject(socket_msg()),
        gleam@erlang@process:subject(watcher_msg())}
) -> nil.
close_socket(State) ->
    gleam@erlang@process:send(
        erlang:element(2, State),
        {remove, erlang:element(1, State)}
    ).

-file("src/lustre_dev_tools/server/live_reload.gleam", 198).
?DOC(false).
-spec loop_watcher(
    watcher_msg(),
    gleam@set:set(gleam@erlang@process:subject(socket_msg())),
    binary(),
    glint:flags()
) -> gleam@otp@actor:next(watcher_msg(), gleam@set:set(gleam@erlang@process:subject(socket_msg()))).
loop_watcher(Msg, State, Entry, Flags) ->
    case Msg of
        {add, Client} ->
            _pipe = Client,
            _pipe@1 = gleam@set:insert(State, _pipe),
            gleam@otp@actor:continue(_pipe@1);

        {remove, Client@1} ->
            _pipe@2 = Client@1,
            _pipe@3 = gleam@set:delete(State, _pipe@2),
            gleam@otp@actor:continue(_pipe@3);

        broadcast ->
            Script = begin
                lustre_dev_tools@cli:do(
                    lustre_dev_tools@cli:mute(),
                    fun(_) ->
                        lustre_dev_tools@cli:do(
                            lustre_dev_tools@cli:get_bool(
                                <<"detect_tailwind"/utf8>>,
                                true,
                                [<<"build"/utf8>>],
                                fun(_capture) ->
                                    glint:get_flag(
                                        _capture,
                                        lustre_dev_tools@cli@flag:detect_tailwind(
                                            
                                        )
                                    )
                                end
                            ),
                            fun(Detect_tailwind) ->
                                lustre_dev_tools@cli:do(
                                    lustre_dev_tools@cli@build:do_app(
                                        Entry,
                                        false,
                                        Detect_tailwind
                                    ),
                                    fun(_) ->
                                        lustre_dev_tools@cli:do(
                                            lustre_dev_tools@cli:unmute(),
                                            fun(_) ->
                                                lustre_dev_tools@cli:return(nil)
                                            end
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end,
            case lustre_dev_tools@cli:run(Script, Flags) of
                {ok, _} ->
                    gleam@set:fold(
                        State,
                        nil,
                        fun(_, Client@2) ->
                            gleam@erlang@process:send(Client@2, reload)
                        end
                    );

                {error, Error} ->
                    _pipe@4 = <<"\x{001b}c"/utf8>>,
                    gleam_stdlib:print_error(_pipe@4),
                    _pipe@5 = lustre_dev_tools@error:explain(Error),
                    gleam_stdlib:println_error(_pipe@5),
                    gleam@set:fold(
                        State,
                        nil,
                        fun(_, Client@3) ->
                            gleam@erlang@process:send(
                                Client@3,
                                {show_error, Error}
                            )
                        end
                    )
            end,
            gleam@otp@actor:continue(State);

        {unknown, _} ->
            gleam@otp@actor:continue(State)
    end.

-file("src/lustre_dev_tools/server/live_reload.gleam", 268).
?DOC(false).
-spec is_interesting_event(gleam@dynamic:dynamic_()) -> boolean().
is_interesting_event(Event) ->
    ((Event =:= gleam_stdlib:identity(created)) orelse (Event =:= gleam_stdlib:identity(
        modified
    )))
    orelse (Event =:= gleam_stdlib:identity(deleted)).

-file("src/lustre_dev_tools/server/live_reload.gleam", 252).
?DOC(false).
-spec change_decoder() -> gleam@dynamic@decode:decoder(watcher_msg()).
change_decoder() ->
    Events_decoder = gleam@dynamic@decode:at(
        [1],
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
        )
    ),
    gleam@dynamic@decode:field(
        2,
        Events_decoder,
        fun(Events) ->
            case gleam@list:any(Events, fun is_interesting_event/1) of
                true ->
                    gleam@dynamic@decode:success(broadcast);

                false ->
                    gleam@dynamic@decode:failure(broadcast, <<""/utf8>>)
            end
        end
    ).

-file("src/lustre_dev_tools/server/live_reload.gleam", 153).
?DOC(false).
-spec init_watcher(binary()) -> gleam@otp@actor:init_result(gleam@set:set(gleam@erlang@process:subject(socket_msg())), watcher_msg()).
init_watcher(Root) ->
    Src = filepath:join(Root, <<"src"/utf8>>),
    Id = erlang:binary_to_atom(Src),
    case lustre_dev_tools_ffi:check_live_reloading() of
        {ok, _} ->
            nil;

        {error, no_file_watcher_supported_for_os} ->
            _pipe = <<"⚠️ There's no live reloading support for your os!"/utf8>>,
            _pipe@1 = gleam_community@ansi:yellow(_pipe),
            gleam_stdlib:println(_pipe@1);

        {error, {no_file_watcher_installed, Watcher}} ->
            _pipe@2 = (<<<<"⚠️ You need to install "/utf8,
                    (gleam@string:inspect(Watcher))/binary>>/binary,
                " for live reloading to work!"/utf8>>),
            _pipe@3 = gleam_community@ansi:yellow(_pipe@2),
            gleam_stdlib:println(_pipe@3)
    end,
    case lustre_dev_tools_ffi:fs_start_link(Id, Src) of
        {ok, _} ->
            Self = gleam@erlang@process:new_subject(),
            Selector = begin
                _pipe@4 = gleam_erlang_ffi:new_selector(),
                _pipe@5 = gleam@erlang@process:selecting(
                    _pipe@4,
                    Self,
                    fun(Msg) -> Msg end
                ),
                gleam@erlang@process:selecting_anything(
                    _pipe@5,
                    fun(Msg@1) ->
                        case gleam@dynamic@decode:run(Msg@1, change_decoder()) of
                            {ok, Broadcast} ->
                                Broadcast;

                            {error, _} ->
                                {unknown, Msg@1}
                        end
                    end
                )
            end,
            State = gleam@set:new(),
            fs:subscribe(Id),
            {ready, State, Selector};

        {error, Err} ->
            {failed,
                <<"Failed to start watcher: "/utf8,
                    (gleam@string:inspect(Err))/binary>>}
    end.

-file("src/lustre_dev_tools/server/live_reload.gleam", 140).
?DOC(false).
-spec start_watcher(binary(), binary(), glint:flags()) -> {ok,
        gleam@erlang@process:subject(watcher_msg())} |
    {error, lustre_dev_tools@error:error()}.
start_watcher(Entry, Root, Flags) ->
    _pipe = gleam@otp@actor:start_spec(
        {spec,
            fun() -> init_watcher(Root) end,
            1000,
            fun(Msg, State) -> loop_watcher(Msg, State, Entry, Flags) end}
    ),
    gleam@result:map_error(
        _pipe,
        fun(Field@0) -> {cannot_start_file_watcher, Field@0} end
    ).

-file("src/lustre_dev_tools/server/live_reload.gleam", 55).
?DOC(false).
-spec start(binary(), binary(), glint:flags()) -> {ok,
        fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data()))} |
    {error, lustre_dev_tools@error:error()}.
start(Entry, Root, Flags) ->
    gleam@result:'try'(
        start_watcher(Entry, Root, Flags),
        fun(Watcher) ->
            Make_socket = fun(_capture) ->
                mist:websocket(
                    _capture,
                    fun loop_socket/3,
                    fun(_capture@1) -> init_socket(Watcher, _capture@1) end,
                    fun close_socket/1
                )
            end,
            {ok, Make_socket}
        end
    ).
