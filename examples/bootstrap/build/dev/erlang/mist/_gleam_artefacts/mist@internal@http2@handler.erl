-module(mist@internal@http2@handler).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([send_hpack_context/2, receive_hpack_context/2, append_data/2, upgrade/3, call/3]).
-export_type([message/0, pending_send/0, state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type message() :: {send,
        mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
        gleam@http@response:response(mist@internal@http:response_data())}.

-type pending_send() :: pending_send.

-type state() :: {state,
        gleam@option:option(mist@internal@http2@frame:frame()),
        mist@internal@buffer:buffer(),
        list(pending_send()),
        mist@internal@http2:hpack_context(),
        gleam@erlang@process:subject(message()),
        mist@internal@http2:hpack_context(),
        integer(),
        integer(),
        mist@internal@http2:http2_settings(),
        gleam@dict:dict(mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()), mist@internal@http2@stream:state())}.

-file("src/mist/internal/http2/handler.gleam", 44).
?DOC(false).
-spec send_hpack_context(state(), mist@internal@http2:hpack_context()) -> state().
send_hpack_context(State, Context) ->
    _record = State,
    {state,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        Context,
        erlang:element(8, _record),
        erlang:element(9, _record),
        erlang:element(10, _record),
        erlang:element(11, _record)}.

-file("src/mist/internal/http2/handler.gleam", 48).
?DOC(false).
-spec receive_hpack_context(state(), mist@internal@http2:hpack_context()) -> state().
receive_hpack_context(State, Context) ->
    _record = State,
    {state,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Context,
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record),
        erlang:element(10, _record),
        erlang:element(11, _record)}.

-file("src/mist/internal/http2/handler.gleam", 52).
?DOC(false).
-spec append_data(state(), bitstring()) -> state().
append_data(State, Data) ->
    _record = State,
    {state,
        erlang:element(2, _record),
        mist@internal@buffer:append(erlang:element(3, State), Data),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record),
        erlang:element(10, _record),
        erlang:element(11, _record)}.

-file("src/mist/internal/http2/handler.gleam", 56).
?DOC(false).
-spec upgrade(
    bitstring(),
    mist@internal@http:connection(),
    gleam@erlang@process:subject(message())
) -> {ok, state()} | {error, gleam@erlang@process:exit_reason()}.
upgrade(_, Conn, _) ->
    _ = mist@internal@http2:default_settings(),
    Settings_frame = {settings, false, []},
    _assert_subject = mist@internal@http2:send_frame(
        Settings_frame,
        erlang:element(3, Conn),
        erlang:element(4, Conn)
    ),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/http2/handler"/utf8>>,
                        function => <<"upgrade"/utf8>>,
                        line => 64})
    end,
    logging:log(error, <<"HTTP/2 currently not supported"/utf8>>),
    {error, {abnormal, <<"HTTP/2 currently not supported"/utf8>>}}.

-file("src/mist/internal/http2/handler.gleam", 118).
?DOC(false).
-spec handle_frame(
    mist@internal@http2@frame:frame(),
    state(),
    mist@internal@http:connection(),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data()))
) -> {ok, state()} | {error, gleam@erlang@process:exit_reason()}.
handle_frame(Frame, State, Conn, Handler) ->
    case {erlang:element(2, State), Frame} of
        {{some, {header, {continued, Existing}, End_stream, Id1, Priority}},
            {continuation, {complete, Data}, Id2}} when Id1 =:= Id2 ->
            Complete_frame = {header,
                {complete, <<Existing/bitstring, Data/bitstring>>},
                End_stream,
                Id1,
                Priority},
            handle_frame(
                Complete_frame,
                begin
                    _record = State,
                    {state,
                        none,
                        erlang:element(3, _record),
                        erlang:element(4, _record),
                        erlang:element(5, _record),
                        erlang:element(6, _record),
                        erlang:element(7, _record),
                        erlang:element(8, _record),
                        erlang:element(9, _record),
                        erlang:element(10, _record),
                        erlang:element(11, _record)}
                end,
                Conn,
                Handler
            );

        {{some,
                {header,
                    {continued, Existing@1},
                    End_stream@1,
                    Id1@1,
                    Priority@1}},
            {continuation, {continued, Data@1}, Id2@1}} when Id1@1 =:= Id2@1 ->
            Next = {header,
                {continued, <<Existing@1/bitstring, Data@1/bitstring>>},
                End_stream@1,
                Id1@1,
                Priority@1},
            {ok,
                begin
                    _record@1 = State,
                    {state,
                        {some, Next},
                        erlang:element(3, _record@1),
                        erlang:element(4, _record@1),
                        erlang:element(5, _record@1),
                        erlang:element(6, _record@1),
                        erlang:element(7, _record@1),
                        erlang:element(8, _record@1),
                        erlang:element(9, _record@1),
                        erlang:element(10, _record@1),
                        erlang:element(11, _record@1)}
                end};

        {none, {window_update, Amount, Identifier}} ->
            case mist@internal@http2@frame:get_stream_identifier(Identifier) of
                0 ->
                    {ok,
                        begin
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
                                begin
                                    _record@3 = erlang:element(10, State),
                                    {http2_settings,
                                        erlang:element(2, _record@3),
                                        erlang:element(3, _record@3),
                                        erlang:element(4, _record@3),
                                        Amount,
                                        erlang:element(6, _record@3),
                                        erlang:element(7, _record@3)}
                                end,
                                erlang:element(11, _record@2)}
                        end};

                _ ->
                    _pipe = erlang:element(11, State),
                    _pipe@1 = gleam_stdlib:map_get(_pipe, Identifier),
                    _pipe@2 = gleam@result:replace_error(
                        _pipe@1,
                        {abnormal,
                            <<"Window update for non-existent stream"/utf8>>}
                    ),
                    gleam@result:then(
                        _pipe@2,
                        fun(Stream) ->
                            case mist@internal@http2@flow_control:update_send_window(
                                erlang:element(6, Stream),
                                Amount
                            ) of
                                {ok, Update} ->
                                    New_stream = begin
                                        _record@4 = Stream,
                                        {state,
                                            erlang:element(2, _record@4),
                                            erlang:element(3, _record@4),
                                            erlang:element(4, _record@4),
                                            erlang:element(5, _record@4),
                                            Update,
                                            erlang:element(7, _record@4)}
                                    end,
                                    {ok,
                                        begin
                                            _record@5 = State,
                                            {state,
                                                erlang:element(2, _record@5),
                                                erlang:element(3, _record@5),
                                                erlang:element(4, _record@5),
                                                erlang:element(5, _record@5),
                                                erlang:element(6, _record@5),
                                                erlang:element(7, _record@5),
                                                erlang:element(8, _record@5),
                                                erlang:element(9, _record@5),
                                                erlang:element(10, _record@5),
                                                gleam@dict:insert(
                                                    erlang:element(11, State),
                                                    Identifier,
                                                    New_stream
                                                )}
                                        end};

                                _ ->
                                    {error,
                                        {abnormal,
                                            <<"Failed to update send window"/utf8>>}}
                            end
                        end
                    )
            end;

        {none, {header, {complete, Data@2}, End_stream@2, Identifier@1, _}} ->
            Conn@1 = {connection,
                {initial, <<>>},
                erlang:element(3, Conn),
                erlang:element(4, Conn)},
            _assert_subject = mist_ffi:hpack_decode(
                erlang:element(5, State),
                Data@2
            ),
            {ok, {Headers, Context}} = case _assert_subject of
                {ok, {_, _}} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/http2/handler"/utf8>>,
                                function => <<"handle_frame"/utf8>>,
                                line => 213})
            end,
            Pending_content_length = begin
                _pipe@3 = Headers,
                _pipe@4 = gleam@list:key_find(
                    _pipe@3,
                    <<"content-length"/utf8>>
                ),
                _pipe@5 = gleam@result:then(
                    _pipe@4,
                    fun gleam_stdlib:parse_int/1
                ),
                gleam@option:from_result(_pipe@5)
            end,
            _assert_subject@1 = mist@internal@http2@stream:new(
                Handler,
                Headers,
                Conn@1,
                fun(Resp) ->
                    gleam@erlang@process:send(
                        erlang:element(6, State),
                        {send, Identifier@1, Resp}
                    )
                end,
                End_stream@2
            ),
            {ok, New_stream@1} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"mist/internal/http2/handler"/utf8>>,
                                function => <<"handle_frame"/utf8>>,
                                line => 222})
            end,
            gleam@erlang@process:send(New_stream@1, ready),
            Stream_state = {state,
                Identifier@1,
                open,
                New_stream@1,
                erlang:element(5, erlang:element(10, State)),
                erlang:element(5, erlang:element(10, State)),
                Pending_content_length},
            Streams = gleam@dict:insert(
                erlang:element(11, State),
                Identifier@1,
                Stream_state
            ),
            {ok,
                begin
                    _record@6 = State,
                    {state,
                        erlang:element(2, _record@6),
                        erlang:element(3, _record@6),
                        erlang:element(4, _record@6),
                        Context,
                        erlang:element(6, _record@6),
                        erlang:element(7, _record@6),
                        erlang:element(8, _record@6),
                        erlang:element(9, _record@6),
                        erlang:element(10, _record@6),
                        Streams}
                end};

        {none, {data, Data@3, End_stream@3, Identifier@2}} ->
            Data_size = erlang:byte_size(Data@3),
            {Conn_receive_window_size, Conn_window_increment} = mist@internal@http2@flow_control:compute_receive_window(
                erlang:element(9, State),
                Data_size
            ),
            _pipe@6 = erlang:element(11, State),
            _pipe@7 = gleam_stdlib:map_get(_pipe@6, Identifier@2),
            _pipe@8 = gleam@result:map(
                _pipe@7,
                fun(_capture) ->
                    mist@internal@http2@stream:receive_data(_capture, Data_size)
                end
            ),
            _pipe@9 = gleam@result:replace_error(
                _pipe@8,
                {abnormal, <<"Stream failed to receive data"/utf8>>}
            ),
            gleam@result:map(
                _pipe@9,
                fun(Update@1) ->
                    {New_stream@2, Increment} = Update@1,
                    _ = case Conn_window_increment > 0 of
                        true ->
                            mist@internal@http2:send_frame(
                                {window_update,
                                    Conn_window_increment,
                                    mist@internal@http2@frame:stream_identifier(
                                        0
                                    )},
                                erlang:element(3, Conn),
                                erlang:element(4, Conn)
                            );

                        false ->
                            {ok, nil}
                    end,
                    _ = case Increment > 0 of
                        true ->
                            mist@internal@http2:send_frame(
                                {window_update, Increment, Identifier@2},
                                erlang:element(3, Conn),
                                erlang:element(4, Conn)
                            );

                        false ->
                            {ok, nil}
                    end,
                    gleam@erlang@process:send(
                        erlang:element(4, New_stream@2),
                        {data, Data@3, End_stream@3}
                    ),
                    _record@7 = State,
                    {state,
                        erlang:element(2, _record@7),
                        erlang:element(3, _record@7),
                        erlang:element(4, _record@7),
                        erlang:element(5, _record@7),
                        erlang:element(6, _record@7),
                        erlang:element(7, _record@7),
                        erlang:element(8, _record@7),
                        Conn_receive_window_size,
                        erlang:element(10, _record@7),
                        gleam@dict:insert(
                            erlang:element(11, State),
                            Identifier@2,
                            New_stream@2
                        )}
                end
            );

        {none, {priority, _, _, _, _}} ->
            {ok, State};

        {none, {settings, true, _}} ->
            {ok, State};

        {_, {settings, _, _}} ->
            _pipe@10 = mist@internal@http2:send_frame(
                mist@internal@http2@frame:settings_ack(),
                erlang:element(3, Conn),
                erlang:element(4, Conn)
            ),
            _pipe@11 = gleam@result:replace(_pipe@10, State),
            gleam@result:replace_error(
                _pipe@11,
                {abnormal, <<"Failed to respond to settings ACK"/utf8>>}
            );

        {none, {go_away, _, _, _}} ->
            logging:log(debug, <<"byteeee~~"/utf8>>),
            {error, normal};

        {_, Frame@1} ->
            logging:log(
                debug,
                <<"Ignoring frame: "/utf8,
                    (gleam@erlang:format(Frame@1))/binary>>
            ),
            {ok, State}
    end.

-file("src/mist/internal/http2/handler.gleam", 91).
?DOC(false).
-spec call(
    state(),
    mist@internal@http:connection(),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data()))
) -> {ok, state()} | {error, gleam@erlang@process:exit_reason()}.
call(State, Conn, Handler) ->
    case mist@internal@http2@frame:decode(
        erlang:element(3, erlang:element(3, State))
    ) of
        {ok, {Frame, Rest}} ->
            New_state = begin
                _record = State,
                {state,
                    erlang:element(2, _record),
                    mist@internal@buffer:new(Rest),
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record),
                    erlang:element(9, _record),
                    erlang:element(10, _record),
                    erlang:element(11, _record)}
            end,
            case handle_frame(Frame, New_state, Conn, Handler) of
                {ok, Updated} ->
                    call(Updated, Conn, Handler);

                {error, Reason} ->
                    {error, Reason}
            end;

        {error, no_error} ->
            {ok, State};

        {error, _} ->
            {ok, State}
    end.
