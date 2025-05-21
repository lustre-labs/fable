-module(mist@internal@http2@stream).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([make_request/2, new/5, receive_data/2]).
-export_type([message/0, stream_state/0, state/0, internal_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type message() :: ready | {data, bitstring(), boolean()} | done.

-type stream_state() :: open | remote_closed | local_closed | closed.

-type state() :: {state,
        mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
        stream_state(),
        gleam@erlang@process:subject(message()),
        integer(),
        integer(),
        gleam@option:option(integer())}.

-type internal_state() :: {internal_state,
        gleam@erlang@process:selector(message()),
        gleam@erlang@process:subject(message()),
        boolean(),
        gleam@option:option(gleam@http@response:response(mist@internal@http:response_data())),
        bitstring()}.

-file("src/mist/internal/http2/stream.gleam", 146).
?DOC(false).
-spec make_request(
    list({binary(), binary()}),
    gleam@http@request:request(mist@internal@http:connection())
) -> {ok, gleam@http@request:request(mist@internal@http:connection())} |
    {error, nil}.
make_request(Headers, Req) ->
    case Headers of
        [] ->
            {ok, Req};

        [{<<"method"/utf8>>, Method} | Rest] ->
            _pipe = Method,
            _pipe@1 = gleam@http:parse_method(_pipe),
            _pipe@2 = gleam@result:replace_error(_pipe@1, nil),
            _pipe@3 = gleam@result:map(
                _pipe@2,
                fun(_capture) ->
                    gleam@http@request:set_method(Req, _capture)
                end
            ),
            gleam@result:then(
                _pipe@3,
                fun(_capture@1) -> make_request(Rest, _capture@1) end
            );

        [{<<"scheme"/utf8>>, Scheme} | Rest@1] ->
            _pipe@4 = Scheme,
            _pipe@5 = gleam@http:scheme_from_string(_pipe@4),
            _pipe@6 = gleam@result:replace_error(_pipe@5, nil),
            _pipe@7 = gleam@result:map(
                _pipe@6,
                fun(_capture@2) ->
                    gleam@http@request:set_scheme(Req, _capture@2)
                end
            ),
            gleam@result:then(
                _pipe@7,
                fun(_capture@3) -> make_request(Rest@1, _capture@3) end
            );

        [{<<"authority"/utf8>>, _} | Rest@2] ->
            make_request(Rest@2, Req);

        [{<<"path"/utf8>>, Path} | Rest@3] ->
            _pipe@8 = Path,
            _pipe@9 = gleam@string:split_once(_pipe@8, <<"?"/utf8>>),
            _pipe@13 = gleam@result:map(
                _pipe@9,
                fun(Split) ->
                    gleam@pair:map_second(Split, fun(Query) -> _pipe@10 = Query,
                            _pipe@11 = gleam_stdlib:parse_query(_pipe@10),
                            _pipe@12 = gleam@result:map(
                                _pipe@11,
                                fun(Field@0) -> {some, Field@0} end
                            ),
                            gleam@result:unwrap(_pipe@12, none) end)
                end
            ),
            _pipe@14 = gleam@result:unwrap(_pipe@13, {Path, none}),
            (fun(Tup) -> _pipe@17 = case erlang:element(2, Tup) of
                    {some, Query@1} ->
                        _pipe@15 = Req,
                        _pipe@16 = gleam@http@request:set_path(
                            _pipe@15,
                            erlang:element(1, Tup)
                        ),
                        gleam@http@request:set_query(_pipe@16, Query@1);

                    _ ->
                        gleam@http@request:set_path(Req, erlang:element(1, Tup))
                end,
                make_request(Rest@3, _pipe@17) end)(_pipe@14);

        [{Key, Value} | Rest@4] ->
            _pipe@18 = Req,
            _pipe@19 = gleam@http@request:set_header(_pipe@18, Key, Value),
            make_request(Rest@4, _pipe@19)
    end.

-file("src/mist/internal/http2/stream.gleam", 55).
?DOC(false).
-spec new(
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data())),
    list({binary(), binary()}),
    mist@internal@http:connection(),
    fun((gleam@http@response:response(mist@internal@http:response_data())) -> any()),
    boolean()
) -> {ok, gleam@erlang@process:subject(message())} |
    {error, gleam@otp@actor:start_error()}.
new(Handler, Headers, Connection, Send, End) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Data_subj = gleam@erlang@process:new_subject(),
                Data_selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:selecting(
                        _pipe,
                        Data_subj,
                        fun gleam@function:identity/1
                    )
                end,
                {ready,
                    {internal_state, Data_selector, Data_subj, End, none, <<>>},
                    Data_selector}
            end,
            1000,
            fun(Msg, State) -> case {Msg, erlang:element(4, State)} of
                    {ready, _} ->
                        Content_length = begin
                            _pipe@1 = Headers,
                            _pipe@2 = gleam@list:key_find(
                                _pipe@1,
                                <<"content-length"/utf8>>
                            ),
                            _pipe@3 = gleam@result:then(
                                _pipe@2,
                                fun gleam_stdlib:parse_int/1
                            ),
                            gleam@result:unwrap(_pipe@3, 0)
                        end,
                        Conn = begin
                            _record = Connection,
                            {connection,
                                {stream,
                                    gleam_erlang_ffi:map_selector(
                                        erlang:element(2, State),
                                        fun(Val) ->
                                            {data, Bits, _} = case Val of
                                                {data, _, _} -> Val;
                                                _assert_fail ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                value => _assert_fail,
                                                                module => <<"mist/internal/http2/stream"/utf8>>,
                                                                function => <<"new"/utf8>>,
                                                                line => 88}
                                                        )
                                            end,
                                            Bits
                                        end
                                    ),
                                    <<>>,
                                    Content_length,
                                    0},
                                erlang:element(3, _record),
                                erlang:element(4, _record)}
                        end,
                        _pipe@4 = gleam@http@request:new(),
                        _pipe@5 = gleam@http@request:set_body(_pipe@4, Conn),
                        _pipe@6 = make_request(Headers, _pipe@5),
                        _pipe@7 = gleam@result:map(_pipe@6, Handler),
                        _pipe@8 = gleam@result:map(
                            _pipe@7,
                            fun(Resp) ->
                                gleam@erlang@process:send(
                                    erlang:element(3, State),
                                    done
                                ),
                                gleam@otp@actor:continue(
                                    begin
                                        _record@1 = State,
                                        {internal_state,
                                            erlang:element(2, _record@1),
                                            erlang:element(3, _record@1),
                                            erlang:element(4, _record@1),
                                            {some, Resp},
                                            erlang:element(6, _record@1)}
                                    end
                                )
                            end
                        ),
                        _pipe@9 = gleam@result:map_error(
                            _pipe@8,
                            fun(Err) ->
                                {stop,
                                    {abnormal,
                                        <<"Failed to respond to request: "/utf8,
                                            (gleam@erlang:format(Err))/binary>>}}
                            end
                        ),
                        gleam@result:unwrap_both(_pipe@9);

                    {done, true} ->
                        _assert_subject = erlang:element(5, State),
                        {some, Resp@1} = case _assert_subject of
                            {some, _} -> _assert_subject;
                            _assert_fail@1 ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                            value => _assert_fail@1,
                                            module => <<"mist/internal/http2/stream"/utf8>>,
                                            function => <<"new"/utf8>>,
                                            line => 115})
                        end,
                        Send(Resp@1),
                        gleam@otp@actor:continue(State);

                    {{data, Bits@1, true}, _} ->
                        gleam@erlang@process:send(
                            erlang:element(3, State),
                            done
                        ),
                        gleam@otp@actor:continue(
                            begin
                                _record@2 = State,
                                {internal_state,
                                    erlang:element(2, _record@2),
                                    erlang:element(3, _record@2),
                                    true,
                                    erlang:element(5, _record@2),
                                    <<(erlang:element(6, State))/bitstring,
                                        Bits@1/bitstring>>}
                            end
                        );

                    {{data, Bits@2, _}, _} ->
                        gleam@otp@actor:continue(
                            begin
                                _record@3 = State,
                                {internal_state,
                                    erlang:element(2, _record@3),
                                    erlang:element(3, _record@3),
                                    erlang:element(4, _record@3),
                                    erlang:element(5, _record@3),
                                    <<(erlang:element(6, State))/bitstring,
                                        Bits@2/bitstring>>}
                            end
                        );

                    {_, _} ->
                        gleam@otp@actor:continue(State)
                end end}
    ).

-file("src/mist/internal/http2/stream.gleam", 198).
?DOC(false).
-spec receive_data(state(), integer()) -> {state(), integer()}.
receive_data(State, Size) ->
    {New_window_size, Increment} = mist@internal@http2@flow_control:compute_receive_window(
        erlang:element(5, State),
        Size
    ),
    New_state = begin
        _record = State,
        {state,
            erlang:element(2, _record),
            erlang:element(3, _record),
            erlang:element(4, _record),
            New_window_size,
            erlang:element(6, _record),
            gleam@option:map(
                erlang:element(7, State),
                fun(Val) -> Val - Size end
            )}
    end,
    {New_state, Increment}.
