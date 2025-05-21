-module(mist@internal@http2).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([default_settings/0, update_settings/2, send_frame/3, hpack_new_context/1, hpack_max_table_size/2, hpack_decode/2, hpack_encode/2, send_bytes_tree/4]).
-export_type([http2_settings/0, hpack_context/0, hpack_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type http2_settings() :: {http2_settings,
        integer(),
        mist@internal@http2@frame:push_state(),
        integer(),
        integer(),
        integer(),
        gleam@option:option(integer())}.

-type hpack_context() :: any().

-type hpack_error() :: compression | {bad_header_packet, bitstring()}.

-file("src/mist/internal/http2.gleam", 30).
?DOC(false).
-spec default_settings() -> http2_settings().
default_settings() ->
    {http2_settings, 4096, disabled, 100, 65535, 16384, none}.

-file("src/mist/internal/http2.gleam", 41).
?DOC(false).
-spec update_settings(
    http2_settings(),
    list(mist@internal@http2@frame:setting())
) -> http2_settings().
update_settings(Current, Settings) ->
    gleam@list:fold(
        Settings,
        Current,
        fun(Settings@1, Setting) -> case Setting of
                {header_table_size, Size} ->
                    _record = Settings@1,
                    {http2_settings,
                        Size,
                        erlang:element(3, _record),
                        erlang:element(4, _record),
                        erlang:element(5, _record),
                        erlang:element(6, _record),
                        erlang:element(7, _record)};

                {server_push, Push} ->
                    _record@1 = Settings@1,
                    {http2_settings,
                        erlang:element(2, _record@1),
                        Push,
                        erlang:element(4, _record@1),
                        erlang:element(5, _record@1),
                        erlang:element(6, _record@1),
                        erlang:element(7, _record@1)};

                {max_concurrent_streams, Max} ->
                    _record@2 = Settings@1,
                    {http2_settings,
                        erlang:element(2, _record@2),
                        erlang:element(3, _record@2),
                        Max,
                        erlang:element(5, _record@2),
                        erlang:element(6, _record@2),
                        erlang:element(7, _record@2)};

                {initial_window_size, Size@1} ->
                    _record@3 = Settings@1,
                    {http2_settings,
                        erlang:element(2, _record@3),
                        erlang:element(3, _record@3),
                        erlang:element(4, _record@3),
                        Size@1,
                        erlang:element(6, _record@3),
                        erlang:element(7, _record@3)};

                {max_frame_size, Size@2} ->
                    _record@4 = Settings@1,
                    {http2_settings,
                        erlang:element(2, _record@4),
                        erlang:element(3, _record@4),
                        erlang:element(4, _record@4),
                        erlang:element(5, _record@4),
                        Size@2,
                        erlang:element(7, _record@4)};

                {max_header_list_size, Size@3} ->
                    _record@5 = Settings@1,
                    {http2_settings,
                        erlang:element(2, _record@5),
                        erlang:element(3, _record@5),
                        erlang:element(4, _record@5),
                        erlang:element(5, _record@5),
                        erlang:element(6, _record@5),
                        {some, Size@3}}
            end end
    ).

-file("src/mist/internal/http2.gleam", 93).
?DOC(false).
-spec send_data(
    mist@internal@http:connection(),
    bitstring(),
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    boolean()
) -> {ok, nil} | {error, gleam@erlang@process:exit_reason()}.
send_data(Conn, Data, Stream_identifier, End_stream) ->
    Data_frame = {data, Data, End_stream, Stream_identifier},
    Encoded = mist@internal@http2@frame:encode(Data_frame),
    _pipe = glisten@transport:send(
        erlang:element(4, Conn),
        erlang:element(3, Conn),
        gleam@bytes_tree:from_bit_array(Encoded)
    ),
    gleam@result:map_error(
        _pipe,
        fun(Err) ->
            logging:log(
                debug,
                <<"failed to send :(  "/utf8,
                    (gleam@erlang:format(Err))/binary>>
            ),
            {abnormal, <<"Failed to send HTTP/2 data"/utf8>>}
        end
    ).

-file("src/mist/internal/http2.gleam", 115).
?DOC(false).
-spec send_frame(
    mist@internal@http2@frame:frame(),
    glisten@socket:socket(),
    glisten@transport:transport()
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
send_frame(Frame_to_send, Socket, Transport) ->
    Data = mist@internal@http2@frame:encode(Frame_to_send),
    glisten@transport:send(
        Transport,
        Socket,
        gleam@bytes_tree:from_bit_array(Data)
    ).

-file("src/mist/internal/http2.gleam", 154).
?DOC(false).
-spec hpack_new_context(integer()) -> hpack_context().
hpack_new_context(Size) ->
    hpack:new_context(Size).

-file("src/mist/internal/http2.gleam", 157).
?DOC(false).
-spec hpack_max_table_size(hpack_context(), integer()) -> hpack_context().
hpack_max_table_size(Context, Size) ->
    mist_ffi:hpack_new_max_table_size(Context, Size).

-file("src/mist/internal/http2.gleam", 165).
?DOC(false).
-spec hpack_decode(hpack_context(), bitstring()) -> {ok,
        {list({binary(), binary()}), hpack_context()}} |
    {error, hpack_error()}.
hpack_decode(Context, Bin) ->
    mist_ffi:hpack_decode(Context, Bin).

-file("src/mist/internal/http2.gleam", 171).
?DOC(false).
-spec hpack_encode(hpack_context(), list({binary(), binary()})) -> {ok,
        {bitstring(), hpack_context()}} |
    {error, any()}.
hpack_encode(Context, Headers) ->
    mist_ffi:hpack_encode(Context, Headers).

-file("src/mist/internal/http2.gleam", 62).
?DOC(false).
-spec send_headers(
    hpack_context(),
    mist@internal@http:connection(),
    list({binary(), binary()}),
    boolean(),
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
) -> {ok, hpack_context()} | {error, gleam@erlang@process:exit_reason()}.
send_headers(Context, Conn, Headers, End_stream, Stream_identifier) ->
    _pipe = mist_ffi:hpack_encode(Context, Headers),
    gleam@result:then(
        _pipe,
        fun(Pair) ->
            {Headers@1, New_context} = Pair,
            Header_frame = {header,
                {complete, Headers@1},
                End_stream,
                Stream_identifier,
                none},
            Encoded = mist@internal@http2@frame:encode(Header_frame),
            case glisten@transport:send(
                erlang:element(4, Conn),
                erlang:element(3, Conn),
                gleam@bytes_tree:from_bit_array(Encoded)
            ) of
                {ok, _} ->
                    {ok, New_context};

                {error, _} ->
                    {error,
                        {abnormal, <<"Failed to send HTTP/2 headers"/utf8>>}}
            end
        end
    ).

-file("src/mist/internal/http2.gleam", 125).
?DOC(false).
-spec send_bytes_tree(
    gleam@http@response:response(gleam@bytes_tree:bytes_tree()),
    mist@internal@http:connection(),
    hpack_context(),
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
) -> {ok, hpack_context()} | {error, gleam@erlang@process:exit_reason()}.
send_bytes_tree(Resp, Conn, Context, Id) ->
    Resp@1 = begin
        _pipe = Resp,
        mist@internal@http:add_default_headers(_pipe, false)
    end,
    Headers = [{<<":status"/utf8>>,
            erlang:integer_to_binary(erlang:element(2, Resp@1))} |
        erlang:element(3, Resp@1)],
    case erlang:iolist_size(erlang:element(4, Resp@1)) of
        0 ->
            send_headers(Context, Conn, Headers, true, Id);

        _ ->
            _pipe@1 = send_headers(Context, Conn, Headers, false, Id),
            gleam@result:then(
                _pipe@1,
                fun(Context@1) ->
                    _pipe@2 = send_data(
                        Conn,
                        erlang:list_to_bitstring(erlang:element(4, Resp@1)),
                        Id,
                        true
                    ),
                    gleam@result:replace(_pipe@2, Context@1)
                end
            )
    end.
