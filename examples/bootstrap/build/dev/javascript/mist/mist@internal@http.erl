-module(mist@internal@http).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_header/1, read_data/4, version_to_string/1, add_date_header/1, connection_close/1, keep_alive/1, maybe_keep_alive/1, add_content_length/2, add_default_headers/2, handle_continue/1, parse_headers/4, crypto_hash/2, base64_encode/1, parse_chunk/1, parse_request/2, read_body/1, upgrade_socket/2, upgrade/4]).
-export_type([response_data/0, connection/0, packet_type/0, http_uri/0, http_packet/0, decoded_packet/0, decode_error/0, chunk/0, http_version/0, parsed_request/0, body/0, sha_hash/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type response_data() :: {websocket,
        gleam@erlang@process:selector(gleam@erlang@process:process_down())} |
    {bytes, gleam@bytes_tree:bytes_tree()} |
    {chunked, gleam@yielder:yielder(gleam@bytes_tree:bytes_tree())} |
    {file, mist@internal@file:file_descriptor(), integer(), integer()} |
    {server_sent_events,
        gleam@erlang@process:selector(gleam@erlang@process:process_down())}.

-type connection() :: {connection,
        body(),
        glisten@socket:socket(),
        glisten@transport:transport()}.

-type packet_type() :: http | httph_bin | http_bin.

-type http_uri() :: {abs_path, bitstring()}.

-type http_packet() :: {http_request,
        gleam@dynamic:dynamic_(),
        http_uri(),
        {integer(), integer()}} |
    {http_header,
        integer(),
        gleam@erlang@atom:atom_(),
        bitstring(),
        bitstring()}.

-type decoded_packet() :: {binary_data, http_packet(), bitstring()} |
    {end_of_headers, bitstring()} |
    {more_data, gleam@option:option(integer())} |
    {http2_upgrade, bitstring()}.

-type decode_error() :: malformed_request |
    invalid_method |
    invalid_path |
    unknown_header |
    unknown_method |
    invalid_body |
    discard_packet |
    no_host_header |
    invalid_http_version.

-type chunk() :: {chunk, bitstring(), mist@internal@buffer:buffer()} | complete.

-type http_version() :: http1 | http11.

-type parsed_request() :: {http1_request,
        gleam@http@request:request(connection()),
        http_version()} |
    {upgrade, bitstring()}.

-type body() :: {initial, bitstring()} |
    {stream,
        gleam@erlang@process:selector(bitstring()),
        bitstring(),
        integer(),
        integer()}.

-type sha_hash() :: sha.

-file("src/mist/internal/http.gleam", 77).
?DOC(false).
-spec from_header(bitstring()) -> binary().
from_header(Value) ->
    _assert_subject = gleam@bit_array:to_string(Value),
    {ok, Value@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/http"/utf8>>,
                        function => <<"from_header"/utf8>>,
                        line => 78})
    end,
    string:lowercase(Value@1).

-file("src/mist/internal/http.gleam", 112).
?DOC(false).
-spec read_data(
    glisten@socket:socket(),
    glisten@transport:transport(),
    mist@internal@buffer:buffer(),
    decode_error()
) -> {ok, bitstring()} | {error, decode_error()}.
read_data(Socket, Transport, Buffer, Error) ->
    To_read = gleam@int:min(erlang:element(2, Buffer), 1000000),
    Timeout = 15000,
    gleam@result:then(
        begin
            _pipe = Socket,
            _pipe@1 = glisten@transport:receive_timeout(
                Transport,
                _pipe,
                To_read,
                Timeout
            ),
            gleam@result:replace_error(_pipe@1, Error)
        end,
        fun(Data) ->
            Next_buffer = {buffer,
                gleam@int:max(0, erlang:element(2, Buffer) - To_read),
                <<(erlang:element(3, Buffer))/bitstring, Data/bitstring>>},
            case erlang:element(2, Next_buffer) > 0 of
                true ->
                    read_data(Socket, Transport, Next_buffer, Error);

                false ->
                    {ok, erlang:element(3, Next_buffer)}
            end
        end
    ).

-file("src/mist/internal/http.gleam", 239).
?DOC(false).
-spec version_to_string(http_version()) -> binary().
version_to_string(Version) ->
    case Version of
        http1 ->
            <<"1.0"/utf8>>;

        http11 ->
            <<"1.1"/utf8>>
    end.

-file("src/mist/internal/http.gleam", 515).
?DOC(false).
-spec add_date_header(gleam@http@response:response(MVY)) -> gleam@http@response:response(MVY).
add_date_header(Resp) ->
    case gleam@http@response:get_header(Resp, <<"date"/utf8>>) of
        {error, _} ->
            gleam@http@response:set_header(
                Resp,
                <<"date"/utf8>>,
                mist@internal@clock:get_date()
            );

        _ ->
            Resp
    end.

-file("src/mist/internal/http.gleam", 522).
?DOC(false).
-spec connection_close(gleam@http@response:response(MWB)) -> gleam@http@response:response(MWB).
connection_close(Resp) ->
    gleam@http@response:set_header(
        Resp,
        <<"connection"/utf8>>,
        <<"close"/utf8>>
    ).

-file("src/mist/internal/http.gleam", 526).
?DOC(false).
-spec keep_alive(gleam@http@response:response(MWE)) -> gleam@http@response:response(MWE).
keep_alive(Resp) ->
    gleam@http@response:set_header(
        Resp,
        <<"connection"/utf8>>,
        <<"keep-alive"/utf8>>
    ).

-file("src/mist/internal/http.gleam", 530).
?DOC(false).
-spec maybe_keep_alive(gleam@http@response:response(MWH)) -> gleam@http@response:response(MWH).
maybe_keep_alive(Resp) ->
    case gleam@http@response:get_header(Resp, <<"connection"/utf8>>) of
        {ok, _} ->
            Resp;

        _ ->
            gleam@http@response:set_header(
                Resp,
                <<"connection"/utf8>>,
                <<"keep-alive"/utf8>>
            )
    end.

-file("src/mist/internal/http.gleam", 537).
?DOC(false).
-spec maybe_drop_body(
    gleam@http@response:response(gleam@bytes_tree:bytes_tree()),
    boolean()
) -> gleam@http@response:response(gleam@bytes_tree:bytes_tree()).
maybe_drop_body(Resp, Is_head_request) ->
    case Is_head_request of
        true ->
            gleam@http@response:set_body(Resp, gleam@bytes_tree:new());

        false ->
            Resp
    end.

-file("src/mist/internal/http.gleam", 547).
?DOC(false).
-spec add_content_length(boolean(), integer()) -> fun((gleam@http@response:response(MWM)) -> gleam@http@response:response(MWM)).
add_content_length(When, Length) ->
    fun(Resp) -> case When of
            true ->
                {_, Headers} = begin
                    _pipe = erlang:element(3, Resp),
                    _pipe@1 = gleam@list:key_pop(
                        _pipe,
                        <<"content-length"/utf8>>
                    ),
                    gleam@result:lazy_unwrap(
                        _pipe@1,
                        fun() -> {<<""/utf8>>, erlang:element(3, Resp)} end
                    )
                end,
                _pipe@2 = begin
                    _record = Resp,
                    {response,
                        erlang:element(2, _record),
                        Headers,
                        erlang:element(4, _record)}
                end,
                gleam@http@response:set_header(
                    _pipe@2,
                    <<"content-length"/utf8>>,
                    erlang:integer_to_binary(Length)
                );

            false ->
                Resp
        end end.

-file("src/mist/internal/http.gleam", 567).
?DOC(false).
-spec add_default_headers(
    gleam@http@response:response(gleam@bytes_tree:bytes_tree()),
    boolean()
) -> gleam@http@response:response(gleam@bytes_tree:bytes_tree()).
add_default_headers(Resp, Is_head_response) ->
    Body_size = erlang:iolist_size(erlang:element(4, Resp)),
    {_, Headers} = begin
        _pipe = erlang:element(3, Resp),
        _pipe@1 = gleam@list:key_pop(_pipe, <<"content-length"/utf8>>),
        gleam@result:lazy_unwrap(
            _pipe@1,
            fun() -> {<<""/utf8>>, erlang:element(3, Resp)} end
        )
    end,
    Resp@1 = case {erlang:element(2, Resp), Body_size} of
        {N, _} when (N >= 100) andalso (N =< 199) ->
            _record = Resp,
            {response,
                erlang:element(2, _record),
                Headers,
                erlang:element(4, _record)};

        {N@1, _} when N@1 =:= 204 ->
            _record@1 = Resp,
            {response,
                erlang:element(2, _record@1),
                Headers,
                erlang:element(4, _record@1)};

        {N@2, 0} when N@2 =:= 304 ->
            Resp;

        {_, 0} when Is_head_response =:= true ->
            Resp;

        {_, _} ->
            gleam@http@response:set_header(
                Resp,
                <<"content-length"/utf8>>,
                erlang:integer_to_binary(Body_size)
            )
    end,
    _pipe@2 = Resp@1,
    _pipe@3 = add_date_header(_pipe@2),
    maybe_drop_body(_pipe@3, Is_head_response).

-file("src/mist/internal/http.gleam", 596).
?DOC(false).
-spec is_continue(gleam@http@request:request(connection())) -> boolean().
is_continue(Req) ->
    _pipe = erlang:element(3, Req),
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(Tup) ->
            (gleam@pair:first(Tup) =:= <<"expect"/utf8>>) andalso (gleam@pair:second(
                Tup
            )
            =:= <<"100-continue"/utf8>>)
        end
    ),
    gleam@result:is_ok(_pipe@1).

-file("src/mist/internal/http.gleam", 604).
?DOC(false).
-spec handle_continue(gleam@http@request:request(connection())) -> {ok, nil} |
    {error, decode_error()}.
handle_continue(Req) ->
    case is_continue(Req) of
        true ->
            _pipe = gleam@http@response:new(100),
            _pipe@1 = gleam@http@response:set_body(
                _pipe,
                gleam@bytes_tree:new()
            ),
            _pipe@2 = mist@internal@encoder:to_bytes_tree(
                _pipe@1,
                <<"1.1"/utf8>>
            ),
            _pipe@3 = glisten@transport:send(
                erlang:element(4, erlang:element(4, Req)),
                erlang:element(3, erlang:element(4, Req)),
                _pipe@2
            ),
            gleam@result:replace_error(_pipe@3, malformed_request);

        false ->
            {ok, nil}
    end.

-file("src/mist/internal/http.gleam", 83).
?DOC(false).
-spec parse_headers(
    bitstring(),
    glisten@socket:socket(),
    glisten@transport:transport(),
    gleam@dict:dict(binary(), binary())
) -> {ok, {gleam@dict:dict(binary(), binary()), bitstring()}} |
    {error, decode_error()}.
parse_headers(Bs, Socket, Transport, Headers) ->
    case mist_ffi:decode_packet(httph_bin, Bs, []) of
        {ok, {binary_data, {http_header, _, _, Field, Value}, Rest}} ->
            Field@1 = from_header(Field),
            _assert_subject = gleam@bit_array:to_string(Value),
            {ok, Value@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"parse_headers"/utf8>>,
                                line => 92})
            end,
            _pipe = Headers,
            _pipe@1 = gleam@dict:insert(_pipe, Field@1, Value@1),
            parse_headers(Rest, Socket, Transport, _pipe@1);

        {ok, {end_of_headers, Rest@1}} ->
            {ok, {Headers, Rest@1}};

        {ok, {more_data, Size}} ->
            Amount_to_read = gleam@option:unwrap(Size, 0),
            gleam@result:then(
                read_data(
                    Socket,
                    Transport,
                    {buffer, Amount_to_read, Bs},
                    unknown_header
                ),
                fun(Next) -> parse_headers(Next, Socket, Transport, Headers) end
            );

        _ ->
            {error, unknown_header}
    end.

-file("src/mist/internal/http.gleam", 625).
?DOC(false).
-spec crypto_hash(sha_hash(), binary()) -> binary().
crypto_hash(Hash, Data) ->
    crypto:hash(Hash, Data).

-file("src/mist/internal/http.gleam", 628).
?DOC(false).
-spec base64_encode(binary()) -> binary().
base64_encode(Data) ->
    base64:encode(Data).

-file("src/mist/internal/http.gleam", 145).
?DOC(false).
-spec parse_chunk(bitstring()) -> chunk().
parse_chunk(String) ->
    case binary:split(String, <<"\r\n"/utf8>>) of
        [<<"0"/utf8>>, _] ->
            complete;

        [Chunk_size, Rest] ->
            _assert_subject = gleam@bit_array:to_string(Chunk_size),
            {ok, Chunk_size@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"parse_chunk"/utf8>>,
                                line => 149})
            end,
            case gleam@int:base_parse(Chunk_size@1, 16) of
                {ok, Size} ->
                    Size@1 = Size * 8,
                    case Rest of
                        <<Next_chunk:Size@1/bitstring,
                            13/integer,
                            10/integer,
                            Rest@1/bitstring>> ->
                            {chunk,
                                Next_chunk,
                                mist@internal@buffer:new(Rest@1)};

                        _ ->
                            {chunk, <<>>, mist@internal@buffer:new(String)}
                    end;

                {error, _} ->
                    {chunk, <<>>, mist@internal@buffer:new(String)}
            end;

        _ ->
            {chunk, <<>>, mist@internal@buffer:new(String)}
    end.

-file("src/mist/internal/http.gleam", 252).
?DOC(false).
-spec parse_request(bitstring(), connection()) -> {ok, parsed_request()} |
    {error, decode_error()}.
parse_request(Bs, Conn) ->
    case mist_ffi:decode_packet(http_bin, Bs, []) of
        {ok,
            {binary_data,
                {http_request, Http_method, {abs_path, Path}, Version},
                Rest}} ->
            gleam@result:then(
                begin
                    _pipe = Http_method,
                    _pipe@1 = gleam_erlang_ffi:atom_from_dynamic(_pipe),
                    _pipe@2 = gleam@result:map(
                        _pipe@1,
                        fun erlang:atom_to_binary/1
                    ),
                    _pipe@3 = gleam@result:replace_error(_pipe@2, nil),
                    _pipe@6 = gleam@result:'or'(
                        _pipe@3,
                        begin
                            _pipe@4 = Http_method,
                            _pipe@5 = gleam@dynamic@decode:run(
                                _pipe@4,
                                {decoder,
                                    fun gleam@dynamic@decode:decode_string/1}
                            ),
                            gleam@result:replace_error(_pipe@5, nil)
                        end
                    ),
                    _pipe@7 = gleam@result:then(
                        _pipe@6,
                        fun gleam@http:parse_method/1
                    ),
                    gleam@result:replace_error(_pipe@7, unknown_method)
                end,
                fun(Method) ->
                    gleam@result:then(
                        parse_headers(
                            Rest,
                            erlang:element(3, Conn),
                            erlang:element(4, Conn),
                            maps:new()
                        ),
                        fun(_use0) ->
                            {Headers, Rest@1} = _use0,
                            gleam@result:then(
                                begin
                                    _pipe@8 = Path,
                                    _pipe@9 = gleam@bit_array:to_string(_pipe@8),
                                    gleam@result:replace_error(
                                        _pipe@9,
                                        invalid_path
                                    )
                                end,
                                fun(Path@1) ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@10 = mist_ffi:get_path_and_query(
                                                Path@1
                                            ),
                                            gleam@result:replace_error(
                                                _pipe@10,
                                                invalid_path
                                            )
                                        end,
                                        fun(_use0@1) ->
                                            {Path@2, Query} = _use0@1,
                                            Scheme = case erlang:element(
                                                4,
                                                Conn
                                            ) of
                                                ssl ->
                                                    https;

                                                tcp ->
                                                    http
                                            end,
                                            gleam@result:then(
                                                begin
                                                    _pipe@11 = gleam_stdlib:map_get(
                                                        Headers,
                                                        <<"host"/utf8>>
                                                    ),
                                                    gleam@result:replace_error(
                                                        _pipe@11,
                                                        no_host_header
                                                    )
                                                end,
                                                fun(Host_header) ->
                                                    {Hostname, Port} = begin
                                                        _pipe@12 = Host_header,
                                                        _pipe@13 = gleam@string:split_once(
                                                            _pipe@12,
                                                            <<":"/utf8>>
                                                        ),
                                                        gleam@result:unwrap(
                                                            _pipe@13,
                                                            {Host_header,
                                                                <<""/utf8>>}
                                                        )
                                                    end,
                                                    Port@1 = begin
                                                        _pipe@14 = gleam_stdlib:parse_int(
                                                            Port
                                                        ),
                                                        _pipe@15 = gleam@result:map_error(
                                                            _pipe@14,
                                                            fun(_) ->
                                                                case Scheme of
                                                                    https ->
                                                                        443;

                                                                    http ->
                                                                        80
                                                                end
                                                            end
                                                        ),
                                                        gleam@result:unwrap_both(
                                                            _pipe@15
                                                        )
                                                    end,
                                                    Req = {request,
                                                        Method,
                                                        maps:to_list(Headers),
                                                        begin
                                                            _record = Conn,
                                                            {connection,
                                                                {initial,
                                                                    Rest@1},
                                                                erlang:element(
                                                                    3,
                                                                    _record
                                                                ),
                                                                erlang:element(
                                                                    4,
                                                                    _record
                                                                )}
                                                        end,
                                                        Scheme,
                                                        Hostname,
                                                        {some, Port@1},
                                                        Path@2,
                                                        gleam@option:from_result(
                                                            Query
                                                        )},
                                                    case Version of
                                                        {1, 0} ->
                                                            {ok,
                                                                {http1_request,
                                                                    Req,
                                                                    http1}};

                                                        {1, 1} ->
                                                            {ok,
                                                                {http1_request,
                                                                    Req,
                                                                    http11}};

                                                        _ ->
                                                            {error,
                                                                invalid_http_version}
                                                    end
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok,
            {http2_upgrade,
                <<13/integer,
                    10/integer,
                    83/integer,
                    77/integer,
                    13/integer,
                    10/integer,
                    13/integer,
                    10/integer,
                    Data/bitstring>>}} ->
            {ok, {upgrade, Data}};

        {ok, {more_data, Size}} ->
            Amount_to_read = gleam@option:unwrap(Size, 0),
            gleam@result:then(
                read_data(
                    erlang:element(3, Conn),
                    erlang:element(4, Conn),
                    {buffer, Amount_to_read, Bs},
                    malformed_request
                ),
                fun(Next) -> parse_request(Next, Conn) end
            );

        _ ->
            {error, discard_packet}
    end.

-file("src/mist/internal/http.gleam", 175).
?DOC(false).
-spec read_chunk(
    glisten@socket:socket(),
    glisten@transport:transport(),
    mist@internal@buffer:buffer(),
    gleam@bytes_tree:bytes_tree()
) -> {ok, gleam@bytes_tree:bytes_tree()} | {error, decode_error()}.
read_chunk(Socket, Transport, Buffer, Body) ->
    case {erlang:element(3, Buffer),
        mist_ffi:binary_match(
            erlang:element(3, Buffer),
            <<13/integer, 10/integer>>
        )} of
        {_, {ok, {Offset, _}}} ->
            _assert_subject = erlang:element(3, Buffer),
            <<Chunk:Offset/binary, _/integer, _/integer, Rest/binary>> = case _assert_subject of
                <<_:Offset/binary, _/integer, _/integer, _/binary>> -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"read_chunk"/utf8>>,
                                line => 183})
            end,
            gleam@result:then(
                begin
                    _pipe = Chunk,
                    _pipe@1 = gleam@bit_array:to_string(_pipe),
                    _pipe@2 = gleam@result:map(
                        _pipe@1,
                        fun unicode:characters_to_list/1
                    ),
                    gleam@result:replace_error(_pipe@2, invalid_body)
                end,
                fun(Chunk_size) ->
                    gleam@result:then(
                        begin
                            _pipe@3 = mist_ffi:string_to_int(Chunk_size, 16),
                            gleam@result:replace_error(_pipe@3, invalid_body)
                        end,
                        fun(Size) -> case Size of
                                0 ->
                                    {ok, Body};

                                Size@1 ->
                                    case Rest of
                                        <<Next_chunk:Size@1/binary,
                                            13/integer,
                                            10/integer,
                                            Rest@1/binary>> ->
                                            read_chunk(
                                                Socket,
                                                Transport,
                                                {buffer, 0, Rest@1},
                                                gleam@bytes_tree:append(
                                                    Body,
                                                    Next_chunk
                                                )
                                            );

                                        _ ->
                                            gleam@result:then(
                                                read_data(
                                                    Socket,
                                                    Transport,
                                                    {buffer,
                                                        0,
                                                        erlang:element(
                                                            3,
                                                            Buffer
                                                        )},
                                                    invalid_body
                                                ),
                                                fun(Next) ->
                                                    read_chunk(
                                                        Socket,
                                                        Transport,
                                                        {buffer, 0, Next},
                                                        Body
                                                    )
                                                end
                                            )
                                    end
                            end end
                    )
                end
            );

        {<<>> = Data, _} ->
            gleam@result:then(
                read_data(Socket, Transport, {buffer, 0, Data}, invalid_body),
                fun(Next@1) ->
                    read_chunk(Socket, Transport, {buffer, 0, Next@1}, Body)
                end
            );

        {Data, {error, nil}} ->
            gleam@result:then(
                read_data(Socket, Transport, {buffer, 0, Data}, invalid_body),
                fun(Next@1) ->
                    read_chunk(Socket, Transport, {buffer, 0, Next@1}, Body)
                end
            )
    end.

-file("src/mist/internal/http.gleam", 362).
?DOC(false).
-spec read_body(gleam@http@request:request(connection())) -> {ok,
        gleam@http@request:request(bitstring())} |
    {error, decode_error()}.
read_body(Req) ->
    Transport = case erlang:element(5, Req) of
        https ->
            ssl;

        http ->
            tcp
    end,
    case {gleam@http@request:get_header(Req, <<"transfer-encoding"/utf8>>),
        erlang:element(2, erlang:element(4, Req))} of
        {{ok, <<"chunked"/utf8>>}, {initial, Rest}} ->
            gleam@result:then(
                handle_continue(Req),
                fun(_) ->
                    gleam@result:then(
                        read_chunk(
                            erlang:element(3, erlang:element(4, Req)),
                            Transport,
                            {buffer, 0, Rest},
                            gleam@bytes_tree:new()
                        ),
                        fun(Chunk) ->
                            {ok,
                                gleam@http@request:set_body(
                                    Req,
                                    erlang:list_to_bitstring(Chunk)
                                )}
                        end
                    )
                end
            );

        {_, {initial, Rest@1}} ->
            gleam@result:then(
                handle_continue(Req),
                fun(_) ->
                    Body_size = begin
                        _pipe = erlang:element(3, Req),
                        _pipe@1 = gleam@list:find(
                            _pipe,
                            fun(Tup) ->
                                gleam@pair:first(Tup) =:= <<"content-length"/utf8>>
                            end
                        ),
                        _pipe@2 = gleam@result:map(
                            _pipe@1,
                            fun gleam@pair:second/1
                        ),
                        _pipe@3 = gleam@result:then(
                            _pipe@2,
                            fun gleam_stdlib:parse_int/1
                        ),
                        gleam@result:unwrap(_pipe@3, 0)
                    end,
                    Remaining = Body_size - erlang:byte_size(Rest@1),
                    _pipe@4 = case {Body_size, Remaining} of
                        {0, 0} ->
                            {ok, <<>>};

                        {0, _} ->
                            {ok, Rest@1};

                        {_, 0} ->
                            {ok, Rest@1};

                        {_, _} ->
                            read_data(
                                erlang:element(3, erlang:element(4, Req)),
                                Transport,
                                {buffer, Remaining, Rest@1},
                                invalid_body
                            )
                    end,
                    _pipe@5 = gleam@result:map(
                        _pipe@4,
                        fun(_capture) ->
                            gleam@http@request:set_body(Req, _capture)
                        end
                    ),
                    gleam@result:replace_error(_pipe@5, invalid_body)
                end
            );

        {_, {stream, Selector, Data, Remaining@1, Attempts}} when Remaining@1 > 0 ->
            Res = begin
                _pipe@6 = Selector,
                _pipe@7 = gleam_erlang_ffi:select(_pipe@6, 1000),
                gleam@result:replace_error(_pipe@7, invalid_body)
            end,
            gleam@result:then(
                Res,
                fun(Next) ->
                    Got = erlang:byte_size(Next),
                    Left = gleam@int:max(Remaining@1 - Got, 0),
                    New_data = gleam@bit_array:append(Data, Next),
                    case Left of
                        0 ->
                            {ok, gleam@http@request:set_body(Req, New_data)};

                        _ ->
                            read_body(
                                gleam@http@request:set_body(
                                    Req,
                                    begin
                                        _record = erlang:element(4, Req),
                                        {connection,
                                            {stream,
                                                Selector,
                                                New_data,
                                                Left,
                                                Attempts + 1},
                                            erlang:element(3, _record),
                                            erlang:element(4, _record)}
                                    end
                                )
                            )
                    end
                end
            );

        {_, {stream, _, Data@1, _, _}} ->
            {ok, gleam@http@request:set_body(Req, Data@1)}
    end.

-file("src/mist/internal/http.gleam", 445).
?DOC(false).
-spec parse_websocket_key(binary()) -> binary().
parse_websocket_key(Key) ->
    _pipe = Key,
    _pipe@1 = gleam@string:append(
        _pipe,
        <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"/utf8>>
    ),
    _pipe@2 = crypto:hash(sha, _pipe@1),
    base64:encode(_pipe@2).

-file("src/mist/internal/http.gleam", 452).
?DOC(false).
-spec upgrade_socket(gleam@http@request:request(connection()), list(binary())) -> {ok,
        gleam@http@response:response(gleam@bytes_tree:bytes_tree())} |
    {error, gleam@http@request:request(connection())}.
upgrade_socket(Req, Extensions) ->
    gleam@result:then(
        begin
            _pipe = gleam@http@request:get_header(Req, <<"upgrade"/utf8>>),
            gleam@result:replace_error(_pipe, Req)
        end,
        fun(_) ->
            gleam@result:then(
                begin
                    _pipe@1 = gleam@http@request:get_header(
                        Req,
                        <<"sec-websocket-key"/utf8>>
                    ),
                    gleam@result:replace_error(_pipe@1, Req)
                end,
                fun(Key) ->
                    gleam@result:then(
                        begin
                            _pipe@2 = gleam@http@request:get_header(
                                Req,
                                <<"sec-websocket-version"/utf8>>
                            ),
                            gleam@result:replace_error(_pipe@2, Req)
                        end,
                        fun(_) ->
                            Permessage_deflate = gramps@websocket:has_deflate(
                                Extensions
                            ),
                            Accept_key = parse_websocket_key(Key),
                            Resp = begin
                                _pipe@3 = gleam@http@response:new(101),
                                _pipe@4 = gleam@http@response:set_body(
                                    _pipe@3,
                                    gleam@bytes_tree:new()
                                ),
                                _pipe@5 = gleam@http@response:prepend_header(
                                    _pipe@4,
                                    <<"upgrade"/utf8>>,
                                    <<"websocket"/utf8>>
                                ),
                                _pipe@6 = gleam@http@response:prepend_header(
                                    _pipe@5,
                                    <<"connection"/utf8>>,
                                    <<"Upgrade"/utf8>>
                                ),
                                gleam@http@response:prepend_header(
                                    _pipe@6,
                                    <<"sec-websocket-accept"/utf8>>,
                                    Accept_key
                                )
                            end,
                            case Permessage_deflate of
                                true ->
                                    {ok,
                                        gleam@http@response:prepend_header(
                                            Resp,
                                            <<"sec-websocket-extensions"/utf8>>,
                                            <<"permessage-deflate"/utf8>>
                                        )};

                                false ->
                                    {ok, Resp}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/mist/internal/http.gleam", 492).
?DOC(false).
-spec upgrade(
    glisten@socket:socket(),
    glisten@transport:transport(),
    list(binary()),
    gleam@http@request:request(connection())
) -> {ok, nil} | {error, nil}.
upgrade(Socket, Transport, Extensions, Req) ->
    gleam@result:then(
        begin
            _pipe = upgrade_socket(Req, Extensions),
            gleam@result:replace_error(_pipe, nil)
        end,
        fun(Resp) ->
            gleam@result:then(
                begin
                    _pipe@1 = Resp,
                    _pipe@2 = add_default_headers(
                        _pipe@1,
                        erlang:element(2, Req) =:= head
                    ),
                    _pipe@3 = maybe_keep_alive(_pipe@2),
                    _pipe@4 = mist@internal@encoder:to_bytes_tree(
                        _pipe@3,
                        <<"1.1"/utf8>>
                    ),
                    _pipe@5 = glisten@transport:send(Transport, Socket, _pipe@4),
                    gleam@result:replace_error(_pipe@5, nil)
                end,
                fun(_) -> {ok, nil} end
            )
        end
    ).
