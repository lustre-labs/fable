-module(glisten).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_supervisor/1, convert_ip_address/1, get_server_info/2, ip_address_to_string/1, get_client_info/1, send/2, handler/2, with_close/2, with_pool_size/2, with_http2/1, with_ipv6/1, start_server/2, serve/2, start_ssl_server/4, serve_ssl/4, bind/2]).
-export_type([start_error/0, message/1, ip_address/0, server/0, connection_info/0, connection/1, handler/2]).

-type start_error() :: listener_closed |
    listener_timeout |
    acceptor_timeout |
    {acceptor_failed, gleam@erlang@process:exit_reason()} |
    {acceptor_crashed, gleam@dynamic:dynamic_()} |
    {system_error, glisten@socket:socket_reason()}.

-type message(IPX) :: {packet, bitstring()} | {user, IPX}.

-type ip_address() :: {ip_v4, integer(), integer(), integer(), integer()} |
    {ip_v6,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-opaque server() :: {server,
        gleam@erlang@process:subject(glisten@internal@listener:message()),
        gleam@erlang@process:subject(gleam@otp@supervisor:message()),
        glisten@transport:transport()}.

-type connection_info() :: {connection_info, integer(), ip_address()}.

-type connection(IPY) :: {connection,
        glisten@socket:socket(),
        glisten@transport:transport(),
        gleam@erlang@process:subject(glisten@internal@handler:message(IPY))}.

-opaque handler(IPZ, IQA) :: {handler,
        glisten@socket@options:interface(),
        fun((connection(IPZ)) -> {IQA,
            gleam@option:option(gleam@erlang@process:selector(IPZ))}),
        fun((message(IPZ), IQA, connection(IPZ)) -> gleam@otp@actor:next(message(IPZ), IQA)),
        gleam@option:option(fun((IQA) -> nil)),
        integer(),
        boolean(),
        boolean()}.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 80).
-spec get_supervisor(server()) -> gleam@erlang@process:subject(gleam@otp@supervisor:message()).
get_supervisor(Server) ->
    erlang:element(3, Server).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 95).
-spec convert_ip_address(glisten@socket@options:ip_address()) -> ip_address().
convert_ip_address(Ip) ->
    case Ip of
        {ip_v4, A, B, C, D} ->
            {ip_v4, A, B, C, D};

        {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H} ->
            {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H}
    end.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 69).
-spec get_server_info(server(), integer()) -> {ok, connection_info()} |
    {error, gleam@erlang@process:call_error(glisten@internal@listener:state())}.
get_server_info(Server, Timeout) ->
    _pipe = gleam@erlang@process:try_call(
        erlang:element(2, Server),
        fun(Field@0) -> {info, Field@0} end,
        Timeout
    ),
    gleam@result:map(
        _pipe,
        fun(State) ->
            {connection_info,
                erlang:element(3, State),
                convert_ip_address(erlang:element(4, State))}
        end
    ).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 124).
-spec join_ipv6_fields(list(integer())) -> binary().
join_ipv6_fields(Fields) ->
    _pipe = gleam@list:map(Fields, fun gleam@int:to_base16/1),
    gleam@string:join(_pipe, <<":"/utf8>>).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 133).
-spec ipv6_zeros(list(integer()), integer(), integer(), integer(), integer()) -> {ok,
        {integer(), integer()}} |
    {error, nil}.
ipv6_zeros(Fields, Pos, Len, Max_start, Max_len) ->
    case Fields of
        [] when Max_len > 1 ->
            {ok, {Max_start, Max_start + Max_len}};

        [] ->
            {error, nil};

        [X | Xs] when X =:= 0 ->
            Len@1 = Len + 1,
            case Len@1 > Max_len of
                true ->
                    ipv6_zeros(Xs, Pos + 1, Len@1, (Pos + 1) - Len@1, Len@1);

                false ->
                    ipv6_zeros(Xs, Pos + 1, Len@1, Max_start, Max_len)
            end;

        [_ | Xs@1] ->
            ipv6_zeros(Xs@1, Pos + 1, 0, Max_start, Max_len)
    end.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 104).
-spec ip_address_to_string(ip_address()) -> binary().
ip_address_to_string(Address) ->
    case Address of
        {ip_v4, A, B, C, D} ->
            _pipe = [A, B, C, D],
            _pipe@1 = gleam@list:map(_pipe, fun erlang:integer_to_binary/1),
            gleam@string:join(_pipe@1, <<"."/utf8>>);

        {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H} ->
            Fields = [A@1, B@1, C@1, D@1, E, F, G, H],
            _pipe@2 = case ipv6_zeros(Fields, 0, 0, 0, 0) of
                {error, _} ->
                    join_ipv6_fields(Fields);

                {ok, {Start, End}} ->
                    <<<<(join_ipv6_fields(gleam@list:take(Fields, Start)))/binary,
                            "::"/utf8>>/binary,
                        (join_ipv6_fields(gleam@list:drop(Fields, End)))/binary>>
            end,
            string:lowercase(_pipe@2)
    end.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 154).
-spec get_client_info(connection(any())) -> {ok, connection_info()} |
    {error, nil}.
get_client_info(Conn) ->
    _pipe = glisten@transport:peername(
        erlang:element(3, Conn),
        erlang:element(2, Conn)
    ),
    gleam@result:map(
        _pipe,
        fun(Pair) ->
            {connection_info,
                erlang:element(2, Pair),
                convert_ip_address(erlang:element(1, Pair))}
        end
    ).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 162).
-spec send(connection(any()), gleam@bytes_tree:bytes_tree()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
send(Conn, Msg) ->
    glisten@transport:send(
        erlang:element(3, Conn),
        erlang:element(2, Conn),
        Msg
    ).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 223).
-spec convert_on_init(
    fun((connection(IRO)) -> {IRQ,
        gleam@option:option(gleam@erlang@process:selector(IRO))})
) -> fun((glisten@internal@handler:connection(IRO)) -> {IRQ,
    gleam@option:option(gleam@erlang@process:selector(IRO))}).
convert_on_init(On_init) ->
    fun(Conn) ->
        Connection = {connection,
            erlang:element(3, Conn),
            erlang:element(4, Conn),
            erlang:element(5, Conn)},
        On_init(Connection)
    end.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 242).
-spec handler(
    fun((connection(IRW)) -> {IRY,
        gleam@option:option(gleam@erlang@process:selector(IRW))}),
    fun((message(IRW), IRY, connection(IRW)) -> gleam@otp@actor:next(message(IRW), IRY))
) -> handler(IRW, IRY).
handler(On_init, Loop) ->
    {handler, loopback, On_init, Loop, none, 10, false, false}.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 188).
-spec map_user_selector(gleam@erlang@process:selector(message(IRD))) -> gleam@erlang@process:selector(glisten@internal@handler:loop_message(IRD)).
map_user_selector(Selector) ->
    gleam_erlang_ffi:map_selector(Selector, fun(Value) -> case Value of
                {packet, Msg} ->
                    {packet, Msg};

                {user, Msg@1} ->
                    {custom, Msg@1}
            end end).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 199).
-spec convert_loop(
    fun((message(IRI), IRJ, connection(IRI)) -> gleam@otp@actor:next(message(IRI), IRJ))
) -> fun((glisten@internal@handler:loop_message(IRI), IRJ, glisten@internal@handler:connection(IRI)) -> gleam@otp@actor:next(glisten@internal@handler:loop_message(IRI), IRJ)).
convert_loop(Loop) ->
    fun(Msg, Data, Conn) ->
        Conn@1 = {connection,
            erlang:element(3, Conn),
            erlang:element(4, Conn),
            erlang:element(5, Conn)},
        case Msg of
            {packet, Msg@1} ->
                case Loop({packet, Msg@1}, Data, Conn@1) of
                    {continue, Data@1, Selector} ->
                        {continue,
                            Data@1,
                            gleam@option:map(Selector, fun map_user_selector/1)};

                    {stop, Reason} ->
                        {stop, Reason}
                end;

            {custom, Msg@2} ->
                case Loop({user, Msg@2}, Data, Conn@1) of
                    {continue, Data@2, Selector@1} ->
                        {continue,
                            Data@2,
                            gleam@option:map(
                                Selector@1,
                                fun map_user_selector/1
                            )};

                    {stop, Reason@1} ->
                        {stop, Reason@1}
                end
        end
    end.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 259).
-spec with_close(handler(ISF, ISG), fun((ISG) -> nil)) -> handler(ISF, ISG).
with_close(Handler, On_close) ->
    _record = Handler,
    {handler,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        {some, On_close},
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record)}.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 267).
-spec with_pool_size(handler(ISL, ISM), integer()) -> handler(ISL, ISM).
with_pool_size(Handler, Size) ->
    _record = Handler,
    {handler,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        Size,
        erlang:element(7, _record),
        erlang:element(8, _record)}.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 277).
-spec with_http2(handler(ISR, ISS)) -> handler(ISR, ISS).
with_http2(Handler) ->
    _record = Handler,
    {handler,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        true,
        erlang:element(8, _record)}.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 305).
-spec with_ipv6(handler(ITD, ITE)) -> handler(ITD, ITE).
with_ipv6(Handler) ->
    _record = Handler,
    {handler,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        true}.

-file("/home/alex/gleams/glisten/src/glisten.gleam", 335).
-spec start_server(handler(any(), any()), integer()) -> {ok, server()} |
    {error, start_error()}.
start_server(Handler, Port) ->
    Return = gleam@erlang@process:new_subject(),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(_pipe, Return, fun(Subj) -> Subj end)
    end,
    Options = case erlang:element(8, Handler) of
        true ->
            [{ip, erlang:element(2, Handler)}, ipv6];

        false ->
            [{ip, erlang:element(2, Handler)}]
    end,
    _pipe@1 = {pool,
        convert_loop(erlang:element(4, Handler)),
        erlang:element(6, Handler),
        convert_on_init(erlang:element(3, Handler)),
        erlang:element(5, Handler),
        tcp},
    _pipe@2 = glisten@internal@acceptor:start_pool(
        _pipe@1,
        tcp,
        Port,
        Options,
        Return
    ),
    _pipe@3 = gleam@result:map_error(_pipe@2, fun(Err) -> case Err of
                init_timeout ->
                    acceptor_timeout;

                {init_failed, Reason} ->
                    {acceptor_failed, Reason};

                {init_crashed, Reason@1} ->
                    {acceptor_crashed, Reason@1}
            end end),
    gleam@result:then(
        _pipe@3,
        fun(Pool) -> _pipe@4 = gleam_erlang_ffi:select(Selector, 1500),
            _pipe@5 = gleam@result:map(
                _pipe@4,
                fun(Listener) -> {server, Listener, Pool, tcp} end
            ),
            gleam@result:replace_error(_pipe@5, acceptor_timeout) end
    ).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 312).
-spec serve(handler(any(), any()), integer()) -> {ok,
        gleam@erlang@process:subject(gleam@otp@supervisor:message())} |
    {error, start_error()}.
serve(Handler, Port) ->
    _pipe = start_server(Handler, Port),
    gleam@result:map(_pipe, fun get_supervisor/1).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 377).
-spec start_ssl_server(handler(any(), any()), integer(), binary(), binary()) -> {ok,
        server()} |
    {error, start_error()}.
start_ssl_server(Handler, Port, Certfile, Keyfile) ->
    Base_options = [{ip, erlang:element(2, Handler)},
        {certfile, Certfile},
        {keyfile, Keyfile}],
    Default_options = case erlang:element(8, Handler) of
        true ->
            [ipv6 | Base_options];

        false ->
            Base_options
    end,
    Protocol_options = case erlang:element(7, Handler) of
        true ->
            [{alpn_preferred_protocols, [<<"h2"/utf8>>, <<"http/1.1"/utf8>>]}];

        false ->
            [{alpn_preferred_protocols, [<<"http/1.1"/utf8>>]}]
    end,
    Return = gleam@erlang@process:new_subject(),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(_pipe, Return, fun(Subj) -> Subj end)
    end,
    _pipe@1 = {pool,
        convert_loop(erlang:element(4, Handler)),
        erlang:element(6, Handler),
        convert_on_init(erlang:element(3, Handler)),
        erlang:element(5, Handler),
        ssl},
    _pipe@2 = glisten@internal@acceptor:start_pool(
        _pipe@1,
        ssl,
        Port,
        gleam@list:flatten([Default_options, Protocol_options]),
        Return
    ),
    _pipe@3 = gleam@result:map_error(_pipe@2, fun(Err) -> case Err of
                init_timeout ->
                    acceptor_timeout;

                {init_failed, Reason} ->
                    {acceptor_failed, Reason};

                {init_crashed, Reason@1} ->
                    {acceptor_crashed, Reason@1}
            end end),
    gleam@result:then(
        _pipe@3,
        fun(Pool) -> _pipe@4 = gleam_erlang_ffi:select(Selector, 1500),
            _pipe@5 = gleam@result:map(
                _pipe@4,
                fun(Listener) -> {server, Listener, Pool, tcp} end
            ),
            gleam@result:replace_error(_pipe@5, acceptor_timeout) end
    ).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 322).
-spec serve_ssl(handler(any(), any()), integer(), binary(), binary()) -> {ok,
        gleam@erlang@process:subject(gleam@otp@supervisor:message())} |
    {error, start_error()}.
serve_ssl(Handler, Port, Certfile, Keyfile) ->
    _pipe = start_ssl_server(Handler, Port, Certfile, Keyfile),
    gleam@result:map(_pipe, fun get_supervisor/1).

-file("/home/alex/gleams/glisten/src/glisten.gleam", 287).
-spec bind(handler(ISX, ISY), binary()) -> handler(ISX, ISY).
bind(Handler, Interface) ->
    Address@1 = case {Interface,
        glisten_ffi:parse_address(unicode:characters_to_list(Interface))} of
        {<<"0.0.0.0"/utf8>>, _} ->
            any;

        {<<"localhost"/utf8>>, _} ->
            loopback;

        {<<"127.0.0.1"/utf8>>, _} ->
            loopback;

        {_, {ok, Address}} ->
            {address, Address};

        {_, {error, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Invalid interface provided:  must be a valid IPv4/IPv6 address, or \"localhost\""/utf8>>,
                    module => <<"glisten"/utf8>>,
                    function => <<"bind"/utf8>>,
                    line => 296})
    end,
    _record = Handler,
    {handler,
        Address@1,
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record)}.
