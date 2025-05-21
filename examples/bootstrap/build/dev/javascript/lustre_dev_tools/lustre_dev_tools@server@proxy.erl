-module(lustre_dev_tools@server@proxy).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([middleware/3, get/0]).
-export_type([proxy/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type proxy() :: {proxy, binary(), gleam@uri:uri()}.

-file("src/lustre_dev_tools/server/proxy.gleam", 26).
?DOC(false).
-spec middleware(
    gleam@http@request:request(mist@internal@http:connection()),
    gleam@option:option(proxy()),
    fun(() -> gleam@http@response:response(mist:response_data()))
) -> gleam@http@response:response(mist:response_data()).
middleware(Req, Proxy, K) ->
    case Proxy of
        none ->
            K();

        {some, {proxy, From, To}} ->
            gleam@bool:lazy_guard(
                not gleam_stdlib:string_starts_with(
                    erlang:element(8, Req),
                    From
                ),
                K,
                fun() ->
                    Internal_error = begin
                        _pipe = gleam@http@response:new(500),
                        gleam@http@response:set_body(
                            _pipe,
                            {bytes, gleam@bytes_tree:new()}
                        )
                    end,
                    _assert_subject = erlang:element(4, To),
                    {some, Host} = case _assert_subject of
                        {some, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail,
                                        module => <<"lustre_dev_tools/server/proxy"/utf8>>,
                                        function => <<"middleware"/utf8>>,
                                        line => 39})
                    end,
                    Path = begin
                        _pipe@1 = erlang:element(8, Req),
                        _pipe@2 = gleam@string:replace(
                            _pipe@1,
                            From,
                            <<""/utf8>>
                        ),
                        filepath:join(erlang:element(6, To), _pipe@2)
                    end,
                    _assert_subject@1 = mist:read_body(Req, (100 * 1024) * 1024),
                    {ok, Req@1} = case _assert_subject@1 of
                        {ok, _} -> _assert_subject@1;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail@1,
                                        module => <<"lustre_dev_tools/server/proxy"/utf8>>,
                                        function => <<"middleware"/utf8>>,
                                        line => 44})
                    end,
                    _pipe@3 = begin
                        _record = Req@1,
                        {request,
                            erlang:element(2, _record),
                            erlang:element(3, _record),
                            erlang:element(4, _record),
                            erlang:element(5, _record),
                            Host,
                            erlang:element(5, To),
                            Path,
                            erlang:element(9, _record)}
                    end,
                    _pipe@4 = gleam@httpc:send_bits(_pipe@3),
                    _pipe@5 = gleam@result:map(
                        _pipe@4,
                        fun(_capture) ->
                            gleam@http@response:map(
                                _capture,
                                fun gleam@bytes_tree:from_bit_array/1
                            )
                        end
                    ),
                    _pipe@6 = gleam@result:map(
                        _pipe@5,
                        fun(_capture@1) ->
                            gleam@http@response:map(
                                _capture@1,
                                fun(Field@0) -> {bytes, Field@0} end
                            )
                        end
                    ),
                    gleam@result:unwrap(_pipe@6, Internal_error)
                end
            )
    end.

-file("src/lustre_dev_tools/server/proxy.gleam", 67).
?DOC(false).
-spec get_proxy_from() -> lustre_dev_tools@cli:cli(gleam@option:option(binary())).
get_proxy_from() ->
    lustre_dev_tools@cli:do(
        lustre_dev_tools@cli:get_flags(),
        fun(Flags) ->
            lustre_dev_tools@cli:do(
                lustre_dev_tools@cli:get_config(),
                fun(Config) ->
                    Flag = gleam@result:replace_error(
                        glint:get_flag(
                            Flags,
                            lustre_dev_tools@cli@flag:proxy_from()
                        ),
                        nil
                    ),
                    Toml = gleam@result:replace_error(
                        tom:get_string(
                            erlang:element(3, Config),
                            [<<"lustre-dev"/utf8>>,
                                <<"start"/utf8>>,
                                <<"proxy"/utf8>>,
                                <<"from"/utf8>>]
                        ),
                        nil
                    ),
                    _pipe = gleam@result:'or'(Flag, Toml),
                    _pipe@1 = gleam@option:from_result(_pipe),
                    lustre_dev_tools@cli:return(_pipe@1)
                end
            )
        end
    ).

-file("src/lustre_dev_tools/server/proxy.gleam", 83).
?DOC(false).
-spec get_proxy_to() -> lustre_dev_tools@cli:cli(gleam@option:option(gleam@uri:uri())).
get_proxy_to() ->
    lustre_dev_tools@cli:do(
        lustre_dev_tools@cli:get_flags(),
        fun(Flags) ->
            lustre_dev_tools@cli:do(
                lustre_dev_tools@cli:get_config(),
                fun(Config) ->
                    Flag = gleam@result:replace_error(
                        glint:get_flag(
                            Flags,
                            lustre_dev_tools@cli@flag:proxy_to()
                        ),
                        nil
                    ),
                    Toml = gleam@result:replace_error(
                        tom:get_string(
                            erlang:element(3, Config),
                            [<<"lustre-dev"/utf8>>,
                                <<"start"/utf8>>,
                                <<"proxy"/utf8>>,
                                <<"to"/utf8>>]
                        ),
                        nil
                    ),
                    From = gleam@result:'or'(Flag, Toml),
                    gleam@bool:guard(
                        From =:= {error, nil},
                        lustre_dev_tools@cli:return(none),
                        fun() ->
                            {ok, From@1} = case From of
                                {ok, _} -> From;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail,
                                                module => <<"lustre_dev_tools/server/proxy"/utf8>>,
                                                function => <<"get_proxy_to"/utf8>>,
                                                line => 96})
                            end,
                            case gleam_stdlib:uri_parse(From@1) of
                                {ok, From@2} ->
                                    lustre_dev_tools@cli:return({some, From@2});

                                {error, _} ->
                                    lustre_dev_tools@cli:throw(
                                        {invalid_proxy_target, From@1}
                                    )
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre_dev_tools/server/proxy.gleam", 55).
?DOC(false).
-spec get() -> lustre_dev_tools@cli:cli(gleam@option:option(proxy())).
get() ->
    lustre_dev_tools@cli:do(
        get_proxy_from(),
        fun(From) ->
            lustre_dev_tools@cli:do(
                get_proxy_to(),
                fun(To) -> case {From, To} of
                        {{some, From@1}, {some, To@1}} ->
                            lustre_dev_tools@cli:return(
                                {some, {proxy, From@1, To@1}}
                            );

                        {{some, _}, none} ->
                            lustre_dev_tools@cli:throw(
                                {incomplete_proxy, [<<"proxy-to"/utf8>>]}
                            );

                        {none, {some, _}} ->
                            lustre_dev_tools@cli:throw(
                                {incomplete_proxy, [<<"proxy-from"/utf8>>]}
                            );

                        {none, none} ->
                            lustre_dev_tools@cli:return(none)
                    end end
            )
        end
    ).
