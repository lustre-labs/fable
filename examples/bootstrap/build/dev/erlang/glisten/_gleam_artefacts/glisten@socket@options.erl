-module(glisten@socket@options).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_dict/1, merge_with_defaults/1]).
-export_type([socket_mode/0, active_state/0, interface/0, tcp_option/0, ip_address/0]).

-type socket_mode() :: binary.

-type active_state() :: once | passive | {count, integer()} | active.

-type interface() :: {address, ip_address()} | any | loopback.

-type tcp_option() :: {backlog, integer()} |
    {nodelay, boolean()} |
    {linger, {boolean(), integer()}} |
    {send_timeout, integer()} |
    {send_timeout_close, boolean()} |
    {reuseaddr, boolean()} |
    {active_mode, active_state()} |
    {mode, socket_mode()} |
    {certfile, binary()} |
    {keyfile, binary()} |
    {alpn_preferred_protocols, list(binary())} |
    ipv6 |
    {buffer, integer()} |
    {ip, interface()}.

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

-file("src/glisten/socket/options.gleam", 45).
-spec to_dict(list(tcp_option())) -> gleam@dict:dict(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
to_dict(Options) ->
    Opt_decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
            fun(Opt) ->
                gleam@dynamic@decode:field(
                    1,
                    {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                    fun(Value) -> gleam@dynamic@decode:success({Opt, Value}) end
                )
            end
        )
    end,
    Active = erlang:binary_to_atom(<<"active"/utf8>>),
    Ip = erlang:binary_to_atom(<<"ip"/utf8>>),
    _pipe = Options,
    _pipe@1 = gleam@list:map(_pipe, fun(Opt@1) -> case Opt@1 of
                {active_mode, passive} ->
                    gleam_stdlib:identity({Active, false});

                {active_mode, active} ->
                    gleam_stdlib:identity({Active, true});

                {active_mode, {count, N}} ->
                    gleam_stdlib:identity({Active, N});

                {active_mode, once} ->
                    gleam_stdlib:identity(
                        {Active, erlang:binary_to_atom(<<"once"/utf8>>)}
                    );

                {ip, {address, {ip_v4, A, B, C, D}}} ->
                    gleam_stdlib:identity(
                        {Ip, gleam_stdlib:identity({A, B, C, D})}
                    );

                {ip, {address, {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H}}} ->
                    gleam_stdlib:identity(
                        {Ip,
                            gleam_stdlib:identity(
                                {A@1, B@1, C@1, D@1, E, F, G, H}
                            )}
                    );

                {ip, any} ->
                    gleam_stdlib:identity(
                        {Ip, erlang:binary_to_atom(<<"any"/utf8>>)}
                    );

                {ip, loopback} ->
                    gleam_stdlib:identity(
                        {Ip, erlang:binary_to_atom(<<"loopback"/utf8>>)}
                    );

                ipv6 ->
                    gleam_stdlib:identity(
                        erlang:binary_to_atom(<<"inet6"/utf8>>)
                    );

                Other ->
                    gleam_stdlib:identity(Other)
            end end),
    _pipe@2 = gleam@list:filter_map(
        _pipe@1,
        fun(_capture) -> gleam@dynamic@decode:run(_capture, Opt_decoder) end
    ),
    maps:from_list(_pipe@2).

-file("src/glisten/socket/options.gleam", 87).
-spec merge_with_defaults(list(tcp_option())) -> list(tcp_option()).
merge_with_defaults(Options) ->
    Overrides = to_dict(Options),
    Has_ipv6 = gleam@list:contains(Options, ipv6),
    _pipe = [{backlog, 1024},
        {nodelay, true},
        {send_timeout, 30000},
        {send_timeout_close, true},
        {reuseaddr, true},
        {mode, binary},
        {active_mode, passive}],
    _pipe@1 = to_dict(_pipe),
    _pipe@2 = maps:merge(_pipe@1, Overrides),
    _pipe@3 = maps:to_list(_pipe@2),
    _pipe@4 = gleam@list:map(_pipe@3, fun gleam_stdlib:identity/1),
    _pipe@5 = (fun(Opts) -> case Has_ipv6 of
            true ->
                [gleam_stdlib:identity(erlang:binary_to_atom(<<"inet6"/utf8>>)) |
                    Opts];

            _ ->
                Opts
        end end)(_pipe@4),
    gleam@function:identity(_pipe@5).
