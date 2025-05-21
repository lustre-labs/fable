-module(lustre_dev_tools@cli).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([run/2, return/1, throw/1, from_result/1, do/2, in/1, map/2, 'try'/2, log/2, success/2, notify/2, mute/0, unmute/0, template/2, get_config/0, get_name/0, get_flags/0, get_config_value/5, get_int/4, get_string/4, get_bool/4]).
-export_type([cli/1, env/0, spinner_status/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque cli(XSJ) :: {cli,
        fun((env()) -> {env(),
            {ok, XSJ} | {error, lustre_dev_tools@error:error()}})}.

-type env() :: {env,
        boolean(),
        spinner_status(),
        glint:flags(),
        lustre_dev_tools@project:config()}.

-type spinner_status() :: {running,
        lustre_dev_tools@vendor@spinner:spinner(),
        binary()} |
    paused.

-file("src/lustre_dev_tools/cli.gleam", 40).
?DOC(false).
-spec run(cli(XSK), glint:flags()) -> {ok, XSK} |
    {error, lustre_dev_tools@error:error()}.
run(Step, Flags) ->
    gleam@result:'try'(
        lustre_dev_tools@project:config(),
        fun(Config) ->
            Env = {env, false, paused, Flags, Config},
            {Env@1, Result} = (erlang:element(2, Step))(Env),
            case erlang:element(3, Env@1) of
                {running, Spinner, _} ->
                    lustre_dev_tools@vendor@spinner:stop(Spinner);

                paused ->
                    nil
            end,
            case {Result, erlang:element(3, Env@1)} of
                {{error, _}, {running, _, Message}} ->
                    gleam_stdlib:println(
                        <<"❌ "/utf8,
                            (gleam_community@ansi:red(Message))/binary>>
                    );

                {{error, _}, _} ->
                    nil;

                {{ok, _}, _} ->
                    nil
            end,
            Result
        end
    ).

-file("src/lustre_dev_tools/cli.gleam", 64).
?DOC(false).
-spec return(XSO) -> cli(XSO).
return(Value) ->
    {cli, fun(Env) -> {Env, {ok, Value}} end}.

-file("src/lustre_dev_tools/cli.gleam", 72).
?DOC(false).
-spec throw(lustre_dev_tools@error:error()) -> cli(any()).
throw(Error) ->
    {cli, fun(Env) -> {Env, {error, Error}} end}.

-file("src/lustre_dev_tools/cli.gleam", 78).
?DOC(false).
-spec from_result({ok, XSS} | {error, lustre_dev_tools@error:error()}) -> cli(XSS).
from_result(Result) ->
    {cli, fun(Env) -> {Env, Result} end}.

-file("src/lustre_dev_tools/cli.gleam", 88).
?DOC(false).
-spec do(cli(XSW), fun((XSW) -> cli(XSY))) -> cli(XSY).
do(Step, Next) ->
    {cli,
        fun(Env) ->
            {Env@1, Result} = (erlang:element(2, Step))(Env),
            case Result of
                {ok, Value} ->
                    (erlang:element(2, Next(Value)))(Env@1);

                {error, Error} ->
                    case erlang:element(3, Env@1) of
                        {running, Spinner, _} ->
                            lustre_dev_tools@vendor@spinner:stop(Spinner);

                        paused ->
                            nil
                    end,
                    {Env@1, {error, Error}}
            end
        end}.

-file("src/lustre_dev_tools/cli.gleam", 105).
?DOC(false).
-spec in(fun(() -> XTB)) -> cli(XTB).
in(Value) ->
    {cli, fun(Env) -> {Env, {ok, Value()}} end}.

-file("src/lustre_dev_tools/cli.gleam", 111).
?DOC(false).
-spec map(cli(XTD), fun((XTD) -> XTF)) -> cli(XTF).
map(Step, Next) ->
    {cli,
        fun(Env) ->
            {Env@1, Result} = (erlang:element(2, Step))(Env),
            Result@1 = gleam@result:map(Result, Next),
            {Env@1, Result@1}
        end}.

-file("src/lustre_dev_tools/cli.gleam", 121).
?DOC(false).
-spec 'try'(
    {ok, XTH} | {error, lustre_dev_tools@error:error()},
    fun((XTH) -> cli(XTK))
) -> cli(XTK).
'try'(Result, Next) ->
    {cli, fun(Env) -> case Result of
                {ok, A} ->
                    (erlang:element(2, Next(A)))(Env);

                {error, Error} ->
                    case erlang:element(3, Env) of
                        {running, Spinner, _} ->
                            lustre_dev_tools@vendor@spinner:stop(Spinner);

                        paused ->
                            nil
                    end,
                    {Env, {error, Error}}
            end end}.

-file("src/lustre_dev_tools/cli.gleam", 141).
?DOC(false).
-spec log(binary(), fun(() -> cli(XTN))) -> cli(XTN).
log(Message, Next) ->
    {cli,
        fun(Env) ->
            Env@1 = case erlang:element(2, Env) of
                true ->
                    Env;

                false ->
                    _record = Env,
                    {env,
                        erlang:element(2, _record),
                        case erlang:element(3, Env) of
                            paused ->
                                {running,
                                    begin
                                        _pipe = lustre_dev_tools@vendor@spinner:new(
                                            Message
                                        ),
                                        _pipe@1 = lustre_dev_tools@vendor@spinner:with_colour(
                                            _pipe,
                                            fun gleam_community@ansi:magenta/1
                                        ),
                                        _pipe@2 = lustre_dev_tools@vendor@spinner:with_frames(
                                            _pipe@1,
                                            [<<"⠋"/utf8>>,
                                                <<"⠙"/utf8>>,
                                                <<"⠹"/utf8>>,
                                                <<"⠸"/utf8>>,
                                                <<"⠼"/utf8>>,
                                                <<"⠴"/utf8>>,
                                                <<"⠦"/utf8>>,
                                                <<"⠧"/utf8>>,
                                                <<"⠇"/utf8>>,
                                                <<"⠏"/utf8>>]
                                        ),
                                        lustre_dev_tools@vendor@spinner:start(
                                            _pipe@2
                                        )
                                    end,
                                    Message};

                            {running, Spinner, _} ->
                                lustre_dev_tools@vendor@spinner:set_text(
                                    Spinner,
                                    Message
                                ),
                                {running, Spinner, Message}
                        end,
                        erlang:element(4, _record),
                        erlang:element(5, _record)}
            end,
            (erlang:element(2, Next()))(Env@1)
        end}.

-file("src/lustre_dev_tools/cli.gleam", 166).
?DOC(false).
-spec success(binary(), fun(() -> cli(XTQ))) -> cli(XTQ).
success(Message, Next) ->
    {cli,
        fun(Env) ->
            Env@1 = begin
                _record = Env,
                {env, erlang:element(2, _record), case erlang:element(3, Env) of
                        paused ->
                            paused;

                        {running, Spinner, _} ->
                            lustre_dev_tools@vendor@spinner:stop(Spinner),
                            paused
                    end, erlang:element(4, _record), erlang:element(5, _record)}
            end,
            case erlang:element(2, Env@1) of
                true ->
                    nil;

                false ->
                    gleam_stdlib:println(
                        <<"✅ "/utf8,
                            (gleam_community@ansi:green(Message))/binary>>
                    )
            end,
            (erlang:element(2, Next()))(Env@1)
        end}.

-file("src/lustre_dev_tools/cli.gleam", 185).
?DOC(false).
-spec notify(binary(), fun(() -> cli(XTT))) -> cli(XTT).
notify(Message, Next) ->
    {cli,
        fun(Env) ->
            Env@1 = begin
                _record = Env,
                {env, erlang:element(2, _record), case erlang:element(3, Env) of
                        paused ->
                            paused;

                        {running, Spinner, _} ->
                            lustre_dev_tools@vendor@spinner:stop(Spinner),
                            paused
                    end, erlang:element(4, _record), erlang:element(5, _record)}
            end,
            case erlang:element(2, Env@1) of
                true ->
                    nil;

                false ->
                    gleam_stdlib:println(
                        gleam_community@ansi:bright_cyan(Message)
                    )
            end,
            (erlang:element(2, Next()))(Env@1)
        end}.

-file("src/lustre_dev_tools/cli.gleam", 204).
?DOC(false).
-spec mute() -> cli(nil).
mute() ->
    {cli,
        fun(Env) ->
            {begin
                    _record = Env,
                    {env,
                        true,
                        erlang:element(3, _record),
                        erlang:element(4, _record),
                        erlang:element(5, _record)}
                end,
                {ok, nil}}
        end}.

-file("src/lustre_dev_tools/cli.gleam", 210).
?DOC(false).
-spec unmute() -> cli(nil).
unmute() ->
    {cli,
        fun(Env) ->
            {begin
                    _record = Env,
                    {env,
                        false,
                        erlang:element(3, _record),
                        erlang:element(4, _record),
                        erlang:element(5, _record)}
                end,
                {ok, nil}}
        end}.

-file("src/lustre_dev_tools/cli.gleam", 218).
?DOC(false).
-spec template(binary(), fun((binary()) -> cli(XTY))) -> cli(XTY).
template(Name, Next) ->
    {cli,
        fun(Env) ->
            _assert_subject = gleam_erlang_ffi:priv_directory(
                <<"lustre_dev_tools"/utf8>>
            ),
            {ok, Priv} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/cli"/utf8>>,
                                function => <<"template"/utf8>>,
                                line => 220})
            end,
            case simplifile:read(
                <<<<Priv/binary, "/template/"/utf8>>/binary, Name/binary>>
            ) of
                {ok, Template} ->
                    (erlang:element(2, Next(Template)))(Env);

                {error, Error} ->
                    {Env, {error, {template_missing, Name, Error}}}
            end
        end}.

-file("src/lustre_dev_tools/cli.gleam", 230).
?DOC(false).
-spec get_config() -> cli(lustre_dev_tools@project:config()).
get_config() ->
    {cli, fun(Env) -> {Env, {ok, erlang:element(5, Env)}} end}.

-file("src/lustre_dev_tools/cli.gleam", 236).
?DOC(false).
-spec get_name() -> cli(binary()).
get_name() ->
    {cli,
        fun(Env) -> {Env, {ok, erlang:element(2, erlang:element(5, Env))}} end}.

-file("src/lustre_dev_tools/cli.gleam", 244).
?DOC(false).
-spec get_flags() -> cli(glint:flags()).
get_flags() ->
    {cli, fun(Env) -> {Env, {ok, erlang:element(4, Env)}} end}.

-file("src/lustre_dev_tools/cli.gleam", 250).
?DOC(false).
-spec get_config_value(
    binary(),
    XUE,
    list(binary()),
    fun((gleam@dict:dict(binary(), tom:toml()), list(binary())) -> {ok, XUE} |
        {error, any()}),
    fun((glint:flags()) -> {ok, XUE} | {error, any()})
) -> cli(XUE).
get_config_value(Name, Fallback, Namespace, Toml, Flag) ->
    {cli,
        fun(Env) ->
            Toml_path = gleam@list:flatten(
                [[<<"lustre-dev"/utf8>>], Namespace, [Name]]
            ),
            Value = begin
                _pipe = gleam@result:'or'(
                    gleam@result:replace_error(
                        Flag(erlang:element(4, Env)),
                        nil
                    ),
                    gleam@result:replace_error(
                        Toml(
                            erlang:element(3, erlang:element(5, Env)),
                            Toml_path
                        ),
                        nil
                    )
                ),
                gleam@result:unwrap(_pipe, Fallback)
            end,
            {Env, {ok, Value}}
        end}.

-file("src/lustre_dev_tools/cli.gleam", 269).
?DOC(false).
-spec get_int(
    binary(),
    integer(),
    list(binary()),
    fun((glint:flags()) -> {ok, integer()} | {error, any()})
) -> cli(integer()).
get_int(Name, Fallback, Namespace, Flag) ->
    get_config_value(Name, Fallback, Namespace, fun tom:get_int/2, Flag).

-file("src/lustre_dev_tools/cli.gleam", 278).
?DOC(false).
-spec get_string(
    binary(),
    binary(),
    list(binary()),
    fun((glint:flags()) -> {ok, binary()} | {error, any()})
) -> cli(binary()).
get_string(Name, Fallback, Namespace, Flag) ->
    get_config_value(Name, Fallback, Namespace, fun tom:get_string/2, Flag).

-file("src/lustre_dev_tools/cli.gleam", 287).
?DOC(false).
-spec get_bool(
    binary(),
    boolean(),
    list(binary()),
    fun((glint:flags()) -> {ok, boolean()} | {error, any()})
) -> cli(boolean()).
get_bool(Name, Fallback, Namespace, Flag) ->
    get_config_value(Name, Fallback, Namespace, fun tom:get_bool/2, Flag).
