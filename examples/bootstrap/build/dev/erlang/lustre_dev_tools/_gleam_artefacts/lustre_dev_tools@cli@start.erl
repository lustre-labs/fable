-module(lustre_dev_tools@cli@start).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([run/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/cli/start.gleam", 64).
?DOC(false).
-spec check_otp_version() -> lustre_dev_tools@cli:cli(nil).
check_otp_version() ->
    lustre_dev_tools@cli:log(
        <<"Checking OTP version"/utf8>>,
        fun() ->
            Version = lustre_dev_tools@project:otp_version(),
            case Version =< 25 of
                false ->
                    lustre_dev_tools@cli:return(nil);

                true ->
                    lustre_dev_tools@cli:throw({otp_too_old, Version})
            end
        end
    ).

-file("src/lustre_dev_tools/cli/start.gleam", 91).
?DOC(false).
-spec write_html(binary(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
write_html(Path, Source) ->
    _pipe = simplifile:write(Path, Source),
    gleam@result:map_error(
        _pipe,
        fun(_capture) -> {cannot_write_file, _capture, Path} end
    ).

-file("src/lustre_dev_tools/cli/start.gleam", 73).
?DOC(false).
-spec prepare_html() -> lustre_dev_tools@cli:cli(nil).
prepare_html() ->
    _assert_subject = lustre_dev_tools_ffi:get_cwd(),
    {ok, Cwd} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/cli/start"/utf8>>,
                        function => <<"prepare_html"/utf8>>,
                        line => 74})
    end,
    _assert_subject@1 = filepath:expand(
        filepath:join(Cwd, lustre_dev_tools@project:root())
    ),
    {ok, Root} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre_dev_tools/cli/start"/utf8>>,
                        function => <<"prepare_html"/utf8>>,
                        line => 75})
    end,
    Index = filepath:join(Root, <<"index.html"/utf8>>),
    case simplifile_erl:is_file(Index) of
        {ok, true} ->
            lustre_dev_tools@cli:return(nil);

        {ok, false} ->
            lustre_dev_tools@cli:template(
                <<"index.html"/utf8>>,
                fun(Html) ->
                    lustre_dev_tools@cli:do(
                        lustre_dev_tools@cli:get_name(),
                        fun(App_name) ->
                            Html@1 = gleam@string:replace(
                                Html,
                                <<"{app_name}"/utf8>>,
                                App_name
                            ),
                            lustre_dev_tools@cli:'try'(
                                write_html(Index, Html@1),
                                fun(_) -> lustre_dev_tools@cli:return(nil) end
                            )
                        end
                    )
                end
            );

        {error, _} ->
            lustre_dev_tools@cli:template(
                <<"index.html"/utf8>>,
                fun(Html) ->
                    lustre_dev_tools@cli:do(
                        lustre_dev_tools@cli:get_name(),
                        fun(App_name) ->
                            Html@1 = gleam@string:replace(
                                Html,
                                <<"{app_name}"/utf8>>,
                                App_name
                            ),
                            lustre_dev_tools@cli:'try'(
                                write_html(Index, Html@1),
                                fun(_) -> lustre_dev_tools@cli:return(nil) end
                            )
                        end
                    )
                end
            )
    end.

-file("src/lustre_dev_tools/cli/start.gleam", 19).
?DOC(false).
-spec run() -> glint:command(nil).
run() ->
    Description = <<"
Start a development server for your Lustre project. This command will compile your
application and serve it on a local server.
    "/utf8>>,
    glint:command_help(
        Description,
        fun() ->
            glint:unnamed_args(
                {eq_args, 0},
                fun() ->
                    glint:flag(
                        lustre_dev_tools@cli@flag:port(),
                        fun(Port) ->
                            glint:flag(
                                lustre_dev_tools@cli@flag:bind(),
                                fun(Bind) ->
                                    glint:flag(
                                        lustre_dev_tools@cli@flag:proxy_from(),
                                        fun(_) ->
                                            glint:flag(
                                                lustre_dev_tools@cli@flag:proxy_to(
                                                    
                                                ),
                                                fun(_) ->
                                                    glint:flag(
                                                        lustre_dev_tools@cli@flag:detect_tailwind(
                                                            
                                                        ),
                                                        fun(Detect_tailwind) ->
                                                            glint:flag(
                                                                lustre_dev_tools@cli@flag:tailwind_entry(
                                                                    
                                                                ),
                                                                fun(_) ->
                                                                    glint:flag(
                                                                        lustre_dev_tools@cli@flag:entry(
                                                                            
                                                                        ),
                                                                        fun(
                                                                            Entry
                                                                        ) ->
                                                                            glint:command(
                                                                                fun(
                                                                                    _,
                                                                                    _,
                                                                                    Flags
                                                                                ) ->
                                                                                    Script = begin
                                                                                        lustre_dev_tools@cli:do(
                                                                                            lustre_dev_tools@cli:get_name(
                                                                                                
                                                                                            ),
                                                                                            fun(
                                                                                                Project_name
                                                                                            ) ->
                                                                                                lustre_dev_tools@cli:do(
                                                                                                    lustre_dev_tools@cli:get_int(
                                                                                                        <<"port"/utf8>>,
                                                                                                        1234,
                                                                                                        [<<"start"/utf8>>],
                                                                                                        Port
                                                                                                    ),
                                                                                                    fun(
                                                                                                        Port@1
                                                                                                    ) ->
                                                                                                        lustre_dev_tools@cli:do(
                                                                                                            lustre_dev_tools@cli:get_string(
                                                                                                                <<"bind"/utf8>>,
                                                                                                                <<"localhost"/utf8>>,
                                                                                                                [<<"start"/utf8>>],
                                                                                                                Bind
                                                                                                            ),
                                                                                                            fun(
                                                                                                                Bind@1
                                                                                                            ) ->
                                                                                                                lustre_dev_tools@cli:do(
                                                                                                                    lustre_dev_tools@cli:get_bool(
                                                                                                                        <<"detect_tailwind"/utf8>>,
                                                                                                                        true,
                                                                                                                        [<<"build"/utf8>>],
                                                                                                                        Detect_tailwind
                                                                                                                    ),
                                                                                                                    fun(
                                                                                                                        Detect_tailwind@1
                                                                                                                    ) ->
                                                                                                                        lustre_dev_tools@cli:do(
                                                                                                                            lustre_dev_tools@cli:get_string(
                                                                                                                                <<"entry"/utf8>>,
                                                                                                                                Project_name,
                                                                                                                                [<<"build"/utf8>>],
                                                                                                                                Entry
                                                                                                                            ),
                                                                                                                            fun(
                                                                                                                                Entry@1
                                                                                                                            ) ->
                                                                                                                                lustre_dev_tools@cli:do(
                                                                                                                                    check_otp_version(
                                                                                                                                        
                                                                                                                                    ),
                                                                                                                                    fun(
                                                                                                                                        _
                                                                                                                                    ) ->
                                                                                                                                        lustre_dev_tools@cli:do(
                                                                                                                                            lustre_dev_tools@cli@build:do_app(
                                                                                                                                                Entry@1,
                                                                                                                                                false,
                                                                                                                                                Detect_tailwind@1
                                                                                                                                            ),
                                                                                                                                            fun(
                                                                                                                                                _
                                                                                                                                            ) ->
                                                                                                                                                lustre_dev_tools@cli:do(
                                                                                                                                                    prepare_html(
                                                                                                                                                        
                                                                                                                                                    ),
                                                                                                                                                    fun(
                                                                                                                                                        _
                                                                                                                                                    ) ->
                                                                                                                                                        lustre_dev_tools@cli:do(
                                                                                                                                                            lustre_dev_tools@server:start(
                                                                                                                                                                Entry@1,
                                                                                                                                                                Port@1,
                                                                                                                                                                Bind@1
                                                                                                                                                            ),
                                                                                                                                                            fun(
                                                                                                                                                                _
                                                                                                                                                            ) ->
                                                                                                                                                                lustre_dev_tools@cli:return(
                                                                                                                                                                    nil
                                                                                                                                                                )
                                                                                                                                                            end
                                                                                                                                                        )
                                                                                                                                                    end
                                                                                                                                                )
                                                                                                                                            end
                                                                                                                                        )
                                                                                                                                    end
                                                                                                                                )
                                                                                                                            end
                                                                                                                        )
                                                                                                                    end
                                                                                                                )
                                                                                                            end
                                                                                                        )
                                                                                                    end
                                                                                                )
                                                                                            end
                                                                                        )
                                                                                    end,
                                                                                    case lustre_dev_tools@cli:run(
                                                                                        Script,
                                                                                        Flags
                                                                                    ) of
                                                                                        {ok,
                                                                                            _} ->
                                                                                            nil;

                                                                                        {error,
                                                                                            Error} ->
                                                                                            _pipe = lustre_dev_tools@error:explain(
                                                                                                Error
                                                                                            ),
                                                                                            gleam_stdlib:print_error(
                                                                                                _pipe
                                                                                            )
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
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
