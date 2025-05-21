-module(modem).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([initial_uri/0, init/1, advanced/2, load/1, forward/1, back/1, simulate/4, push/3, replace/3]).
-export_type([options/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " > **modem**: a device that converts signals produced by one type of device\n"
    " > (such as a computer) to a form compatible with another (such as a\n"
    " > telephone) –  [Merriam-Webster](https://www.merriam-webster.com/dictionary/modem)\n"
    "\n"
    " Modem is a little library for Lustre that helps you manage navigation and URLs\n"
    " in the browser. It converts url requests into messages that you can handle\n"
    " in your app's update function. Modem isn't a router, but it can help you\n"
    " build one!\n"
    "\n"
    "\n"
).

-type options() :: {options, boolean(), boolean()}.

-file("src/modem.gleam", 73).
?DOC(
    " Get the `Uri` of the page when it first loaded. This can be useful to read\n"
    " in your own app's `init` function so you can choose the correct initial\n"
    " route for your app.\n"
    "\n"
    " To subscribe to changes in the uri when a user navigates around your app, see\n"
    " the [`init`](#init) and [`advanced`](#advanced) functions.\n"
    "\n"
    " > **Note**: this function is only meaningful when run in the browser. When\n"
    " > run in a backend JavaScript environment or in Erlang this function will\n"
    " > always fail.\n"
).
-spec initial_uri() -> {ok, gleam@uri:uri()} | {error, nil}.
initial_uri() ->
    {error, nil}.

-file("src/modem.gleam", 97).
-spec do_init(fun((gleam@uri:uri()) -> nil)) -> nil.
do_init(_) ->
    nil.

-file("src/modem.gleam", 86).
?DOC(
    " Initialise a simple modem that intercepts internal links and sends them to\n"
    " your update function through the provided handler.\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec init(fun((gleam@uri:uri()) -> UFB)) -> lustre@effect:effect(UFB).
init(Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_init(fun(Uri) -> _pipe = Uri,
                            _pipe@1 = Handler(_pipe),
                            Dispatch(_pipe@1) end) end
            )
        end
    ).

-file("src/modem.gleam", 120).
-spec do_advanced(fun((gleam@uri:uri()) -> nil), options()) -> nil.
do_advanced(_, _) ->
    nil.

-file("src/modem.gleam", 109).
?DOC(
    " Initialise an advanced modem that lets you configure what types of links to\n"
    " intercept. Take a look at the [`Options`](#options) type for info on what\n"
    " can be configured.\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec advanced(options(), fun((gleam@uri:uri()) -> UFD)) -> lustre@effect:effect(UFD).
advanced(Options, Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_advanced(fun(Uri) -> _pipe = Uri,
                            _pipe@1 = Handler(_pipe),
                            Dispatch(_pipe@1) end, Options) end
            )
        end
    ).

-file("src/modem.gleam", 146).
-spec do_push(gleam@uri:uri()) -> nil.
do_push(_) ->
    nil.

-file("src/modem.gleam", 169).
-spec do_replace(gleam@uri:uri()) -> nil.
do_replace(_) ->
    nil.

-file("src/modem.gleam", 191).
-spec do_load(gleam@uri:uri()) -> nil.
do_load(_) ->
    nil.

-file("src/modem.gleam", 183).
?DOC(
    " Load a new uri. This will always trigger a full page reload even if the uri\n"
    " is relative or the same as the current page.\n"
    "\n"
    " **Note**: if you load a new uri while the user has navigated using the back\n"
    " or forward buttons, you will clear any forward history in the stack!\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec load(gleam@uri:uri()) -> lustre@effect:effect(any()).
load(Uri) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_load(Uri) end
            )
        end
    ).

-file("src/modem.gleam", 215).
-spec do_forward(integer()) -> nil.
do_forward(_) ->
    nil.

-file("src/modem.gleam", 207).
?DOC(
    " The browser maintains a history stack of all the url's the user has visited.\n"
    " This function lets you move forward the given number of steps in that stack.\n"
    " If you reach the end of the stack, further attempts to go forward will do\n"
    " nothing (unfortunately time travel is not quite possible yet).\n"
    "\n"
    " **Note**: you can go _too far forward_ and end up navigating the user off your\n"
    " app if you're not careful.\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec forward(integer()) -> lustre@effect:effect(any()).
forward(Steps) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_forward(Steps) end
            )
        end
    ).

-file("src/modem.gleam", 242).
-spec do_back(integer()) -> nil.
do_back(_) ->
    nil.

-file("src/modem.gleam", 234).
?DOC(
    " The browser maintains a history stack of all the url's the user has visited.\n"
    " This function lets you move back the given number of steps in that stack.\n"
    " If you reach the beginning of the stack, further attempts to go back will do\n"
    " nothing (unfortunately time travel is not quite possible yet).\n"
    "\n"
    " **Note**: if you navigate back and then [`push`](#push) a new url, you will\n"
    " clear the forward history of the stack.\n"
    "\n"
    " **Note**: you can go _too far back_ and end up navigating the user off your\n"
    " app if you're not careful.\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec back(integer()) -> lustre@effect:effect(any()).
back(Steps) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_back(Steps) end
            )
        end
    ).

-file("src/modem.gleam", 269).
?DOC(
    " Simulate a click on a link in the browser that would trigger a navigation.\n"
    " This will dispatch a message to the simulated application if the link's `href`\n"
    " is valid and would cause an internal navigation.\n"
    "\n"
    " The base URL is necessary to resolve relative links. It should be a full\n"
    " complete URL, typically the one you would use for the live version of your app.\n"
    " For example:\n"
    "\n"
    " - `https://lustre.build`\n"
    "\n"
    " - `http://localhost:1234`\n"
    "\n"
    " - `https://gleam.run/news`\n"
    "\n"
    " Modem can simulate links that are relative to that base URL such as `./wibble`,\n"
    " absolute paths like `/wobble`, or full URLs **as long as their origin matches\n"
    " the base URL**.\n"
    "\n"
    " External links will log a problem in the simulation's history. Links with an\n"
    " empty `href` attribute will be ignored.\n"
).
-spec simulate(
    lustre@dev@simulate:simulation(UFT, UFU),
    lustre@dev@query:'query'(),
    binary(),
    fun((gleam@uri:uri()) -> UFU)
) -> lustre@dev@simulate:simulation(UFT, UFU).
simulate(Simulation, Query, Route, Handler) ->
    gleam@result:unwrap_both(
        begin
            gleam@result:'try'(
                gleam@result:replace_error(
                    gleam_stdlib:uri_parse(Route),
                    lustre@dev@simulate:problem(
                        Simulation,
                        <<"ModemInvalidBaseURL"/utf8>>,
                        <<<<"`"/utf8, Route/binary>>/binary,
                            "` is not a valid base URL"/utf8>>
                    )
                ),
                fun(Base) ->
                    gleam@result:'try'(
                        gleam@result:replace_error(
                            gleam@uri:origin(Base),
                            lustre@dev@simulate:problem(
                                Simulation,
                                <<"ModemInvalidBaseURL"/utf8>>,
                                <<<<"`"/utf8, Route/binary>>/binary,
                                    "` is not a valid base URL"/utf8>>
                            )
                        ),
                        fun(Origin) ->
                            gleam@result:'try'(
                                gleam@result:replace_error(
                                    lustre@dev@query:find(
                                        lustre@dev@simulate:view(Simulation),
                                        Query
                                    ),
                                    lustre@dev@simulate:problem(
                                        Simulation,
                                        <<"EventTargetNotFound"/utf8>>,
                                        <<"No element matching "/utf8,
                                            (lustre@dev@query:to_readable_string(
                                                Query
                                            ))/binary>>
                                    )
                                ),
                                fun(Target) -> gleam@result:'try'(case Target of
                                            {element,
                                                _,
                                                _,
                                                _,
                                                _,
                                                <<"a"/utf8>>,
                                                Attributes,
                                                _,
                                                _,
                                                _,
                                                _} ->
                                                {ok, Attributes};

                                            _ ->
                                                {error,
                                                    lustre@dev@simulate:problem(
                                                        Simulation,
                                                        <<"ModemInvalidTarget"/utf8>>,
                                                        <<"Target must be an <a> tag"/utf8>>
                                                    )}
                                        end, fun(Attributes@1) ->
                                            gleam@result:'try'(
                                                gleam@result:replace_error(
                                                    gleam@list:find_map(
                                                        Attributes@1,
                                                        fun(Attribute) ->
                                                            case Attribute of
                                                                {attribute,
                                                                    _,
                                                                    <<"href"/utf8>>,
                                                                    Value} ->
                                                                    {ok, Value};

                                                                _ ->
                                                                    {error, nil}
                                                            end
                                                        end
                                                    ),
                                                    lustre@dev@simulate:problem(
                                                        Simulation,
                                                        <<"ModemMissingHref"/utf8>>,
                                                        <<"Target must have an `href` attribute"/utf8>>
                                                    )
                                                ),
                                                fun(Href) ->
                                                    gleam@result:'try'(
                                                        gleam@result:replace_error(
                                                            gleam_stdlib:uri_parse(
                                                                Href
                                                            ),
                                                            lustre@dev@simulate:problem(
                                                                Simulation,
                                                                <<"ModemInvalidHref"/utf8>>,
                                                                <<<<"`"/utf8,
                                                                        Href/binary>>/binary,
                                                                    "` is not a valid URL"/utf8>>
                                                            )
                                                        ),
                                                        fun(Relative) ->
                                                            gleam@result:'try'(
                                                                case gleam@uri:origin(
                                                                    Relative
                                                                ) of
                                                                    {ok,
                                                                        Relative_origin} when Origin =/= Relative_origin ->
                                                                        {error,
                                                                            lustre@dev@simulate:problem(
                                                                                Simulation,
                                                                                <<"ModemExternalUrl"/utf8>>,
                                                                                <<<<"`"/utf8,
                                                                                        Href/binary>>/binary,
                                                                                    "` is an external URL and cannot be simulated"/utf8>>
                                                                            )};

                                                                    _ ->
                                                                        {ok,
                                                                            nil}
                                                                end,
                                                                fun(_) ->
                                                                    gleam@result:'try'(
                                                                        gleam@result:replace_error(
                                                                            gleam@uri:merge(
                                                                                Base,
                                                                                Relative
                                                                            ),
                                                                            lustre@dev@simulate:problem(
                                                                                Simulation,
                                                                                <<"ModemInvalidBaseURL"/utf8>>,
                                                                                <<<<"`"/utf8,
                                                                                        Route/binary>>/binary,
                                                                                    "` is not a valid base URL"/utf8>>
                                                                            )
                                                                        ),
                                                                        fun(
                                                                            Resolved
                                                                        ) ->
                                                                            {ok,
                                                                                lustre@dev@simulate:message(
                                                                                    Simulation,
                                                                                    Handler(
                                                                                        Resolved
                                                                                    )
                                                                                )}
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end) end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/modem.gleam", 134).
?DOC(
    " Push a new relative route onto the browser's history stack. This will not\n"
    " trigger a full page reload.\n"
    "\n"
    " **Note**: if you push a new uri while the user has navigated using the back\n"
    " or forward buttons, you will clear any forward history in the stack!\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec push(
    binary(),
    gleam@option:option(binary()),
    gleam@option:option(binary())
) -> lustre@effect:effect(any()).
push(Path, Query, Fragment) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() ->
                    do_push(
                        begin
                            _record = {uri,
                                none,
                                none,
                                none,
                                none,
                                <<""/utf8>>,
                                none,
                                none},
                            {uri,
                                erlang:element(2, _record),
                                erlang:element(3, _record),
                                erlang:element(4, _record),
                                erlang:element(5, _record),
                                Path,
                                Query,
                                Fragment}
                        end
                    )
                end
            )
        end
    ).

-file("src/modem.gleam", 157).
?DOC(
    " Replace the current uri in the browser's history stack with a new relative\n"
    " route. This will not trigger a full page reload.\n"
    "\n"
    " > **Note**: this effect is only meaningful in the browser. When executed in\n"
    " > a backend JavaScript environment or in Erlang this effect will always be\n"
    " > equivalent to `effect.none()`\n"
).
-spec replace(
    binary(),
    gleam@option:option(binary()),
    gleam@option:option(binary())
) -> lustre@effect:effect(any()).
replace(Path, Query, Fragment) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() ->
                    do_replace(
                        begin
                            _record = {uri,
                                none,
                                none,
                                none,
                                none,
                                <<""/utf8>>,
                                none,
                                none},
                            {uri,
                                erlang:element(2, _record),
                                erlang:element(3, _record),
                                erlang:element(4, _record),
                                erlang:element(5, _record),
                                Path,
                                Query,
                                Fragment}
                        end
                    )
                end
            )
        end
    ).
