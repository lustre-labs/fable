-module(rsvp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([expect_ok_response/1, expect_text/1, expect_any_response/1, send/2, simulate/3, expect_json/2, parse_relative_uri/1, get/2, post/3]).
-export_type([error/0, handler/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type error() :: bad_body |
    {bad_url, binary()} |
    {http_error, gleam@http@response:response(binary())} |
    {json_error, gleam@json:decode_error()} |
    network_error |
    {unhandled_response, gleam@http@response:response(binary())}.

-opaque handler(PWZ) :: {handler,
        fun(({ok, gleam@http@response:response(binary())} | {error, error()}) -> PWZ)}.

-file("src/rsvp.gleam", 185).
?DOC(
    " Handle any response with a `2xx` status code. This handler will return an\n"
    " `Error` if the response status code is not in the `2xx` range. The specific\n"
    " error will depend on the status code:\n"
    "\n"
    " - `4xx` and `5xx` status codes will return `HttpError`\n"
    "\n"
    " - Other non `2xx` status codes will return `UnhandledResponse`\n"
    "\n"
    " **Note**: if you need to handle HTTP responses with different status codes,\n"
    " you should use the more-general [`expect_any_response`](#expect_any_response)\n"
    " handler.\n"
).
-spec expect_ok_response(
    fun(({ok, gleam@http@response:response(binary())} | {error, error()}) -> PXX)
) -> handler(PXX).
expect_ok_response(Handler) ->
    {handler,
        fun(Result) ->
            Handler(
                begin
                    gleam@result:'try'(
                        Result,
                        fun(Response) -> case erlang:element(2, Response) of
                                Code when (Code >= 200) andalso (Code < 300) ->
                                    {ok, Response};

                                Code@1 when (Code@1 >= 400) andalso (Code@1 < 600) ->
                                    {error, {http_error, Response}};

                                _ ->
                                    {error, {unhandled_response, Response}}
                            end end
                    )
                end
            )
        end}.

-file("src/rsvp.gleam", 113).
-spec expect_json_response(
    fun(({ok, gleam@http@response:response(binary())} | {error, error()}) -> PXJ)
) -> handler(PXJ).
expect_json_response(Handler) ->
    expect_ok_response(
        fun(Result) ->
            Handler(
                begin
                    gleam@result:'try'(
                        Result,
                        fun(Response) ->
                            case gleam@http@response:get_header(
                                Response,
                                <<"content-type"/utf8>>
                            ) of
                                {ok, <<"application/json"/utf8>>} ->
                                    {ok, Response};

                                {ok, <<"application/json;"/utf8, _/binary>>} ->
                                    {ok, Response};

                                _ ->
                                    {error, {unhandled_response, Response}}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/rsvp.gleam", 158).
-spec expect_text_response(
    fun(({ok, gleam@http@response:response(binary())} | {error, error()}) -> PXS)
) -> handler(PXS).
expect_text_response(Handler) ->
    expect_ok_response(
        fun(Result) ->
            Handler(
                begin
                    gleam@result:'try'(
                        Result,
                        fun(Response) ->
                            case gleam@http@response:get_header(
                                Response,
                                <<"content-type"/utf8>>
                            ) of
                                {ok, <<"text/"/utf8, _/binary>>} ->
                                    {ok, Response};

                                _ ->
                                    {error, {unhandled_response, Response}}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/rsvp.gleam", 150).
?DOC(
    " Handle the body of a plain text response. This handler will check the\n"
    " following conditions:\n"
    "\n"
    " - The response status code is `2xx`.\n"
    "\n"
    " - The response content-type specifies `\"text/\"` such as `\"text/plain\"` or\n"
    "   `\"text/html\"`.\n"
    "\n"
    " If any of these conditions are not met, an `Error` will be returned instead.\n"
    " The specific error will depend on which condition failed:\n"
    "\n"
    " - `4xx` and `5xx` status codes will return `HttpError`\n"
    "\n"
    " - Other non `2xx` status codes will return `UnhandledResponse`\n"
    "\n"
    " - A missing or incorrect `content-type` header will return `UnhandledResponse`\n"
    "\n"
    " **Note**: if you need more advanced handling of the request body directly, you\n"
    " should use the more-general [`expect_ok_response`](#expect_ok_response) or\n"
    " [`expect_any_response`](#expect_any_response) handlers.\n"
).
-spec expect_text(fun(({ok, binary()} | {error, error()}) -> PXN)) -> handler(PXN).
expect_text(Handler) ->
    expect_text_response(fun(Result) -> _pipe = Result,
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(Response) -> erlang:element(4, Response) end
            ),
            Handler(_pipe@1) end).

-file("src/rsvp.gleam", 213).
?DOC(
    " Handle any HTTP response, regardless of status code. Your custom handler will\n"
    " still have to handle potential errors such as network errors or malformed\n"
    " responses.\n"
    "\n"
    " It is uncommon to need a handler this low-level, instead you can consider the\n"
    " following more-specific handlers:\n"
    "\n"
    " - [`expect_ok_response`](#expect_ok_response) to handle any response with a\n"
    "   `2xx` status code.\n"
    "\n"
    " - [`expect_json`](#expect_json) to handle responses from JSON apis\n"
).
-spec expect_any_response(
    fun(({ok, gleam@http@response:response(binary())} | {error, error()}) -> PYC)
) -> handler(PYC).
expect_any_response(Handler) ->
    {handler, Handler}.

-file("src/rsvp.gleam", 300).
-spec do_send(gleam@http@request:request(binary()), handler(PYP)) -> lustre@effect:effect(PYP).
do_send(Request, Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@erlang@process:start(
                fun() -> _pipe = gleam@httpc:send(Request),
                    _pipe@1 = gleam@result:map_error(
                        _pipe,
                        fun(Error) -> case Error of
                                invalid_utf8_response ->
                                    bad_body;

                                {failed_to_connect, _, _} ->
                                    network_error
                            end end
                    ),
                    _pipe@2 = (erlang:element(2, Handler))(_pipe@1),
                    Dispatch(_pipe@2) end,
                true
            ),
            nil
        end
    ).

-file("src/rsvp.gleam", 295).
?DOC(
    " Send a [`Request`](https://hexdocs.pm/gleam_http/gleam/http/request.html#Request)\n"
    " and dispatch a message back to your `update` function when the response is\n"
    " handled.\n"
    "\n"
    " For simple requests, you can use the more-convenient [`get`](#get) and\n"
    " [`post`](#post) functions instead.\n"
    "\n"
    " **Note**: On the **JavaScript** target this will use the `fetch` API. Make\n"
    " sure you have a polyfill for it if you need to support older browsers or\n"
    " server-side runtimes that don't have it.\n"
    "\n"
    " **Note**: On the **Erlang** target this will use the `httpc` module. Each\n"
    " request will start a new linked process to make and handle the request.\n"
).
-spec send(gleam@http@request:request(binary()), handler(PYL)) -> lustre@effect:effect(PYL).
send(Request, Handler) ->
    do_send(Request, Handler).

-file("src/rsvp.gleam", 347).
?DOC(
    " Simulate a response in a simulated application. This runs the provided handler\n"
    " against the response and dispatches the message to your simulated application.\n"
).
-spec simulate(
    lustre@dev@simulate:simulation(PYS, PYT),
    gleam@http@response:response(binary()),
    handler(PYT)
) -> lustre@dev@simulate:simulation(PYS, PYT).
simulate(Simulation, Response, Handler) ->
    lustre@dev@simulate:message(
        Simulation,
        (erlang:element(2, Handler))({ok, Response})
    ).

-file("src/rsvp.gleam", 357).
-spec reject(error(), handler(PZA)) -> lustre@effect:effect(PZA).
reject(Err, Handler) ->
    lustre@effect:from(fun(Dispatch) -> _pipe = {error, Err},
            _pipe@1 = (erlang:element(2, Handler))(_pipe),
            Dispatch(_pipe@1) end).

-file("src/rsvp.gleam", 365).
-spec decode_json_body(
    gleam@http@response:response(binary()),
    gleam@dynamic@decode:decoder(PZE)
) -> {ok, PZE} | {error, error()}.
decode_json_body(Response, Decoder) ->
    _pipe = erlang:element(4, Response),
    _pipe@1 = gleam@json:parse(_pipe, Decoder),
    gleam@result:map_error(_pipe@1, fun(Field@0) -> {json_error, Field@0} end).

-file("src/rsvp.gleam", 102).
?DOC(
    " A handler that runs a JSON decoder on a response body and returns the result\n"
    " as a message. This handler will check the following conditions:\n"
    "\n"
    " - The response status code is `2xx`.\n"
    "\n"
    " - The response content-type is `\"application/json\"`\n"
    "\n"
    " - The response body can be decoded using the provided JSON decoder\n"
    "\n"
    " If any of these conditions are not met, an `Error` will be returned instead.\n"
    " The specific error will depend on which condition failed:\n"
    "\n"
    " - `4xx` and `5xx` status codes will return `HttpError`\n"
    "\n"
    " - Other non `2xx` status codes will return `UnhandledResponse`\n"
    "\n"
    " - A missing or incorrect `content-type` header will return `UnhandledResponse`\n"
    "\n"
    " - A JSON decoding error will return `JsonError`\n"
    "\n"
    " **Note**: if you need more advanced handling of the request body directly, you\n"
    " should use the more-general [`expect_ok_response`](#expect_ok_response) or\n"
    " [`expect_any_response`](#expect_any_response) handlers.\n"
).
-spec expect_json(
    gleam@dynamic@decode:decoder(PXA),
    fun(({ok, PXA} | {error, error()}) -> PXE)
) -> handler(PXE).
expect_json(Decoder, Handler) ->
    expect_json_response(fun(Result) -> _pipe = Result,
            _pipe@1 = gleam@result:then(
                _pipe,
                fun(_capture) -> decode_json_body(_capture, Decoder) end
            ),
            Handler(_pipe@1) end).

-file("src/rsvp.gleam", 390).
?DOC(
    " The standard library [`uri.parse`](https://hexdocs.pm/gleam_stdlib/0.45.0/gleam/uri.html#parse)\n"
    " function does not support relative URIs. When running in the browser, however,\n"
    " we have enough information to resolve relative URIs into complete ones!\n"
    "\n"
    " This function will always fail when running on the server, but in the browser\n"
    " it will resolve relative URIs based on the current page's URL\n"
).
-spec parse_relative_uri(binary()) -> {ok, gleam@uri:uri()} | {error, nil}.
parse_relative_uri(_) ->
    {error, nil}.

-file("src/rsvp.gleam", 374).
-spec to_uri(binary()) -> {ok, gleam@uri:uri()} | {error, error()}.
to_uri(Uri_string) ->
    _pipe = case Uri_string of
        <<"./"/utf8, _/binary>> ->
            parse_relative_uri(Uri_string);

        <<"/"/utf8, _/binary>> ->
            parse_relative_uri(Uri_string);

        _ ->
            gleam_stdlib:uri_parse(Uri_string)
    end,
    gleam@result:replace_error(_pipe, {bad_url, Uri_string}).

-file("src/rsvp.gleam", 235).
?DOC(
    " A convenience function to send a `GET` request to a URL and handle the response\n"
    " using a [`Hander`](#Handler).\n"
    "\n"
    " **Note**: if you need more control over the kind of request being sent, for\n"
    " example to set additional headers or use a different HTTP method, you should\n"
    " use the more-general [`send`](#send) function insteaed.\n"
    "\n"
    " **Note**: On the **JavaScript** target this will use the `fetch` API. Make\n"
    " sure you have a polyfill for it if you need to support older browsers or\n"
    " server-side runtimes that don't have it.\n"
    "\n"
    " **Note**: On the **Erlang** target this will use the `httpc` module. Each\n"
    " request will start a new linked process to make and handle the request.\n"
).
-spec get(binary(), handler(PYE)) -> lustre@effect:effect(PYE).
get(Url, Handler) ->
    case to_uri(Url) of
        {ok, Uri} ->
            _pipe = gleam@http@request:from_uri(Uri),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_capture) -> send(_capture, Handler) end
            ),
            _pipe@2 = gleam@result:map_error(
                _pipe@1,
                fun(_) -> reject({bad_url, Url}, Handler) end
            ),
            gleam@result:unwrap_both(_pipe@2);

        {error, Err} ->
            reject(Err, Handler)
    end.

-file("src/rsvp.gleam", 263).
?DOC(
    " A convenience function for sending a POST request with a JSON body and handle\n"
    " the response with a handler function. This will automatically set the\n"
    " `content-type` header to `application/json` and handle requests to relative\n"
    " URLs if this effect is running in a browser.\n"
    "\n"
    " **Note**: if you need more control over the kind of request being sent, for\n"
    " example to set additional headers or use a different HTTP method, you should\n"
    " use the more-general [`send`](#send) function insteaed.\n"
    "\n"
    " **Note**: On the **JavaScript** target this will use the `fetch` API. Make\n"
    " sure you have a polyfill for it if you need to support older browsers or\n"
    " server-side runtimes that don't have it.\n"
    "\n"
    " **Note**: On the **Erlang** target this will use the `httpc` module. Each\n"
    " request will start a new linked process to make and handle the request.\n"
).
-spec post(binary(), gleam@json:json(), handler(PYH)) -> lustre@effect:effect(PYH).
post(Url, Body, Handler) ->
    case to_uri(Url) of
        {ok, Uri} ->
            _pipe = gleam@http@request:from_uri(Uri),
            _pipe@5 = gleam@result:map(_pipe, fun(Request) -> _pipe@1 = Request,
                    _pipe@2 = gleam@http@request:set_method(_pipe@1, post),
                    _pipe@3 = gleam@http@request:set_header(
                        _pipe@2,
                        <<"content-type"/utf8>>,
                        <<"application/json"/utf8>>
                    ),
                    _pipe@4 = gleam@http@request:set_body(
                        _pipe@3,
                        gleam@json:to_string(Body)
                    ),
                    send(_pipe@4, Handler) end),
            _pipe@6 = gleam@result:map_error(
                _pipe@5,
                fun(_) -> reject({bad_url, Url}, Handler) end
            ),
            gleam@result:unwrap_both(_pipe@6);

        {error, Err} ->
            reject(Err, Handler)
    end.
