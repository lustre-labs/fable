-module(wisp@testing).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([string_body/1, bit_array_body/1, request/4, get/2, post/3, post_form/3, post_json/3, head/2, put/3, put_form/3, put_json/3, delete/3, delete_form/3, delete_json/3, trace/2, connect/2, options/2, patch/3, patch_form/3, patch_json/3, set_cookie/4]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/wisp/testing.gleam", 227).
?DOC(
    " Read the body of a response as a string.\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic if the response body is a file and the file cannot\n"
    " be read, or if it does not contain valid UTF-8.\n"
).
-spec string_body(gleam@http@response:response(wisp:body())) -> binary().
string_body(Response) ->
    case erlang:element(4, Response) of
        empty ->
            <<""/utf8>>;

        {text, Tree} ->
            unicode:characters_to_binary(Tree);

        {bytes, Bytes} ->
            Data = erlang:list_to_bitstring(Bytes),
            String@1 = case gleam@bit_array:to_string(Data) of
                {ok, String} -> String;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"wisp/testing"/utf8>>,
                                function => <<"string_body"/utf8>>,
                                line => 233})
            end,
            String@1;

        {file, Path} ->
            Contents@1 = case simplifile:read(Path) of
                {ok, Contents} -> Contents;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"wisp/testing"/utf8>>,
                                function => <<"string_body"/utf8>>,
                                line => 237})
            end,
            Contents@1
    end.

-file("src/wisp/testing.gleam", 250).
?DOC(
    " Read the body of a response as a bit string\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic if the response body is a file and the file cannot\n"
    " be read.\n"
).
-spec bit_array_body(gleam@http@response:response(wisp:body())) -> bitstring().
bit_array_body(Response) ->
    case erlang:element(4, Response) of
        empty ->
            <<>>;

        {bytes, Tree} ->
            erlang:list_to_bitstring(Tree);

        {text, Tree@1} ->
            erlang:list_to_bitstring(gleam_stdlib:wrap_list(Tree@1));

        {file, Path} ->
            Contents@1 = case simplifile_erl:read_bits(Path) of
                {ok, Contents} -> Contents;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"wisp/testing"/utf8>>,
                                function => <<"bit_array_body"/utf8>>,
                                line => 256})
            end,
            Contents@1
    end.

-file("src/wisp/testing.gleam", 29).
?DOC(
    " Create a test HTTP request that can be used to test your request handler\n"
    " functions.\n"
    "\n"
    " Note not all HTTP methods are expected to have an accompanying body, so when\n"
    " using this function directly over other functions such as `get` and `post`\n"
    " take care to ensure you are not providing a body when it is not expected.\n"
    " \n"
    " The `default_secret_key_base` constant is used as the secret key base for\n"
    " requests made with this function.\n"
).
-spec request(
    gleam@http:method(),
    binary(),
    list({binary(), binary()}),
    bitstring()
) -> gleam@http@request:request(wisp@internal:connection()).
request(Method, Path, Headers, Body) ->
    {Path@2, Query@1} = case gleam@string:split(Path, <<"?"/utf8>>) of
        [Path@1, Query] ->
            {Path@1, {some, Query}};

        _ ->
            {Path, none}
    end,
    _pipe = {request,
        Method,
        Headers,
        Body,
        https,
        <<"localhost"/utf8>>,
        none,
        Path@2,
        Query@1},
    gleam@http@request:set_body(
        _pipe,
        wisp:create_canned_connection(
            Body,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        )
    ).

-file("src/wisp/testing.gleam", 57).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec get(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
get(Path, Headers) ->
    request(get, Path, Headers, <<>>).

-file("src/wisp/testing.gleam", 63).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec post(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
post(Path, Headers, Body) ->
    request(post, Path, Headers, <<Body/binary>>).

-file("src/wisp/testing.gleam", 72).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The body parameters are encoded as form data and the `content-type` header\n"
    " is set to `application/x-www-form-urlencoded`.\n"
).
-spec post_form(
    binary(),
    list({binary(), binary()}),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
post_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(post, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-file("src/wisp/testing.gleam", 86).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The `content-type` header is set to `application/json`.\n"
).
-spec post_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
post_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(post, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-file("src/wisp/testing.gleam", 98).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec head(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
head(Path, Headers) ->
    request(head, Path, Headers, <<>>).

-file("src/wisp/testing.gleam", 104).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec put(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
put(Path, Headers, Body) ->
    request(put, Path, Headers, <<Body/binary>>).

-file("src/wisp/testing.gleam", 113).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The body parameters are encoded as form data and the `content-type` header\n"
    " is set to `application/x-www-form-urlencoded`.\n"
).
-spec put_form(binary(), list({binary(), binary()}), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
put_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(put, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-file("src/wisp/testing.gleam", 127).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The `content-type` header is set to `application/json`.\n"
).
-spec put_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
put_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(put, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-file("src/wisp/testing.gleam", 135).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec delete(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
delete(Path, Headers, Body) ->
    request(delete, Path, Headers, <<Body/binary>>).

-file("src/wisp/testing.gleam", 144).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The body parameters are encoded as form data and the `content-type` header\n"
    " is set to `application/x-www-form-urlencoded`.\n"
).
-spec delete_form(
    binary(),
    list({binary(), binary()}),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
delete_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(delete, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-file("src/wisp/testing.gleam", 158).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The `content-type` header is set to `application/json`.\n"
).
-spec delete_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
delete_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(delete, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-file("src/wisp/testing.gleam", 170).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec trace(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
trace(Path, Headers) ->
    request(trace, Path, Headers, <<>>).

-file("src/wisp/testing.gleam", 176).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec connect(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
connect(Path, Headers) ->
    request(connect, Path, Headers, <<>>).

-file("src/wisp/testing.gleam", 182).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec options(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
options(Path, Headers) ->
    request(options, Path, Headers, <<>>).

-file("src/wisp/testing.gleam", 188).
?DOC(" Create a test HTTP request that can be used to test your request handler.\n").
-spec patch(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
patch(Path, Headers, Body) ->
    request(patch, Path, Headers, <<Body/binary>>).

-file("src/wisp/testing.gleam", 196).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The body parameters are encoded as form data and the `content-type` header is set to `application/x-www-form-urlencoded`.\n"
).
-spec patch_form(
    binary(),
    list({binary(), binary()}),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
patch_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(patch, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-file("src/wisp/testing.gleam", 210).
?DOC(
    " Create a test HTTP request that can be used to test your request handler.\n"
    " \n"
    " The `content-type` header is set to `application/json`.\n"
).
-spec patch_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
patch_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(patch, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-file("src/wisp/testing.gleam", 277).
?DOC(" Set a cookie on the request.\n").
-spec set_cookie(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    wisp:security()
) -> gleam@http@request:request(wisp@internal:connection()).
set_cookie(Req, Name, Value, Security) ->
    Value@1 = case Security of
        plain_text ->
            gleam_stdlib:bit_array_base64_encode(<<Value/binary>>, false);

        signed ->
            wisp:sign_message(Req, <<Value/binary>>, sha512)
    end,
    gleam@http@request:set_cookie(Req, Name, Value@1).
