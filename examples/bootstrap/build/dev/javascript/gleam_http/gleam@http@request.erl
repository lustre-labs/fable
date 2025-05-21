-module(gleam@http@request).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_uri/1, from_uri/1, get_header/2, set_header/3, prepend_header/3, set_body/2, map/2, path_segments/1, get_query/1, set_query/2, set_method/2, new/0, to/1, set_scheme/2, set_host/2, set_port/2, set_path/2, set_cookie/3, get_cookies/1, remove_cookie/2]).
-export_type([request/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type request(GMZ) :: {request,
        gleam@http:method(),
        list({binary(), binary()}),
        GMZ,
        gleam@http:scheme(),
        binary(),
        gleam@option:option(integer()),
        binary(),
        gleam@option:option(binary())}.

-file("src/gleam/http/request.gleam", 30).
?DOC(" Return the uri that a request was sent to.\n").
-spec to_uri(request(any())) -> gleam@uri:uri().
to_uri(Request) ->
    {uri,
        {some, gleam@http:scheme_to_string(erlang:element(5, Request))},
        none,
        {some, erlang:element(6, Request)},
        erlang:element(7, Request),
        erlang:element(8, Request),
        erlang:element(9, Request),
        none}.

-file("src/gleam/http/request.gleam", 44).
?DOC(" Construct a request from a URI.\n").
-spec from_uri(gleam@uri:uri()) -> {ok, request(binary())} | {error, nil}.
from_uri(Uri) ->
    gleam@result:then(
        begin
            _pipe = erlang:element(2, Uri),
            _pipe@1 = gleam@option:unwrap(_pipe, <<""/utf8>>),
            gleam@http:scheme_from_string(_pipe@1)
        end,
        fun(Scheme) ->
            gleam@result:then(
                begin
                    _pipe@2 = erlang:element(4, Uri),
                    gleam@option:to_result(_pipe@2, nil)
                end,
                fun(Host) ->
                    Req = {request,
                        get,
                        [],
                        <<""/utf8>>,
                        Scheme,
                        Host,
                        erlang:element(5, Uri),
                        erlang:element(6, Uri),
                        erlang:element(7, Uri)},
                    {ok, Req}
                end
            )
        end
    ).

-file("src/gleam/http/request.gleam", 75).
?DOC(
    " Get the value for a given header.\n"
    "\n"
    " If the request does not have that header then `Error(Nil)` is returned.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec get_header(request(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Request, Key) ->
    gleam@list:key_find(erlang:element(3, Request), string:lowercase(Key)).

-file("src/gleam/http/request.gleam", 86).
?DOC(
    " Set the header with the given value under the given header key.\n"
    "\n"
    " If already present, it is replaced.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec set_header(request(GNJ), binary(), binary()) -> request(GNJ).
set_header(Request, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Request),
        string:lowercase(Key),
        Value
    ),
    _record = Request,
    {request,
        erlang:element(2, _record),
        Headers,
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 103).
?DOC(
    " Prepend the header with the given value under the given header key.\n"
    "\n"
    " Similar to `set_header` except if the header already exists it prepends\n"
    " another header with the same key.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec prepend_header(request(GNM), binary(), binary()) -> request(GNM).
prepend_header(Request, Key, Value) ->
    Headers = [{string:lowercase(Key), Value} | erlang:element(3, Request)],
    _record = Request,
    {request,
        erlang:element(2, _record),
        Headers,
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 115).
?DOC(" Set the body of the request, overwriting any existing body.\n").
-spec set_body(request(any()), GNR) -> request(GNR).
set_body(Req, Body) ->
    {request, Method, Headers, _, Scheme, Host, Port, Path, Query} = Req,
    {request, Method, Headers, Body, Scheme, Host, Port, Path, Query}.

-file("src/gleam/http/request.gleam", 140).
?DOC(" Update the body of a request using a given function.\n").
-spec map(request(GNT), fun((GNT) -> GNV)) -> request(GNV).
map(Request, Transform) ->
    _pipe = erlang:element(4, Request),
    _pipe@1 = Transform(_pipe),
    set_body(Request, _pipe@1).

-file("src/gleam/http/request.gleam", 160).
?DOC(
    " Return the non-empty segments of a request path.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> set_path(\"/one/two/three\")\n"
    " > |> path_segments\n"
    " [\"one\", \"two\", \"three\"]\n"
    " ```\n"
).
-spec path_segments(request(any())) -> list(binary()).
path_segments(Request) ->
    _pipe = erlang:element(8, Request),
    gleam@uri:path_segments(_pipe).

-file("src/gleam/http/request.gleam", 166).
?DOC(" Decode the query of a request.\n").
-spec get_query(request(any())) -> {ok, list({binary(), binary()})} |
    {error, nil}.
get_query(Request) ->
    case erlang:element(9, Request) of
        {some, Query_string} ->
            gleam_stdlib:parse_query(Query_string);

        none ->
            {ok, []}
    end.

-file("src/gleam/http/request.gleam", 176).
?DOC(
    " Set the query of the request.\n"
    " Query params will be percent encoded before being added to the Request.\n"
).
-spec set_query(request(GOF), list({binary(), binary()})) -> request(GOF).
set_query(Req, Query) ->
    Pair = fun(T) ->
        <<<<(gleam_stdlib:percent_encode(erlang:element(1, T)))/binary,
                "="/utf8>>/binary,
            (gleam_stdlib:percent_encode(erlang:element(2, T)))/binary>>
    end,
    Query@1 = begin
        _pipe = Query,
        _pipe@1 = gleam@list:map(_pipe, Pair),
        _pipe@2 = gleam@list:intersperse(_pipe@1, <<"&"/utf8>>),
        _pipe@3 = gleam@string:concat(_pipe@2),
        {some, _pipe@3}
    end,
    _record = Req,
    {request,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        Query@1}.

-file("src/gleam/http/request.gleam", 194).
?DOC(" Set the method of the request.\n").
-spec set_method(request(GOJ), gleam@http:method()) -> request(GOJ).
set_method(Req, Method) ->
    _record = Req,
    {request,
        Method,
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 201).
?DOC(
    " A request with commonly used default values. This request can be used as\n"
    " an initial value and then update to create the desired request.\n"
).
-spec new() -> request(binary()).
new() ->
    {request,
        get,
        [],
        <<""/utf8>>,
        https,
        <<"localhost"/utf8>>,
        none,
        <<""/utf8>>,
        none}.

-file("src/gleam/http/request.gleam", 216).
?DOC(" Construct a request from a URL string\n").
-spec to(binary()) -> {ok, request(binary())} | {error, nil}.
to(Url) ->
    _pipe = Url,
    _pipe@1 = gleam_stdlib:uri_parse(_pipe),
    gleam@result:then(_pipe@1, fun from_uri/1).

-file("src/gleam/http/request.gleam", 224).
?DOC(" Set the scheme (protocol) of the request.\n").
-spec set_scheme(request(GOQ), gleam@http:scheme()) -> request(GOQ).
set_scheme(Req, Scheme) ->
    _record = Req,
    {request,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Scheme,
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 230).
?DOC(" Set the host of the request.\n").
-spec set_host(request(GOT), binary()) -> request(GOT).
set_host(Req, Host) ->
    _record = Req,
    {request,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        Host,
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 236).
?DOC(" Set the port of the request.\n").
-spec set_port(request(GOW), integer()) -> request(GOW).
set_port(Req, Port) ->
    _record = Req,
    {request,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        {some, Port},
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 242).
?DOC(" Set the path of the request.\n").
-spec set_path(request(GOZ), binary()) -> request(GOZ).
set_path(Req, Path) ->
    _record = Req,
    {request,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        Path,
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 249).
?DOC(
    " Send a cookie with a request\n"
    "\n"
    " Multiple cookies are added to the same cookie header.\n"
).
-spec set_cookie(request(GPC), binary(), binary()) -> request(GPC).
set_cookie(Req, Name, Value) ->
    New_cookie_string = gleam@string:join([Name, Value], <<"="/utf8>>),
    {Cookies_string@2, Headers@1} = case gleam@list:key_pop(
        erlang:element(3, Req),
        <<"cookie"/utf8>>
    ) of
        {ok, {Cookies_string, Headers}} ->
            Cookies_string@1 = gleam@string:join(
                [Cookies_string, New_cookie_string],
                <<"; "/utf8>>
            ),
            {Cookies_string@1, Headers};

        {error, nil} ->
            {New_cookie_string, erlang:element(3, Req)}
    end,
    _record = Req,
    {request,
        erlang:element(2, _record),
        [{<<"cookie"/utf8>>, Cookies_string@2} | Headers@1],
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.

-file("src/gleam/http/request.gleam", 268).
?DOC(
    " Fetch the cookies sent in a request.\n"
    "\n"
    " Note badly formed cookie pairs will be ignored.\n"
    " RFC6265 specifies that invalid cookie names/attributes should be ignored.\n"
).
-spec get_cookies(request(any())) -> list({binary(), binary()}).
get_cookies(Req) ->
    {request, _, Headers, _, _, _, _, _, _} = Req,
    _pipe = Headers,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Header) ->
            {Name, Value} = Header,
            case Name of
                <<"cookie"/utf8>> ->
                    {ok, gleam@http@cookie:parse(Value)};

                _ ->
                    {error, nil}
            end
        end
    ),
    gleam@list:flatten(_pipe@1).

-file("src/gleam/http/request.gleam", 286).
?DOC(
    " Remove a cookie from a request\n"
    "\n"
    " Remove a cookie from the request. If no cookie is found return the request unchanged.\n"
    " This will not remove the cookie from the client.\n"
).
-spec remove_cookie(request(GPH), binary()) -> request(GPH).
remove_cookie(Req, Name) ->
    case gleam@list:key_pop(erlang:element(3, Req), <<"cookie"/utf8>>) of
        {ok, {Cookies_string, Headers}} ->
            New_cookies_string = begin
                _pipe = gleam@string:split(Cookies_string, <<";"/utf8>>),
                _pipe@4 = gleam@list:filter(
                    _pipe,
                    fun(Str) -> _pipe@1 = gleam@string:trim(Str),
                        _pipe@2 = gleam@string:split_once(_pipe@1, <<"="/utf8>>),
                        _pipe@3 = gleam@result:map(
                            _pipe@2,
                            fun(Tup) -> erlang:element(1, Tup) /= Name end
                        ),
                        gleam@result:unwrap(_pipe@3, true) end
                ),
                gleam@string:join(_pipe@4, <<";"/utf8>>)
            end,
            _record = Req,
            {request,
                erlang:element(2, _record),
                [{<<"cookie"/utf8>>, New_cookies_string} | Headers],
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record)};

        {error, _} ->
            Req
    end.
