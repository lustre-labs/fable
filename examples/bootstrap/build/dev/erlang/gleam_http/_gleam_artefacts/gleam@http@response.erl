-module(gleam@http@response).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, get_header/2, set_header/3, prepend_header/3, set_body/2, try_map/2, map/2, redirect/1, get_cookies/1, set_cookie/4, expire_cookie/3]).
-export_type([response/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type response(GUW) :: {response, integer(), list({binary(), binary()}), GUW}.

-file("src/gleam/http/response.gleam", 40).
?DOC(
    " Construct an empty Response.\n"
    "\n"
    " The body type of the returned response is `String` and could be set with a\n"
    " call to `set_body`.\n"
).
-spec new(integer()) -> response(binary()).
new(Status) ->
    {response, Status, [], <<""/utf8>>}.

-file("src/gleam/http/response.gleam", 48).
?DOC(
    " Get the value for a given header.\n"
    "\n"
    " If the response does not have that header then `Error(Nil)` is returned.\n"
).
-spec get_header(response(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Response, Key) ->
    gleam@list:key_find(erlang:element(3, Response), string:lowercase(Key)).

-file("src/gleam/http/response.gleam", 59).
?DOC(
    " Set the header with the given value under the given header key.\n"
    "\n"
    " If the response already has that key, it is replaced.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec set_header(response(GVL), binary(), binary()) -> response(GVL).
set_header(Response, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Response),
        string:lowercase(Key),
        Value
    ),
    _record = Response,
    {response, erlang:element(2, _record), Headers, erlang:element(4, _record)}.

-file("src/gleam/http/response.gleam", 76).
?DOC(
    " Prepend the header with the given value under the given header key.\n"
    "\n"
    " Similar to `set_header` except if the header already exists it prepends\n"
    " another header with the same key.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec prepend_header(response(GVO), binary(), binary()) -> response(GVO).
prepend_header(Response, Key, Value) ->
    Headers = [{string:lowercase(Key), Value} | erlang:element(3, Response)],
    _record = Response,
    {response, erlang:element(2, _record), Headers, erlang:element(4, _record)}.

-file("src/gleam/http/response.gleam", 87).
?DOC(" Set the body of the response, overwriting any existing body.\n").
-spec set_body(response(any()), GVT) -> response(GVT).
set_body(Response, Body) ->
    {response, Status, Headers, _} = Response,
    {response, Status, Headers, Body}.

-file("src/gleam/http/response.gleam", 27).
?DOC(
    " Update the body of a response using a given result returning function.\n"
    "\n"
    " If the given function returns an `Ok` value the body is set, if it returns\n"
    " an `Error` value then the error is returned.\n"
).
-spec try_map(response(GUX), fun((GUX) -> {ok, GUZ} | {error, GVA})) -> {ok,
        response(GUZ)} |
    {error, GVA}.
try_map(Response, Transform) ->
    gleam@result:then(
        Transform(erlang:element(4, Response)),
        fun(Body) -> {ok, set_body(Response, Body)} end
    ).

-file("src/gleam/http/response.gleam", 97).
?DOC(" Update the body of a response using a given function.\n").
-spec map(response(GVV), fun((GVV) -> GVX)) -> response(GVX).
map(Response, Transform) ->
    _pipe = erlang:element(4, Response),
    _pipe@1 = Transform(_pipe),
    set_body(Response, _pipe@1).

-file("src/gleam/http/response.gleam", 108).
?DOC(" Create a response that redirects to the given uri.\n").
-spec redirect(binary()) -> response(binary()).
redirect(Uri) ->
    {response,
        303,
        [{<<"location"/utf8>>, Uri}],
        gleam@string:append(<<"You are being redirected to "/utf8>>, Uri)}.

-file("src/gleam/http/response.gleam", 120).
?DOC(
    " Fetch the cookies sent in a response. \n"
    "\n"
    " Badly formed cookies will be discarded.\n"
).
-spec get_cookies(response(any())) -> list({binary(), binary()}).
get_cookies(Resp) ->
    {response, _, Headers, _} = Resp,
    _pipe = Headers,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Header) ->
            {Name, Value} = Header,
            case Name of
                <<"set-cookie"/utf8>> ->
                    {ok, gleam@http@cookie:parse(Value)};

                _ ->
                    {error, nil}
            end
        end
    ),
    gleam@list:flatten(_pipe@1).

-file("src/gleam/http/response.gleam", 135).
?DOC(" Set a cookie value for a client\n").
-spec set_cookie(
    response(GWC),
    binary(),
    binary(),
    gleam@http@cookie:attributes()
) -> response(GWC).
set_cookie(Response, Name, Value, Attributes) ->
    prepend_header(
        Response,
        <<"set-cookie"/utf8>>,
        gleam@http@cookie:set_header(Name, Value, Attributes)
    ).

-file("src/gleam/http/response.gleam", 151).
?DOC(
    " Expire a cookie value for a client\n"
    "\n"
    " Note: The attributes value should be the same as when the response cookie was set.\n"
).
-spec expire_cookie(response(GWF), binary(), gleam@http@cookie:attributes()) -> response(GWF).
expire_cookie(Response, Name, Attributes) ->
    Attrs = begin
        _record = Attributes,
        {attributes,
            {some, 0},
            erlang:element(3, _record),
            erlang:element(4, _record),
            erlang:element(5, _record),
            erlang:element(6, _record),
            erlang:element(7, _record)}
    end,
    set_cookie(Response, Name, <<""/utf8>>, Attrs).
