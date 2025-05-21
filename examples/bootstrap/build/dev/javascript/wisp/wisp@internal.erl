-module(wisp@internal).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([remove_preceeding_slashes/1, join_path/2, random_string/1, random_slug/0, make_connection/2, generate_etag/2]).
-export_type([connection/0, read/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type connection() :: {connection,
        fun((integer()) -> {ok, read()} | {error, nil}),
        integer(),
        integer(),
        integer(),
        binary(),
        binary()}.

-type read() :: {chunk,
        bitstring(),
        fun((integer()) -> {ok, read()} | {error, nil})} |
    reading_finished.

-file("src/wisp/internal.gleam", 61).
?DOC(false).
-spec remove_preceeding_slashes(binary()) -> binary().
remove_preceeding_slashes(String) ->
    case String of
        <<"/"/utf8, Rest/binary>> ->
            remove_preceeding_slashes(Rest);

        _ ->
            String
    end.

-file("src/wisp/internal.gleam", 69).
?DOC(false).
-spec join_path(binary(), binary()) -> binary().
join_path(A, B) ->
    B@1 = remove_preceeding_slashes(B),
    case gleam_stdlib:string_ends_with(A, <<"/"/utf8>>) of
        true ->
            <<A/binary, B@1/binary>>;

        false ->
            <<<<A/binary, "/"/utf8>>/binary, B@1/binary>>
    end.

-file("src/wisp/internal.gleam", 83).
?DOC(false).
-spec random_string(integer()) -> binary().
random_string(Length) ->
    _pipe = crypto:strong_rand_bytes(Length),
    _pipe@1 = gleam@bit_array:base64_url_encode(_pipe, false),
    gleam@string:slice(_pipe@1, 0, Length).

-file("src/wisp/internal.gleam", 89).
?DOC(false).
-spec random_slug() -> binary().
random_slug() ->
    random_string(16).

-file("src/wisp/internal.gleam", 29).
?DOC(false).
-spec make_connection(fun((integer()) -> {ok, read()} | {error, nil}), binary()) -> connection().
make_connection(Body_reader, Secret_key_base) ->
    Prefix = case directories:tmp_dir() of
        {ok, Tmp_dir} ->
            <<Tmp_dir/binary, "/gleam-wisp/"/utf8>>;

        {error, _} ->
            <<"./tmp/"/utf8>>
    end,
    Temporary_directory = join_path(Prefix, random_slug()),
    {connection,
        Body_reader,
        8000000,
        32000000,
        1000000,
        Secret_key_base,
        Temporary_directory}.

-file("src/wisp/internal.gleam", 96).
?DOC(false).
-spec generate_etag(integer(), integer()) -> binary().
generate_etag(File_size, Mtime_seconds) ->
    <<<<(gleam@int:to_base16(File_size))/binary, "-"/utf8>>/binary,
        (gleam@int:to_base16(Mtime_seconds))/binary>>.
