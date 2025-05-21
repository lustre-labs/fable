-module(lustre_fable@value).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([as_string/1, as_int/1, as_float/1, as_bool/1, to_json/1, decoder/0]).
-export_type([value/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type value() :: {primitive_string, binary()} |
    {primitive_int, integer()} |
    {primitive_float, float()} |
    {primitive_bool, boolean()}.

-file("src/lustre_fable/value.gleam", 21).
?DOC(false).
-spec as_string(value()) -> binary().
as_string(Value) ->
    case Value of
        {primitive_string, String} ->
            String;

        _ ->
            <<""/utf8>>
    end.

-file("src/lustre_fable/value.gleam", 30).
?DOC(false).
-spec as_int(value()) -> integer().
as_int(Value) ->
    case Value of
        {primitive_int, Int} ->
            Int;

        _ ->
            0
    end.

-file("src/lustre_fable/value.gleam", 39).
?DOC(false).
-spec as_float(value()) -> float().
as_float(Value) ->
    case Value of
        {primitive_float, Float} ->
            Float;

        _ ->
            +0.0
    end.

-file("src/lustre_fable/value.gleam", 48).
?DOC(false).
-spec as_bool(value()) -> boolean().
as_bool(Value) ->
    case Value of
        {primitive_bool, Bool} ->
            Bool;

        _ ->
            false
    end.

-file("src/lustre_fable/value.gleam", 59).
?DOC(false).
-spec to_json(value()) -> gleam@json:json().
to_json(Value) ->
    case Value of
        {primitive_string, String} ->
            gleam@json:string(String);

        {primitive_int, Int} ->
            gleam@json:int(Int);

        {primitive_float, Float} ->
            gleam@json:float(Float);

        {primitive_bool, Bool} ->
            gleam@json:bool(Bool)
    end.

-file("src/lustre_fable/value.gleam", 70).
?DOC(false).
-spec decoder() -> gleam@dynamic@decode:decoder(value()).
decoder() ->
    gleam@dynamic@decode:one_of(
        begin
            _pipe = {decoder, fun gleam@dynamic@decode:decode_string/1},
            gleam@dynamic@decode:map(
                _pipe,
                fun(Field@0) -> {primitive_string, Field@0} end
            )
        end,
        [begin
                _pipe@1 = {decoder, fun gleam@dynamic@decode:decode_int/1},
                gleam@dynamic@decode:map(
                    _pipe@1,
                    fun(Field@0) -> {primitive_int, Field@0} end
                )
            end,
            begin
                _pipe@2 = {decoder, fun gleam@dynamic@decode:decode_float/1},
                gleam@dynamic@decode:map(
                    _pipe@2,
                    fun(Field@0) -> {primitive_float, Field@0} end
                )
            end,
            begin
                _pipe@3 = {decoder, fun gleam@dynamic@decode:decode_bool/1},
                gleam@dynamic@decode:map(
                    _pipe@3,
                    fun(Field@0) -> {primitive_bool, Field@0} end
                )
            end]
    ).
