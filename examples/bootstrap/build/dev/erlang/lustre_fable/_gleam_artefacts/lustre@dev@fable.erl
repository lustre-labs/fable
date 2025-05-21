-module(lustre@dev@fable).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([book/1, chapter/3, stylesheet/2, external_stylesheet/2, start/1, story/2, scene/1, input/2, checkbox/2, select/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/dev/fable.gleam", 24).
?DOC("\n").
-spec book(binary()) -> lustre_fable@book:book().
book(Title) ->
    {book, Title, [], [], []}.

-file("src/lustre/dev/fable.gleam", 30).
?DOC("\n").
-spec chapter(
    lustre_fable@book:book(),
    binary(),
    list(lustre_fable@story:story_config())
) -> lustre_fable@book:book().
chapter(Book, Title, Stories) ->
    _record = Book,
    {book,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        [{Title, Stories} | erlang:element(5, Book)]}.

-file("src/lustre/dev/fable.gleam", 36).
?DOC("\n").
-spec stylesheet(lustre_fable@book:book(), binary()) -> lustre_fable@book:book().
stylesheet(Book, Stylesheet) ->
    _record = Book,
    {book,
        erlang:element(2, _record),
        [Stylesheet | erlang:element(3, Book)],
        erlang:element(4, _record),
        erlang:element(5, _record)}.

-file("src/lustre/dev/fable.gleam", 42).
?DOC("\n").
-spec external_stylesheet(lustre_fable@book:book(), binary()) -> lustre_fable@book:book().
external_stylesheet(Book, Href) ->
    _record = Book,
    {book,
        erlang:element(2, _record),
        erlang:element(3, _record),
        [Href | erlang:element(4, Book)],
        erlang:element(5, _record)}.

-file("src/lustre/dev/fable.gleam", 46).
-spec start(lustre_fable@book:book()) -> nil.
start(Book) ->
    _ = lustre_fable@book:start(Book),
    nil.

-file("src/lustre/dev/fable.gleam", 78).
?DOC("\n").
-spec story(binary(), fun(() -> lustre_fable@story:story_builder())) -> lustre_fable@story:story_config().
story(Title, Builder) ->
    _record = (erlang:element(2, Builder()))(0),
    {story_config,
        Title,
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record)}.

-file("src/lustre/dev/fable.gleam", 84).
?DOC("\n").
-spec scene(
    fun((lustre_fable@story:model()) -> lustre@vdom@vnode:element(lustre_fable@story:msg()))
) -> lustre_fable@story:story_builder().
scene(View) ->
    {story_builder,
        fun(_) ->
            {story_config,
                <<""/utf8>>,
                [],
                [lustre@component:adopt_styles(false)],
                View}
        end}.

-file("src/lustre/dev/fable.gleam", 154).
-spec control(
    fun((CUQ) -> lustre_fable@value:value()),
    fun((lustre_fable@value:value()) -> CUQ),
    fun((CUQ, fun((CUQ) -> lustre_fable@story:msg())) -> lustre@vdom@vnode:element(lustre_fable@story:msg())),
    fun((fun((lustre_fable@story:model()) -> CUQ), fun((CUQ) -> lustre_fable@story:msg())) -> lustre_fable@story:story_builder())
) -> lustre_fable@story:story_builder().
control(Wrap, Read, View, Next) ->
    {story_builder,
        fun(Key) ->
            State = fun(Model) -> _pipe = erlang:element(2, Model),
                _pipe@1 = gleam_stdlib:map_get(_pipe, Key),
                _pipe@2 = gleam@result:unwrap(
                    _pipe@1,
                    {primitive_string, <<""/utf8>>}
                ),
                Read(_pipe@2) end,
            Set_state = fun(Value) -> _pipe@3 = Value,
                _pipe@4 = Wrap(_pipe@3),
                {user_edited_value, Key, _pipe@4} end,
            Input = fun(Model@1) -> View(State(Model@1), Set_state) end,
            Option = lustre@component:on_property_change(
                erlang:integer_to_binary(Key),
                begin
                    _pipe@5 = lustre_fable@value:decoder(),
                    _pipe@6 = gleam@dynamic@decode:map(
                        _pipe@5,
                        fun(_capture) ->
                            {component_updated_value, Key, _capture}
                        end
                    ),
                    gleam@dynamic@decode:map(
                        _pipe@6,
                        fun(Msg) ->
                            echo(Msg, "src/lustre/dev/fable.gleam", 185)
                        end
                    )
                end
            ),
            {story_config, Title, Inputs, Options, View@1} = (erlang:element(
                2,
                Next(State, Set_state)
            ))(Key + 1),
            {story_config, Title, [Input | Inputs], [Option | Options], View@1}
        end}.

-file("src/lustre/dev/fable.gleam", 99).
?DOC("\n").
-spec input(
    binary(),
    fun((fun((lustre_fable@story:model()) -> binary()), fun((binary()) -> lustre_fable@story:msg())) -> lustre_fable@story:story_builder())
) -> lustre_fable@story:story_builder().
input(Label, Next) ->
    control(
        fun(Field@0) -> {primitive_string, Field@0} end,
        fun lustre_fable@value:as_string/1,
        fun(Value, Set_value) ->
            lustre@element@html:label(
                [],
                [lustre@element@html:p([], [lustre@element@html:text(Label)]),
                    lustre@element@html:input(
                        [lustre@attribute:value(Value),
                            lustre@event:on_input(Set_value)]
                    )]
            )
        end,
        Next
    ).

-file("src/lustre/dev/fable.gleam", 113).
?DOC("\n").
-spec checkbox(
    binary(),
    fun((fun((lustre_fable@story:model()) -> boolean()), fun((boolean()) -> lustre_fable@story:msg())) -> lustre_fable@story:story_builder())
) -> lustre_fable@story:story_builder().
checkbox(Label, Next) ->
    control(
        fun(Field@0) -> {primitive_bool, Field@0} end,
        fun lustre_fable@value:as_bool/1,
        fun(Value, Set_value) ->
            lustre@element@html:label(
                [],
                [lustre@element@html:p([], [lustre@element@html:text(Label)]),
                    lustre@element@html:input(
                        [lustre@attribute:checked(Value),
                            lustre@attribute:type_(<<"checkbox"/utf8>>),
                            lustre@event:on_check(Set_value)]
                    )]
            )
        end,
        Next
    ).

-file("src/lustre/dev/fable.gleam", 131).
?DOC("\n").
-spec select(
    binary(),
    list({binary(), binary()}),
    fun((fun((lustre_fable@story:model()) -> binary()), fun((binary()) -> lustre_fable@story:msg())) -> lustre_fable@story:story_builder())
) -> lustre_fable@story:story_builder().
select(Label, Options, Next) ->
    control(
        fun(Field@0) -> {primitive_string, Field@0} end,
        fun lustre_fable@value:as_string/1,
        fun(Value, Set_value) ->
            Options@1 = gleam@list:map(
                Options,
                fun(Option) ->
                    lustre@element@html:option(
                        [lustre@attribute:value(erlang:element(1, Option)),
                            lustre@attribute:selected(
                                Value =:= erlang:element(1, Option)
                            )],
                        erlang:element(2, Option)
                    )
                end
            ),
            lustre@element@html:label(
                [],
                [lustre@element@html:p([], [lustre@element@html:text(Label)]),
                    lustre@element@html:select(
                        [lustre@event:on_change(Set_value)],
                        [lustre@element@html:option(
                                [lustre@attribute:value(<<""/utf8>>),
                                    lustre@attribute:selected(
                                        Value =:= <<""/utf8>>
                                    )],
                                <<""/utf8>>
                            ) |
                            Options@1]
                    )]
            )
        end,
        Next
    ).

-define(is_lowercase_char(X), (X > 96 andalso X < 123)).
-define(is_underscore_char(X), (X == 95)).
-define(is_digit_char(X), (X > 47 andalso X < 58)).
-define(could_be_record(Tuple),
    erlang:is_tuple(Tuple) andalso
        erlang:is_atom(erlang:element(1, Tuple)) andalso
        erlang:element(1, Tuple) =/= false andalso
        erlang:element(1, Tuple) =/= true andalso
        erlang:element(1, Tuple) =/= nil
).
-define(is_atom_char(C),
    (?is_lowercase_char(C) orelse
        ?is_underscore_char(C) orelse
        ?is_digit_char(C))
).

-define(grey, "\e[90m").
-define(reset_color, "\e[39m").

echo(Value, File, Line) ->
    StringLine = erlang:integer_to_list(Line),
    StringValue = echo@inspect(Value),
    io:put_chars(
      standard_error,
      [?grey, File, $:, StringLine, ?reset_color, $\n, StringValue, $\n]
    ),
    Value.

echo@inspect(Value) ->
    case Value of
        nil -> "Nil";
        true -> "True";
        false -> "False";
        Int when erlang:is_integer(Int) -> erlang:integer_to_list(Int);
        Float when erlang:is_float(Float) -> io_lib_format:fwrite_g(Float);
        Binary when erlang:is_binary(Binary) -> inspect@binary(Binary);
        Bits when erlang:is_bitstring(Bits) -> inspect@bit_array(Bits);
        Atom when erlang:is_atom(Atom) -> inspect@atom(Atom);
        List when erlang:is_list(List) -> inspect@list(List);
        Map when erlang:is_map(Map) -> inspect@map(Map);
        Record when ?could_be_record(Record) -> inspect@record(Record);
        Tuple when erlang:is_tuple(Tuple) -> inspect@tuple(Tuple);
        Function when erlang:is_function(Function) -> inspect@function(Function);
        Any -> ["//erl(", io_lib:format("~p", [Any]), ")"]
    end.

inspect@bit_array(Bits) ->
    Pieces = inspect@bit_array_pieces(Bits, []),
    Inner = lists:join(", ", lists:reverse(Pieces)),
    ["<<", Inner, ">>"].

inspect@bit_array_pieces(Bits, Acc) ->
    case Bits of
        <<>> ->
            Acc;
        <<Byte, Rest/bitstring>> ->
            inspect@bit_array_pieces(Rest, [erlang:integer_to_binary(Byte) | Acc]);
        _ ->
            Size = erlang:bit_size(Bits),
            <<RemainingBits:Size>> = Bits,
            SizeString = [":size(", erlang:integer_to_binary(Size), ")"],
            Piece = [erlang:integer_to_binary(RemainingBits), SizeString],
            [Piece | Acc]
    end.

inspect@binary(Binary) ->
    case inspect@maybe_utf8_string(Binary, <<>>) of
        {ok, InspectedUtf8String} ->
            InspectedUtf8String;
        {error, not_a_utf8_string} ->
            Segments = [erlang:integer_to_list(X) || <<X>> <= Binary],
            ["<<", lists:join(", ", Segments), ">>"]
    end.

inspect@atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    case inspect@maybe_gleam_atom(Binary, none, <<>>) of
        {ok, Inspected} -> Inspected;
        {error, _} -> ["atom.create_from_string(\"", Binary, "\")"]
    end.

inspect@list(List) ->
    case inspect@proper_or_improper_list(List) of
        {proper, Elements} -> ["[", Elements, "]"];
        {improper, Elements} -> ["//erl([", Elements, "])"]
    end.

inspect@map(Map) ->
    Fields = [
        [<<"#(">>, echo@inspect(Key), <<", ">>, echo@inspect(Value), <<")">>]
        || {Key, Value} <- maps:to_list(Map)
    ],
    ["dict.from_list([", lists:join(", ", Fields), "])"].

inspect@record(Record) ->
    [Atom | ArgsList] = Tuple = erlang:tuple_to_list(Record),
    case inspect@maybe_gleam_atom(Atom, none, <<>>) of
        {ok, Tag} ->
            Args = lists:join(", ", lists:map(fun echo@inspect/1, ArgsList)),
            [Tag, "(", Args, ")"];
        _ ->
            inspect@tuple(Tuple)
    end.

inspect@tuple(Tuple) when erlang:is_tuple(Tuple) ->
    inspect@tuple(erlang:tuple_to_list(Tuple));
inspect@tuple(Tuple) ->
    Elements = lists:map(fun echo@inspect/1, Tuple),
    ["#(", lists:join(", ", Elements), ")"].

inspect@function(Function) ->
    {arity, Arity} = erlang:fun_info(Function, arity),
    ArgsAsciiCodes = lists:seq($a, $a + Arity - 1),
    Args = lists:join(", ", lists:map(fun(Arg) -> <<Arg>> end, ArgsAsciiCodes)),
    ["//fn(", Args, ") { ... }"].

inspect@maybe_utf8_string(Binary, Acc) ->
    case Binary of
        <<>> ->
            {ok, <<$", Acc/binary, $">>};
        <<First/utf8, Rest/binary>> ->
            Escaped = inspect@escape_grapheme(First),
            inspect@maybe_utf8_string(Rest, <<Acc/binary, Escaped/binary>>);
        _ ->
            {error, not_a_utf8_string}
    end.

inspect@escape_grapheme(Char) ->
    case Char of
        $" -> <<$\\, $">>;
        $\\ -> <<$\\, $\\>>;
        $\r -> <<$\\, $r>>;
        $\n -> <<$\\, $n>>;
        $\t -> <<$\\, $t>>;
        $\f -> <<$\\, $f>>;
        X when X > 126, X < 160 -> inspect@convert_to_u(X);
        X when X < 32 -> inspect@convert_to_u(X);
        Other -> <<Other/utf8>>
    end.

inspect@convert_to_u(Code) ->
    erlang:list_to_binary(io_lib:format("\\u{~4.16.0B}", [Code])).

inspect@proper_or_improper_list(List) ->
    case List of
        [] ->
            {proper, []};
        [First] ->
            {proper, [echo@inspect(First)]};
        [First | Rest] when erlang:is_list(Rest) ->
            {Kind, Inspected} = inspect@proper_or_improper_list(Rest),
            {Kind, [echo@inspect(First), ", " | Inspected]};
        [First | ImproperRest] ->
            {improper, [echo@inspect(First), " | ", echo@inspect(ImproperRest)]}
    end.

inspect@maybe_gleam_atom(Atom, PrevChar, Acc) when erlang:is_atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    inspect@maybe_gleam_atom(Binary, PrevChar, Acc);
inspect@maybe_gleam_atom(Atom, PrevChar, Acc) ->
    case {Atom, PrevChar} of
        {<<>>, none} ->
            {error, nil};
        {<<First, _/binary>>, none} when ?is_digit_char(First) ->
            {error, nil};
        {<<"_", _/binary>>, none} ->
            {error, nil};
        {<<"_">>, _} ->
            {error, nil};
        {<<"_", _/binary>>, $_} ->
            {error, nil};
        {<<First, _/binary>>, _} when not ?is_atom_char(First) ->
            {error, nil};
        {<<First, Rest/binary>>, none} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
        {<<"_", Rest/binary>>, _} ->
            inspect@maybe_gleam_atom(Rest, $_, Acc);
        {<<First, Rest/binary>>, $_} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
        {<<First, Rest/binary>>, _} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, First>>);
        {<<>>, _} ->
            {ok, Acc};
        _ ->
            erlang:throw({gleam_error, echo, Atom, PrevChar, Acc})
    end.

inspect@uppercase(X) -> X - 32.

