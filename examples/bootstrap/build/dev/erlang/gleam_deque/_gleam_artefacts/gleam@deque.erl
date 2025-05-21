-module(gleam@deque).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, is_empty/1, length/1, push_back/2, push_front/2, pop_back/1, pop_front/1, reverse/1, is_logically_equal/3, is_equal/2]).
-export_type([deque/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque deque(FAA) :: {deque, list(FAA), list(FAA)}.

-file("src/gleam/deque.gleam", 23).
?DOC(" Creates a fresh deque that contains no values.\n").
-spec new() -> deque(any()).
new() ->
    {deque, [], []}.

-file("src/gleam/deque.gleam", 39).
?DOC(
    " Converts a list of elements into a deque of the same elements in the same\n"
    " order. The first element in the list becomes the front element in the deque.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3] |> from_list |> length\n"
    " // -> 3\n"
    " ```\n"
).
-spec from_list(list(FAD)) -> deque(FAD).
from_list(List) ->
    {deque, [], List}.

-file("src/gleam/deque.gleam", 55).
?DOC(
    " Converts a deque of elements into a list of the same elements in the same\n"
    " order. The front element in the deque becomes the first element in the list.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " new() |> push_back(1) |> push_back(2) |> to_list\n"
    " // -> [1, 2]\n"
    " ```\n"
).
-spec to_list(deque(FAG)) -> list(FAG).
to_list(Deque) ->
    _pipe = erlang:element(3, Deque),
    lists:append(_pipe, lists:reverse(erlang:element(2, Deque))).

-file("src/gleam/deque.gleam", 81).
?DOC(
    " Determines whether or not the deque is empty.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> from_list |> is_empty\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1] |> from_list |> is_empty\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2] |> from_list |> is_empty\n"
    " // -> False\n"
    " ```\n"
).
-spec is_empty(deque(any())) -> boolean().
is_empty(Deque) ->
    (erlang:element(2, Deque) =:= []) andalso (erlang:element(3, Deque) =:= []).

-file("src/gleam/deque.gleam", 107).
?DOC(
    " Counts the number of elements in a given deque.\n"
    "\n"
    " This function has to traverse the deque to determine the number of elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " length(from_list([]))\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length(from_list([1]))\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length(from_list([1, 2]))\n"
    " // -> 2\n"
    " ```\n"
).
-spec length(deque(any())) -> integer().
length(Deque) ->
    erlang:length(erlang:element(2, Deque)) + erlang:length(
        erlang:element(3, Deque)
    ).

-file("src/gleam/deque.gleam", 120).
?DOC(
    " Pushes an element onto the back of the deque.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2] |> from_list |> push_back(3) |> to_list\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec push_back(deque(FAN), FAN) -> deque(FAN).
push_back(Deque, Item) ->
    {deque, [Item | erlang:element(2, Deque)], erlang:element(3, Deque)}.

-file("src/gleam/deque.gleam", 133).
?DOC(
    " Pushes an element onto the front of the deque.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " [0, 0] |> from_list |> push_front(1) |> to_list\n"
    " // -> [1, 0, 0]\n"
    " ```\n"
).
-spec push_front(deque(FAQ), FAQ) -> deque(FAQ).
push_front(Deque, Item) ->
    {deque, erlang:element(2, Deque), [Item | erlang:element(3, Deque)]}.

-file("src/gleam/deque.gleam", 165).
?DOC(
    " Gets the last element from the deque, returning the\n"
    " element and a new deque without that element.\n"
    "\n"
    " This function typically runs in constant time, but will occasionally run in\n"
    " linear time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_back(0)\n"
    " |> push_back(1)\n"
    " |> pop_back\n"
    " // -> Ok(#(1, push_front(new(), 0)))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_front(0)\n"
    " |> pop_back\n"
    " // -> Ok(#(0, new()))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> pop_back\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop_back(deque(FAT)) -> {ok, {FAT, deque(FAT)}} | {error, nil}.
pop_back(Deque) ->
    case Deque of
        {deque, [], []} ->
            {error, nil};

        {deque, [], Out} ->
            pop_back({deque, lists:reverse(Out), []});

        {deque, [First | Rest], Out@1} ->
            Deque@1 = {deque, Rest, Out@1},
            {ok, {First, Deque@1}}
    end.

-file("src/gleam/deque.gleam", 204).
?DOC(
    " Gets the first element from the deque, returning the\n"
    " element and a new deque without that element.\n"
    "\n"
    " This function typically runs in constant time, but will occasionally run in\n"
    " linear time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_front(1)\n"
    " |> push_front(0)\n"
    " |> pop_front\n"
    " // -> Ok(#(0, push_back(new(), 1)))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " |> push_back(0)\n"
    " |> pop_front\n"
    " // -> Ok(#(0, new()))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " new() |> pop_back\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop_front(deque(FAY)) -> {ok, {FAY, deque(FAY)}} | {error, nil}.
pop_front(Deque) ->
    case Deque of
        {deque, [], []} ->
            {error, nil};

        {deque, In, []} ->
            pop_front({deque, [], lists:reverse(In)});

        {deque, In@1, [First | Rest]} ->
            Deque@1 = {deque, In@1, Rest},
            {ok, {First, Deque@1}}
    end.

-file("src/gleam/deque.gleam", 237).
?DOC(
    " Creates a new deque from a given deque containing the same elements, but in\n"
    " the opposite order.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> from_list |> reverse |> to_list\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1] |> from_list |> reverse |> to_list\n"
    " // -> [1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2] |> from_list |> reverse |> to_list\n"
    " // -> [2, 1]\n"
    " ```\n"
).
-spec reverse(deque(FBD)) -> deque(FBD).
reverse(Deque) ->
    {deque, erlang:element(3, Deque), erlang:element(2, Deque)}.

-file("src/gleam/deque.gleam", 260).
-spec check_equal(
    list(FBJ),
    list(FBJ),
    list(FBJ),
    list(FBJ),
    fun((FBJ, FBJ) -> boolean())
) -> boolean().
check_equal(Xs, X_tail, Ys, Y_tail, Eq) ->
    case {Xs, X_tail, Ys, Y_tail} of
        {[], [], [], []} ->
            true;

        {[X | Xs@1], _, [Y | Ys@1], _} ->
            case Eq(X, Y) of
                false ->
                    false;

                true ->
                    check_equal(Xs@1, X_tail, Ys@1, Y_tail, Eq)
            end;

        {[], [_ | _], _, _} ->
            check_equal(lists:reverse(X_tail), [], Ys, Y_tail, Eq);

        {_, _, [], [_ | _]} ->
            check_equal(Xs, X_tail, lists:reverse(Y_tail), [], Eq);

        {_, _, _, _} ->
            false
    end.

-file("src/gleam/deque.gleam", 252).
?DOC(
    " Checks whether two deques have equal elements in the same order, where the\n"
    " equality of elements is determined by a given equality checking function.\n"
    "\n"
    " This function is useful as the internal representation may be different for\n"
    " two deques with the same elements in the same order depending on how they\n"
    " were constructed, so the equality operator `==` may return surprising\n"
    " results.\n"
    "\n"
    " This function runs in linear time multiplied by the time taken by the\n"
    " element equality checking function.\n"
).
-spec is_logically_equal(deque(FBG), deque(FBG), fun((FBG, FBG) -> boolean())) -> boolean().
is_logically_equal(A, B, Element_is_equal) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        Element_is_equal
    ).

-file("src/gleam/deque.gleam", 289).
?DOC(
    " Checks whether two deques have the same elements in the same order.\n"
    "\n"
    " This function is useful as the internal representation may be different for\n"
    " two deques with the same elements in the same order depending on how they\n"
    " were constructed, so the equality operator `==` may return surprising\n"
    " results.\n"
    "\n"
    " This function runs in linear time.\n"
).
-spec is_equal(deque(FBO), deque(FBO)) -> boolean().
is_equal(A, B) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        fun(A@1, B@1) -> A@1 =:= B@1 end
    ).
