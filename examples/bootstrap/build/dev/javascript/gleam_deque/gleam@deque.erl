-module(gleam@deque).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, is_empty/1, length/1, push_back/2, push_front/2, pop_back/1, pop_front/1, reverse/1, is_logically_equal/3, is_equal/2]).
-export_type([deque/1]).

-opaque deque(FWM) :: {deque, list(FWM), list(FWM)}.

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 23).
-spec new() -> deque(any()).
new() ->
    {deque, [], []}.

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 39).
-spec from_list(list(FWP)) -> deque(FWP).
from_list(List) ->
    {deque, [], List}.

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 55).
-spec to_list(deque(FWS)) -> list(FWS).
to_list(Deque) ->
    _pipe = erlang:element(3, Deque),
    lists:append(_pipe, lists:reverse(erlang:element(2, Deque))).

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 81).
-spec is_empty(deque(any())) -> boolean().
is_empty(Deque) ->
    (erlang:element(2, Deque) =:= []) andalso (erlang:element(3, Deque) =:= []).

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 107).
-spec length(deque(any())) -> integer().
length(Deque) ->
    erlang:length(erlang:element(2, Deque)) + erlang:length(
        erlang:element(3, Deque)
    ).

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 120).
-spec push_back(deque(FWZ), FWZ) -> deque(FWZ).
push_back(Deque, Item) ->
    {deque, [Item | erlang:element(2, Deque)], erlang:element(3, Deque)}.

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 133).
-spec push_front(deque(FXC), FXC) -> deque(FXC).
push_front(Deque, Item) ->
    {deque, erlang:element(2, Deque), [Item | erlang:element(3, Deque)]}.

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 165).
-spec pop_back(deque(FXF)) -> {ok, {FXF, deque(FXF)}} | {error, nil}.
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

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 204).
-spec pop_front(deque(FXK)) -> {ok, {FXK, deque(FXK)}} | {error, nil}.
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

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 237).
-spec reverse(deque(FXP)) -> deque(FXP).
reverse(Deque) ->
    {deque, erlang:element(3, Deque), erlang:element(2, Deque)}.

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 260).
-spec check_equal(
    list(FXV),
    list(FXV),
    list(FXV),
    list(FXV),
    fun((FXV, FXV) -> boolean())
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

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 252).
-spec is_logically_equal(deque(FXS), deque(FXS), fun((FXS, FXS) -> boolean())) -> boolean().
is_logically_equal(A, B, Element_is_equal) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        Element_is_equal
    ).

-file("/Users/louis/src/gleam/deque/src/gleam/deque.gleam", 289).
-spec is_equal(deque(FYA), deque(FYA)) -> boolean().
is_equal(A, B) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        fun(A@1, B@1) -> A@1 =:= B@1 end
    ).
