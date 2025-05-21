-module(repeatedly).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([call/3, stop/1, set_function/2, update_state/2, set_state/2]).
-export_type([repeater/1]).

-type repeater(FLM) :: any() | {gleam_phantom, FLM}.

-file("/Users/louis/src/gleam/repeatedly/src/repeatedly.gleam", 8).
-spec call(integer(), FLN, fun((FLN, integer()) -> FLN)) -> repeater(FLN).
call(Delay_ms, State, Function) ->
    repeatedly_ffi:call(Delay_ms, State, Function).

-file("/Users/louis/src/gleam/repeatedly/src/repeatedly.gleam", 24).
-spec stop(repeater(any())) -> nil.
stop(Repeater) ->
    repeatedly_ffi:stop(Repeater).

-file("/Users/louis/src/gleam/repeatedly/src/repeatedly.gleam", 36).
-spec set_function(repeater(FLR), fun((FLR, integer()) -> FLR)) -> nil.
set_function(Repeater, Function) ->
    repeatedly_ffi:replace(Repeater, Function).

-file("/Users/louis/src/gleam/repeatedly/src/repeatedly.gleam", 63).
-spec update_state(repeater(FLV), fun((FLV) -> FLV)) -> nil.
update_state(Repeater, Function) ->
    repeatedly_ffi:update_state(Repeater, Function).

-file("/Users/louis/src/gleam/repeatedly/src/repeatedly.gleam", 49).
-spec set_state(repeater(FLT), FLT) -> nil.
set_state(Repeater, State) ->
    repeatedly_ffi:update_state(Repeater, fun(_) -> State end).
