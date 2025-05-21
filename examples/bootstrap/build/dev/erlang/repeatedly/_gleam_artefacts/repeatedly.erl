-module(repeatedly).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([call/3, stop/1, set_function/2, update_state/2, set_state/2]).
-export_type([repeater/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type repeater(ABOQ) :: any() | {gleam_phantom, ABOQ}.

-file("src/repeatedly.gleam", 8).
?DOC(
    " Call a function every specified number of milliseconds, waiting the number\n"
    " of milliseconds before the first call.\n"
).
-spec call(integer(), ABOR, fun((ABOR, integer()) -> ABOR)) -> repeater(ABOR).
call(Delay_ms, State, Function) ->
    repeatedly_ffi:call(Delay_ms, State, Function).

-file("src/repeatedly.gleam", 24).
?DOC(
    " Stop the repeater, preventing it from triggering again.\n"
    "\n"
    " On Erlang if the repeater message queue is not empty then this message will\n"
    " handled after all other messages.\n"
    "\n"
    " On JavaScript there is no message queue so it will stop immediately, though\n"
    " not interrupt the function callback if currently being executed.\n"
).
-spec stop(repeater(any())) -> nil.
stop(Repeater) ->
    repeatedly_ffi:stop(Repeater).

-file("src/repeatedly.gleam", 36).
?DOC(
    " Replace the function being called by a repeater.\n"
    "\n"
    " On Erlang if the repeater message queue is not empty then this message will\n"
    " handled after all other messages.\n"
    "\n"
    " On JavaScript there is no message queue so it will stop immediately, though\n"
    " not interrupt the function callback if currently being executed.\n"
).
-spec set_function(repeater(ABOV), fun((ABOV, integer()) -> ABOV)) -> nil.
set_function(Repeater, Function) ->
    repeatedly_ffi:replace(Repeater, Function).

-file("src/repeatedly.gleam", 63).
?DOC(
    " Update the repeater state.\n"
    "\n"
    " On Erlang if the repeater message queue is not empty then this message will\n"
    " handled after all other messages.\n"
    "\n"
    " On JavaScript there is no message queue so it will stop immediately, though\n"
    " not interrupt the function callback if currently being executed.\n"
).
-spec update_state(repeater(ABOZ), fun((ABOZ) -> ABOZ)) -> nil.
update_state(Repeater, Function) ->
    repeatedly_ffi:update_state(Repeater, Function).

-file("src/repeatedly.gleam", 49).
?DOC(
    " Set the repeater state.\n"
    "\n"
    " On Erlang if the repeater message queue is not empty then this message will\n"
    " handled after all other messages.\n"
    "\n"
    " On JavaScript there is no message queue so it will stop immediately, though\n"
    " not interrupt the function callback if currently being executed.\n"
).
-spec set_state(repeater(ABOX), ABOX) -> nil.
set_state(Repeater, State) ->
    repeatedly_ffi:update_state(Repeater, fun(_) -> State end).
