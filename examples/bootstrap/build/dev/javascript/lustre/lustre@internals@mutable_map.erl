-module(lustre@internals@mutable_map).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, get/2, insert/3, delete/2, size/1, is_empty/1]).
-export_type([mutable_map/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type mutable_map(ONT, ONU) :: any() | {gleam_phantom, ONT, ONU}.

-file("src/lustre/internals/mutable_map.gleam", 21).
?DOC(false).
-spec new() -> mutable_map(any(), any()).
new() ->
    gleam@dict:new().

-file("src/lustre/internals/mutable_map.gleam", 27).
?DOC(false).
-spec get(mutable_map(ONZ, OOA), ONZ) -> {ok, OOA} | {error, nil}.
get(Map, Key) ->
    gleam@dict:get(Map, Key).

-file("src/lustre/internals/mutable_map.gleam", 33).
?DOC(false).
-spec insert(mutable_map(OOF, OOG), OOF, OOG) -> mutable_map(OOF, OOG).
insert(Map, Key, Value) ->
    gleam@dict:insert(Map, Key, Value).

-file("src/lustre/internals/mutable_map.gleam", 43).
?DOC(false).
-spec delete(mutable_map(OOL, OOM), OOL) -> mutable_map(OOL, OOM).
delete(Map, Key) ->
    gleam@dict:delete(Map, Key).

-file("src/lustre/internals/mutable_map.gleam", 49).
?DOC(false).
-spec size(mutable_map(any(), any())) -> integer().
size(Map) ->
    gleam@dict:size(Map).

-file("src/lustre/internals/mutable_map.gleam", 53).
?DOC(false).
-spec is_empty(mutable_map(any(), any())) -> boolean().
is_empty(Map) ->
    gleam@dict:size(Map) =:= 0.
