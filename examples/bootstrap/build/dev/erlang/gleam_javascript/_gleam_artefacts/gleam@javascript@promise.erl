-module(gleam@javascript@promise).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([promise/1]).

-type promise(HCX) :: any() | {gleam_phantom, HCX}.


