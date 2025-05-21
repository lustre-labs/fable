-module(gleam@otp@intensity_tracker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/2, trim_window/3, add_event/1]).
-export_type([intensity_tracker/0, too_intense/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " The intensity tracker is used to monitor how frequently an event happens,\n"
    " erroring if it happens too many times within a period of time.\n"
).

-opaque intensity_tracker() :: {intensity_tracker,
        integer(),
        integer(),
        list(integer())}.

-type too_intense() :: too_intense.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/intensity_tracker.gleam", 14).
-spec new(integer(), integer()) -> intensity_tracker().
new(Limit, Period) ->
    {intensity_tracker, Limit, Period, []}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/intensity_tracker.gleam", 21).
-spec now_seconds() -> integer().
now_seconds() ->
    erlang:monotonic_time(1).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/intensity_tracker.gleam", 25).
-spec trim_window(list(integer()), integer(), integer()) -> list(integer()).
trim_window(Events, Now, Period) ->
    case Events of
        [] ->
            [];

        [Event | Events@1] ->
            case Now < (Event + Period) of
                true ->
                    [Event | trim_window(Events@1, Now, Period)];

                false ->
                    []
            end
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/intensity_tracker.gleam", 36).
-spec add_event(intensity_tracker()) -> {ok, intensity_tracker()} |
    {error, too_intense()}.
add_event(Tracker) ->
    Now = now_seconds(),
    Events = trim_window(
        [Now | erlang:element(4, Tracker)],
        Now,
        erlang:element(3, Tracker)
    ),
    case erlang:length(Events) > erlang:element(2, Tracker) of
        true ->
            {error, too_intense};

        false ->
            {ok,
                begin
                    _record = Tracker,
                    {intensity_tracker,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        Events}
                end}
    end.
