//// The intensity tracker is used to monitor how frequently an event happens,
//// erroring if it happens too many times within a period of time.

import gleam/list

pub opaque type IntensityTracker {
  IntensityTracker(limit: Int, period: Int, events: List(Int))
}

pub type TooIntense {
  TooIntense
}

pub fn new(limit limit: Int, period period: Int) -> IntensityTracker {
  IntensityTracker(limit: limit, period: period, events: [])
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(a: Int) -> Int

fn now_seconds() -> Int {
  monotonic_time(1)
}

pub fn trim_window(events: List(Int), now: Int, period: Int) -> List(Int) {
  case events {
    [] -> []
    [event, ..events] ->
      case now < event + period {
        True -> [event, ..trim_window(events, now, period)]
        False -> []
      }
  }
}

pub fn add_event(
  tracker: IntensityTracker,
) -> Result(IntensityTracker, TooIntense) {
  let now = now_seconds()
  let events = trim_window([now, ..tracker.events], now, tracker.period)
  case list.length(events) > tracker.limit {
    True -> Error(TooIntense)
    False -> Ok(IntensityTracker(..tracker, events: events))
  }
}
