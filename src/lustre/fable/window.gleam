// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}

// UTILS -----------------------------------------------------------------------

@external(javascript, "./window.ffi.mjs", "match_media")
pub fn match_media(_query: String) -> Bool {
  False
}

@external(javascript, "./window.ffi.mjs", "watch_media")
pub fn watch_media(_query: String, _callback: fn(Bool) -> Nil) -> Nil {
  Nil
}

@external(javascript, "./window.ffi.mjs", "add_event_listener")
pub fn add_event_listener(_name: String, _callback: fn(Dynamic) -> Nil) -> Nil {
  Nil
}
