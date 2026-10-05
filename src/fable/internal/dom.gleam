import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import lustre/effect.{type Effect}
import lustre/event.{type Handler}

/// Focus an element by ID in the application's document or shadow root.
pub fn focus(id: String) -> Effect(message) {
  use _, root <- effect.before_paint
  do_focus(root, id)
}

@external(javascript, "./dom.ffi.mjs", "focus")
fn do_focus(root: Dynamic, id: String) -> Nil

///
pub fn add_global_event_listener(
  name: String,
  handler: Decoder(Handler(message)),
) -> Effect(message) {
  use dispatch, root <- effect.before_paint
  use event <- do_add_global_event_listener(root, name)
  case decode.run(event, handler) {
    Ok(handler) -> {
      handle_event(event, handler.prevent_default, handler.stop_propagation)
      dispatch(handler.message)
    }
    Error(_) -> Nil
  }
}

@external(javascript, "./dom.ffi.mjs", "addGlobalEventListener")
fn do_add_global_event_listener(
  root: Dynamic,
  name: String,
  handler: fn(Dynamic) -> Nil,
) -> Nil

@external(javascript, "./dom.ffi.mjs", "handleEvent")
fn handle_event(
  event: Dynamic,
  prevent_default: Bool,
  stop_propagation: Bool,
) -> Nil
