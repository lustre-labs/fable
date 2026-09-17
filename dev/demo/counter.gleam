// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre/attribute
import lustre/dev/query.{type Query}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// VIEW ------------------------------------------------------------------------

pub fn view(
  count count: Int,
  on_increment handle_increment: message,
  on_decrement handle_decrement: message,
  on_reset handle_reset: message,
) -> Element(message) {
  html.div([], [
    view_control("decr", handle_decrement, "-"),
    view_count(count),
    view_control("incr", handle_increment, "+"),
    view_control("reset", handle_reset, "Reset"),
  ])
}

fn view_control(
  test_id: String,
  handler: message,
  label: String,
) -> Element(message) {
  html.button([attribute.data("test-id", test_id), event.on_click(handler)], [
    html.text(label),
  ])
}

fn view_count(count: Int) -> Element(message) {
  html.span([], [html.text(int.to_string(count))])
}

// TEST UTILS ------------------------------------------------------------------

pub fn decrement() -> Query {
  query.element(matching: query.test_id("decr"))
}

pub fn increment() -> Query {
  query.element(matching: query.test_id("incr"))
}

pub fn reset() -> Query {
  query.element(matching: query.test_id("reset"))
}
