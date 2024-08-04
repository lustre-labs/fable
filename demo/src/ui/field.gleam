// IMPORTS ---------------------------------------------------------------------

import fable
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// ELEMENTS --------------------------------------------------------------------

pub fn field(
  on_input handle_input: fn(String) -> msg,
  label label: String,
  value value: String,
  message error: String,
) -> Element(msg) {
  let wrapper_styles = [#("display", "flex"), #("flex-direction", "column")]
  let label_styles = [#("font-size", "0.8em")]
  let input_styles = [
    #("padding", "0.5em 1em"),
    #("border", "1px solid"),
    #("border-color", case error {
      "" -> "#ccc"
      _ -> "red"
    }),
    #("border-radius", "0.5em"),
  ]
  let error_styles = [
    #("align-self", "flex-end"),
    #("font-size", "0.8em"),
    #("color", "red"),
  ]

  html.label([attribute.style(wrapper_styles)], [
    html.p([attribute.style(label_styles)], [html.text(label)]),
    html.input([
      attribute.style(input_styles),
      attribute.value(value),
      event.on_input(handle_input),
    ]),
    html.p([attribute.style(error_styles)], [html.text(error)]),
  ])
}

pub fn field_story() {
  use <- fable.story("field")
  use label <- fable.input("Label value")
  use value <- fable.input("Input value")
  use error <- fable.input("Error message")

  use state <- fable.render

  field(
    on_input: value.set,
    label: label.get(state),
    value: value.get(state),
    message: error.get(state),
  )
}

pub fn field_with_label_story() {
  use <- fable.story("field with label")
  use value <- fable.input("Input value")

  use state <- fable.render

  field(
    on_input: value.set,
    label: "username",
    value: value.get(state),
    message: "",
  )
}

pub fn field_with_message_story() {
  use <- fable.story("field with message")
  use value <- fable.input("Input value")

  use state <- fable.render

  field(
    on_input: value.set,
    label: "username",
    value: value.get(state),
    message: "username already exists.",
  )
}
