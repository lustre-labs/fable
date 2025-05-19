// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import lustre/attribute
import lustre/component
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre_fable/story.{StoryBuilder, StoryConfig}
import lustre_fable/value.{type Value, PrimitiveBool, PrimitiveString}

//

pub fn example_story() {
  // Initialise a new story
  use <- story("This is my fancy input story")

  // Set up some state. Every piece of state has an associated input so it can
  // be controlled in the storybook control panel.
  use label, _ <- input("Label")
  use value, set_value <- input("Value")
  use disabled, _ <- checkbox("Disabled")

  // This is what you want to demo in the story, you can access the state we
  // configured above using the `model`
  let view = fn(model) {
    html.div([], [
      html.label([], [
        html.p([], [html.text(label(model))]),
        html.input([
          attribute.value(value(model)),
          attribute.disabled(disabled(model)),
          event.on_input(set_value),
        ]),
      ]),
    ])
  }

  // Render the scene!
  scene(view)
}

//

///
///
pub type Story =
  story.Story

///
///
pub type StoryBuilder =
  story.StoryBuilder

///
///
pub type Model =
  story.Model

///
///
pub type Msg =
  story.Msg

// BUILDING STORIES ------------------------------------------------------------

///
///
pub fn story(title: String, builder: fn() -> StoryBuilder) -> Story {
  let StoryConfig(inputs:, options:, view:) = builder().run(0)
  let assert Ok(story) = story.register(title, inputs, options, view)

  story
}

///
///
pub fn scene(view: fn(Model) -> Element(Msg)) -> StoryBuilder {
  use _ <- StoryBuilder

  StoryConfig(inputs: [], options: [component.adopt_styles(False)], view:)
}

// INPUTS AND CONTROLS ---------------------------------------------------------

///
///
pub fn input(
  label: String,
  next: fn(fn(Model) -> String, fn(String) -> Msg) -> StoryBuilder,
) -> StoryBuilder {
  use value, set_value <- control(PrimitiveString, value.as_string, _, next)

  html.label([], [
    html.p([], [html.text(label)]),
    html.input([attribute.value(value), event.on_input(set_value)]),
  ])
}

///
///
pub fn checkbox(
  label: String,
  next: fn(fn(Model) -> Bool, fn(Bool) -> Msg) -> StoryBuilder,
) -> StoryBuilder {
  use value, set_value <- control(PrimitiveBool, value.as_bool, _, next)

  html.label([], [
    html.p([], [html.text(label)]),
    html.input([
      attribute.checked(value),
      attribute.type_("checkbox"),
      event.on_check(set_value),
    ]),
  ])
}

///
///
pub fn select(
  label: String,
  options: List(#(String, String)),
  next: fn(fn(Model) -> String, fn(String) -> Msg) -> StoryBuilder,
) -> StoryBuilder {
  use value, set_value <- control(PrimitiveString, value.as_string, _, next)
  let options =
    list.map(options, fn(option) {
      html.option(
        [attribute.value(option.0), attribute.selected(value == option.0)],
        option.1,
      )
    })

  html.label([], [
    html.p([], [html.text(label)]),
    html.select([event.on_change(set_value)], [
      html.option([attribute.value(""), attribute.selected(value == "")], ""),
      ..options
    ]),
  ])
}

fn control(
  wrap: fn(value) -> Value,
  read: fn(Value) -> value,
  view: fn(value, fn(value) -> Msg) -> Element(Msg),
  next: fn(fn(Model) -> value, fn(value) -> Msg) -> StoryBuilder,
) -> StoryBuilder {
  use key <- StoryBuilder

  let state = fn(model: Model) {
    model.lookup
    |> dict.get(key)
    // If the state hasn't yet been set we'll just default to an empty string. The
    // provided `read` function will default to a value of whatever type is
    // appropriate.
    |> result.unwrap(PrimitiveString(""))
    |> read
  }

  let set_state = fn(value) {
    value
    |> wrap
    |> story.UserEditedValue(key:, value: _)
  }

  let input = fn(model) { view(state(model), set_state) }

  let option =
    component.on_property_change(
      int.to_string(key),
      decode.map(value.decoder(), story.ComponentUpdatedValue(key:, value: _)),
    )

  let StoryConfig(inputs:, options:, view:) =
    next(state, set_state).run(key + 1)

  StoryConfig(inputs: [input, ..inputs], options: [option, ..options], view:)
}
