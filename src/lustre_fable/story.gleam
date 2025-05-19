//

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/component.{type Option}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre_fable/value.{type Value}

// TYPES -----------------------------------------------------------------------

pub type Story {
  Story(
    ///
    title: String,
    ///
    component: String,
    ///
    scene: String,
  )
}

///
///
pub type StoryBuilder {
  StoryBuilder(run: fn(Int) -> StoryConfig)
}

///
///
pub type StoryConfig {
  StoryConfig(
    inputs: List(fn(Model) -> Element(Msg)),
    options: List(Option(Msg)),
    view: fn(Model) -> Element(Msg),
  )
}

///
///
pub type Model {
  Model(lookup: Dict(Int, Value))
}

///
///
pub type Msg {
  ComponentUpdatedValue(key: Int, value: Value)
  UserEditedValue(key: Int, value: Value)
}

//

///
///
pub fn register(
  title: String,
  inputs: List(fn(Model) -> Element(Msg)),
  options: List(Option(Msg)),
  view: fn(Model) -> Element(Msg),
) -> Result(Story, lustre.Error) {
  let base = base_tag(title)
  let component = base <> "-story"
  let scene = base <> "-scene"

  use _ <- result.try(lustre.register(
    lustre.simple(init: story_init, update: story_update, view: {
      story_view(_, base <> "-scene", inputs)
    }),
    component,
  ))

  use _ <- result.try(lustre.register(
    lustre.component(init: scene_init, update: scene_update, options:, view:),
    scene,
  ))

  Ok(Story(title:, component:, scene:))
}

// STORY -----------------------------------------------------------------------

fn story_init(_) -> Model {
  Model(lookup: dict.new())
}

fn story_update(model: Model, msg: Msg) -> Model {
  Model(lookup: dict.insert(model.lookup, msg.key, msg.value))
}

fn story_view(
  model: Model,
  scene: String,
  inputs: List(fn(Model) -> Element(Msg)),
) -> Element(Msg) {
  let handle_change = {
    use key <- decode.subfield(["detail", "key"], decode.int)
    use value <- decode.subfield(["detail", "value"], value.decoder())

    decode.success(ComponentUpdatedValue(key:, value:))
  }

  let attributes =
    dict.fold(model.lookup, [], fn(attributes, key, value) {
      [
        attribute.property(int.to_string(key), value.to_json(value)),
        ..attributes
      ]
    })

  element.fragment([
    html.main([], [
      element.element(
        scene,
        [event.on("change", handle_change), ..attributes],
        [],
      ),
    ]),
    html.aside([], list.map(inputs, fn(input) { input(model) })),
    html.style(
      [],
      "
      :host {
        background-color: #f0f0f0;
      }
      ",
    ),
  ])
}

// SCENE -----------------------------------------------------------------------

fn scene_init(_) -> #(Model, Effect(Msg)) {
  #(Model(lookup: dict.new()), effect.none())
}

fn scene_update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ComponentUpdatedValue(key:, value:) -> #(
      Model(dict.insert(model.lookup, key, value)),
      effect.none(),
    )

    UserEditedValue(key:, value:) -> #(
      model,
      event.emit(
        "change",
        json.object([#("key", json.int(key)), #("value", value.to_json(value))]),
      ),
    )
  }
}

// UTILS -----------------------------------------------------------------------

/// Take the title of a story and return a guaranteed-valid tag name to use for
/// the story component. Only alphanumeric characters are preserved, with anything
/// else being replaced by a dash.
///
/// To make sure the tag is valid, we check if the tag is already used in cases
/// where the user has used the same name for more than one story. In those instances
/// an incrementing number is appended to the name.
///
fn base_tag(title: String) -> String {
  let assert Ok(re) = regexp.from_string("[^a-zA-Z0-9]+")
  let safe_component_name =
    title
    |> string.lowercase
    |> regexp.replace(re, _, "-")

  case lustre.is_registered(safe_component_name) {
    True -> do_base_tag(safe_component_name, 1)
    False -> safe_component_name
  }
}

fn do_base_tag(base: String, count: Int) -> String {
  case lustre.is_registered(base <> "-" <> int.to_string(count)) {
    True -> do_base_tag(base, count + 1)
    False -> base <> "-" <> int.to_string(count)
  }
}
