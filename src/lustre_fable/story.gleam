// IMPORTS ---------------------------------------------------------------------

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
import rsvp

// TYPES -----------------------------------------------------------------------

pub type Story {
  Story(
    ///
    title: String,
    ///
    route: String,
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
    title: String,
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
  ExternalStylesheetLoaded(Result(String, rsvp.Error))
  ComponentUpdatedValue(key: Int, value: Value)
  UserEditedValue(key: Int, value: Value)
}

//

///
///
pub fn register(
  config: StoryConfig,
  stylesheets: List(String),
  external_stylesheets: List(String),
) -> Result(Story, lustre.Error) {
  let base = base_tag(config.title)
  let component = base <> "-story"
  let scene = base <> "-scene"

  use _ <- result.try(lustre.register(
    lustre.simple(init: story_init, update: story_update, view: {
      story_view(_, scene, config.inputs)
    }),
    component,
  ))

  use _ <- result.try(lustre.register(
    lustre.component(
      init: scene_init(_, stylesheets, external_stylesheets),
      update: scene_update,
      options: config.options,
      view: scene_view(_, config.view),
    ),
    scene,
  ))

  Ok(Story(title: config.title, route: base, component:, scene:))
}

// STORY -----------------------------------------------------------------------

fn story_init(_) -> Model {
  Model(lookup: dict.new())
}

fn story_update(model: Model, msg: Msg) -> Model {
  case msg {
    ExternalStylesheetLoaded(_) -> model

    ComponentUpdatedValue(key:, value:) ->
      Model(lookup: dict.insert(model.lookup, key, value))

    UserEditedValue(key:, value:) ->
      Model(lookup: dict.insert(model.lookup, key, value))
  }
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

  html.div([attribute.class("@container h-full")], [
    html.div(
      [
        attribute.class("h-full grid grid-cols-1 grid-rows-[1fr_300px]"),
        attribute.class("@3xl:grid-cols-[1fr_300px] @3xl:grid-rows-1"),
      ],
      [
        html.main(
          [
            attribute.class(
              "p-4 grid grid-cols-1 grid-rows-1 place-items-center",
            ),
          ],
          [
            element.element(
              scene,
              [event.on("change", handle_change), ..attributes],
              [],
            ),
          ],
        ),
        html.aside(
          [
            attribute.class("p-4 border-t"),
            attribute.class("@3xl:border-t-0 @3xl:border-l"),
          ],
          list.map(inputs, fn(input) { input(model) }),
        ),
      ],
    ),
  ])
}

// SCENE -----------------------------------------------------------------------

type SceneModel {
  SceneModel(
    lookup: Dict(Int, Value),
    stylesheets: List(String),
    pending_stylesheets: Int,
  )
}

fn scene_init(
  _,
  stylesheets: List(String),
  external_stylesheets: List(String),
) -> #(SceneModel, Effect(Msg)) {
  let model =
    SceneModel(
      lookup: dict.new(),
      stylesheets: stylesheets,
      pending_stylesheets: list.length(external_stylesheets),
    )

  let effect =
    external_stylesheets
    |> list.map(rsvp.get(_, rsvp.expect_text(ExternalStylesheetLoaded)))
    |> effect.batch

  #(model, effect)
}

fn scene_update(model: SceneModel, msg: Msg) -> #(SceneModel, Effect(Msg)) {
  case msg {
    ComponentUpdatedValue(key:, value:) -> #(
      SceneModel(..model, lookup: dict.insert(model.lookup, key, value)),
      effect.none(),
    )

    ExternalStylesheetLoaded(Ok(css)) -> {
      let model =
        SceneModel(
          ..model,
          stylesheets: [css, ..model.stylesheets],
          pending_stylesheets: model.pending_stylesheets - 1,
        )

      #(model, effect.none())
    }

    ExternalStylesheetLoaded(Error(_)) -> {
      let model =
        SceneModel(..model, pending_stylesheets: model.pending_stylesheets - 1)

      #(model, effect.none())
    }

    UserEditedValue(key:, value:) -> #(
      model,
      event.emit(
        "change",
        json.object([#("key", json.int(key)), #("value", value.to_json(value))]),
      ),
    )
  }
}

fn scene_view(
  model: SceneModel,
  view: fn(Model) -> Element(Msg),
) -> Element(Msg) {
  element.fragment(
    list.flatten([
      list.map(model.stylesheets, fn(css) {
        html.style([], string.replace(css, ":root", ":host"))
      }),
      [
        case model.pending_stylesheets {
          0 -> view(Model(model.lookup))
          _ -> element.none()
        },
        // These are all the CSS properties that can inherit and therefore will
        // pierce the shadow DOM. By setting them to `revert` they will be reset
        // back to the *user agent* default (which is not the same as using `initial`
        // which is the *spec* default...)
        //
        html.style(
          [],
          "
          :host {
            border-collapse: revert;
            border-spacing: revert;
            caption-side: revert;
            color: revert;
            cursor: revert;
            direction: revert;
            empty-cells: revert;
            font-family: revert;
            font-size: revert;
            font-style: revert;
            font-variant: revert;
            font-weight: revert;
            font-size-adjust: revert;
            font-stretch: revert;
            font: revert;
            letter-spacing: revert;
            line-height: revert;
            list-style-image: revert;
            list-style-position: revert;
            list-style-type: revert;
            list-style: revert;
            orphans: revert;
            quotes: revert;
            tab-size: revert;
            text-align: revert;
            text-align-last: revert;
            text-decoration-color: revert;
            text-indent: revert;
            text-justify: revert;
            text-shadow: revert;
            text-transform: revert;
            visibility: revert;
            white-space: revert;
            widows: revert;
            word-break: revert;
            word-spacing: revert;
            word-wrap: revert;
          }
          ",
        ),
      ],
    ]),
  )
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
