// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
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
import lustre/fable/ui/layout
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
    inputs: List(fn(Controls) -> Element(Msg)),
    sequences: List(Sequence),
    options: List(Option(Msg)),
    view: fn(Controls) -> Element(Msg),
  )
}

pub type Controls {
  Controls(lookup: Dict(Int, Dynamic))
}

pub type Sequence {
  Sequence(
    ///
    key: Int,
    ///
    name: String,
    ///
    messages: List(Msg),
  )
}

///
///
pub opaque type Model {
  Model(
    lookup: Dict(Int, Dynamic),
    sequences: Dict(Int, List(Msg)),
    sequence: List(Msg),
    controls_visible: Bool,
    tab: Tab,
  )
}

pub opaque type Tab {
  TabControls
  TabSequences
}

///
///
pub type Msg {
  ComponentUpdatedValue(key: Int, value: Dynamic)
  ExternalStylesheetLoaded(Result(String, rsvp.Error))
  ParentSetControlsVisibility(controls_visible: Bool)
  UserChangedTab(tab: Tab)
  UserEditedValue(key: Int, value: Json)
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
    lustre.component(
      init: story_init(
        _,
        sequences: list.fold(
          config.sequences,
          dict.new(),
          fn(sequences, sequence) {
            dict.insert(sequences, sequence.key, sequence.messages)
          },
        ),
        should_show_controls: !list.is_empty(config.inputs),
      ),
      update: story_update,
      view: story_view(_, scene, config.inputs),
      options: [
        component.on_property_change(
          "controls",
          decode.bool |> decode.map(ParentSetControlsVisibility),
        ),
      ],
    ),
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

fn story_init(
  _,
  sequences sequences: Dict(Int, List(Msg)),
  should_show_controls controls_visible: Bool,
) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      lookup: dict.new(),
      sequences:,
      sequence: [],
      controls_visible:,
      tab: TabControls,
    )

  #(model, effect.none())
}

fn story_update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ComponentUpdatedValue(key:, value:) -> #(
      Model(..model, lookup: dict.insert(model.lookup, key, value)),
      effect.none(),
    )

    ExternalStylesheetLoaded(_) -> #(model, effect.none())

    ParentSetControlsVisibility(controls_visible:) -> #(
      Model(..model, controls_visible:),
      effect.none(),
    )

    UserChangedTab(tab:) -> #(Model(..model, tab:), effect.none())

    UserEditedValue(key:, value:) -> #(
      Model(
        ..model,
        lookup: dict.insert(
          model.lookup,
          key,
          value
            |> json.to_string
            |> json.parse(decode.dynamic)
            |> result.unwrap(dynamic.nil()),
        ),
      ),
      effect.none(),
    )
  }
}

fn story_view(
  model: Model,
  scene: String,
  inputs: List(fn(Controls) -> Element(Msg)),
) -> Element(Msg) {
  let handle_change = {
    use key <- decode.subfield(["detail", "key"], decode.int)
    use value <- decode.subfield(["detail", "value"], decode.dynamic)

    decode.success(ComponentUpdatedValue(key:, value:))
  }

  let attributes =
    dict.fold(model.lookup, [], fn(attributes, key, value) {
      [attribute.property(int.to_string(key), as_json(value)), ..attributes]
    })

  layout.story(
    scene: element.element(
      scene,
      [
        event.on("change", handle_change),
        attribute.class("rounded p-4 border-dashed border border-blue-500/0"),
        attribute.class("transition hover:border-blue-500/100"),
        ..attributes
      ],
      [],
    ),
    controls: case list.is_empty(inputs) && dict.is_empty(model.sequences) {
      True -> element.none()
      False ->
        element.fragment([
          story_view_toggle(model.tab),
          html.div(
            [attribute.class("flex flex-col overflow-y-auto")],
            case model.tab {
              TabControls ->
                list.map(inputs, fn(input) { input(Controls(model.lookup)) })

              TabSequences -> [
                html.p(
                  [attribute.class("flex-1 flex justify-center items-center")],
                  [
                    html.text(
                      "Sequences are currently unsupported in this pre-release.",
                    ),
                  ],
                ),
              ]
            },
          ),
        ])
    },
  )
}

fn story_view_toggle(active: Tab) -> Element(Msg) {
  let classes = fn(tab) {
    attribute.classes([
      #("flex-1 text-center px-3 py-1 rounded-sm cursor-pointer", True),
      #("hover:bg-stone-50", True),
      #("bg-white", tab == active),
    ])
  }

  html.div([attribute.class("flex gap-2 rounded bg-stone-100 p-2")], [
    html.button(
      [event.on_click(UserChangedTab(TabControls)), classes(TabControls)],
      [html.text("Controls")],
    ),
    html.button(
      [event.on_click(UserChangedTab(TabSequences)), classes(TabSequences)],
      [html.text("Sequences")],
    ),
  ])
}

// SCENE -----------------------------------------------------------------------

type SceneModel {
  SceneModel(
    lookup: Dict(Int, Dynamic),
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

    ParentSetControlsVisibility(..) -> #(model, effect.none())

    UserChangedTab(..) -> #(model, effect.none())

    UserEditedValue(key:, value:) -> #(
      model,
      event.emit(
        "change",
        json.object([#("key", json.int(key)), #("value", value)]),
      ),
    )
  }
}

fn scene_view(
  model: SceneModel,
  view: fn(Controls) -> Element(Msg),
) -> Element(Msg) {
  element.fragment(
    list.flatten([
      list.map(model.stylesheets, fn(css) {
        html.style([], string.replace(css, ":root", ":host"))
      }),
      [
        case model.pending_stylesheets {
          0 -> view(Controls(model.lookup))
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

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn as_json(val: Dynamic) -> Json
