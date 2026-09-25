// IMPORTS ---------------------------------------------------------------------

import fable/internal/icon
import fable/internal/route
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import justin
import lustre/attribute
import lustre/dev/query
import lustre/dev/simulate.{
  type App, type Event, type Simulation, Dispatch, Event, Problem,
}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/portal
import pprint

// TYPES -----------------------------------------------------------------------

pub type Story {
  Story(
    name: String,
    slug: String,
    template: App(Dynamic, Dynamic, Dynamic),
    scenes: Dict(Int, SceneConfig(Dynamic, Dynamic, Dynamic)),
  )
}

pub opaque type Scene {
  Scene(
    key: Int,
    id: Int,
    name: String,
    step: Int,
    step_count: Int,
    arguments: Dynamic,
    simulation: Simulation(Dynamic, Dynamic),
    computed_dimensions: Option(#(Int, Int)),
  )
}

pub opaque type SceneConfig(arguments, model, message) {
  SceneConfig(
    name: String,
    arguments: arguments,
    default_step: Int,
    setup: fn(Simulation(model, message)) -> Simulation(model, message),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(
  name: String,
  template: App(arguments, model, message),
  scenes: List(SceneConfig(arguments, model, message)),
) -> Story {
  let slug = justin.kebab_case(name)

  Story(name:, slug:, template: coerce(template), scenes: {
    list.index_fold(scenes, dict.new(), fn(acc, config, id) {
      dict.insert(acc, id, coerce(config))
    })
  })
}

pub fn scene(
  name: String,
  arguments: arguments,
  simulation: fn(Simulation(model, message)) -> Simulation(model, message),
) -> SceneConfig(arguments, model, message) {
  SceneConfig(name:, arguments:, default_step: 0, setup: simulation)
}

// BUILDERS --------------------------------------------------------------------

pub fn with_default_step(
  scene: SceneConfig(arguments, model, message),
  step: Int,
) -> SceneConfig(arguments, model, message) {
  SceneConfig(..scene, default_step: step)
}

pub fn with_simulation(
  scene: SceneConfig(arguments, model, message),
  setup: fn(Simulation(model, message)) -> Simulation(model, message),
) -> SceneConfig(arguments, model, message) {
  SceneConfig(..scene, setup:)
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn start(story: Story, id: Int) -> Result(Scene, Nil) {
  use config <- result.map(dict.get(story.scenes, id))
  let simulation =
    simulate.start(story.template, config.arguments)
    |> config.setup
    |> simulate.restart

  let step_count = simulate.history(simulation) |> list.length
  let step = int.max(0, int.min(step_count, config.default_step))

  Scene(
    name: config.name,
    id:,
    key: 0,
    step:,
    step_count:,
    arguments: coerce(config.arguments),
    simulation:,
    computed_dimensions: None,
  )
}

pub fn restart(scene: Scene) -> Scene {
  Scene(
    ..scene,
    key: scene.key + 1,
    step: 0,
    simulation: simulate.restart(scene.simulation),
  )
}

pub fn jump(scene: Scene, steps: Int) -> Scene {
  let step = int.max(0, int.min(scene.step + steps, scene.step_count))

  Scene(..scene, step:, simulation: simulate.jump(scene.simulation, steps))
}

pub fn step_forward(scene: Scene) -> Scene {
  let step = int.min(scene.step + 1, scene.step_count)

  Scene(..scene, step:, simulation: simulate.step_forward(scene.simulation))
}

pub fn step_backward(scene: Scene) -> Scene {
  let step = int.max(scene.step - 1, 0)

  Scene(..scene, step:, simulation: simulate.step_back(scene.simulation))
}

pub fn resize(scene: Scene, width: Int, height: Int) -> Scene {
  Scene(..scene, computed_dimensions: Some(#(width, height)))
}

// EFFECTS ---------------------------------------------------------------------

pub fn inject_interesting_elements() -> Effect(message) {
  use _, root <- effect.before_paint
  do_inject_interesting_elements(root)
}

@external(javascript, "./story.ffi.mjs", "injectInterestingElements")
fn do_inject_interesting_elements(shadow_root: Dynamic) -> Nil

// VIEWS -----------------------------------------------------------------------

pub type Handlers(message) {
  Handlers(
    on_scene_message: message,
    on_restart: message,
    on_jump: fn(Int) -> message,
    on_step_backward: message,
    on_step_forward: message,
  )
}

pub fn view(
  chapter: String,
  story: Story,
  scene: Option(Scene),
  handlers: Handlers(message),
) -> Element(message) {
  case scene {
    Some(scene) -> {
      element.fragment([
        view_story_sidebar(chapter, story, Some(scene), handlers),
        view_scene(scene, handlers),
      ])
    }

    None ->
      element.fragment([
        view_story_sidebar(chapter, story, None, handlers),
      ])
  }
}

fn view_story_sidebar(
  chapter: String,
  story: Story,
  scene: Option(Scene),
  handlers: Handlers(message),
) -> Element(message) {
  html.section([attribute.class("story-sidebar")], [
    view_scene_select(chapter, story, scene),
    option.map(scene, view_scene_history(_, handlers))
      |> option.lazy_unwrap(element.none),
    option.map(scene, view_scene_model) |> option.lazy_unwrap(element.none),
  ])
}

fn view_scene_select(
  chapter: String,
  story: Story,
  scene: Option(Scene),
) -> Element(message) {
  let keys = dict.keys(story.scenes) |> list.sort(int.compare)

  html.div([], [
    html.h4([], [html.text("Scenes")]),
    html.ul([], {
      use id <- list.filter_map(keys)
      let is_active = case scene {
        Some(s) -> s.id == id
        None -> False
      }

      use scene <- result.map(dict.get(story.scenes, id))
      let route = route.SceneDisplay(chapter:, story: story.slug, scene: id)

      html.li([], [
        html.a(
          [route.href(route), attribute.classes([#("active", is_active)])],
          [html.text(scene.name)],
        ),
      ])
    }),
  ])
}

fn view_scene_history(
  scene: Scene,
  handlers: Handlers(message),
) -> Element(message) {
  let events = simulate.history(scene.simulation)
  use <- bool.lazy_guard(list.is_empty(events), element.none)

  html.div([attribute.class("history")], [
    html.h4([], [html.text("History")]),
    html.ul([], [
      view_scene_init_event(scene, handlers),
      element.fragment(
        list.index_map(events, fn(event, index) {
          view_scene_event(scene, event, index + 1, handlers)
        }),
      ),
    ]),
  ])
}

fn view_scene_init_event(
  scene: Scene,
  handlers: Handlers(message),
) -> Element(message) {
  let is_active = scene.step == 0
  let class_list = [
    #("event", True),
    #("active", is_active),
  ]

  html.li([attribute.classes(class_list)], [
    html.button([event.on_click(handlers.on_jump(-scene.step))], [
      html.p([], [html.text("Init")]),
      html.pre([], [html.text(string.inspect(scene.arguments))]),
    ]),
  ])
}

fn view_scene_event(
  scene: Scene,
  event: Event(_),
  step: Int,
  handlers: Handlers(message),
) -> Element(message) {
  let is_active = scene.step == step
  let class_list = [
    #("event", True),
    #("active", is_active),
  ]

  html.li([attribute.classes(class_list)], [
    html.button(
      [event.on_click(handlers.on_jump(step - scene.step))],
      case event {
        Dispatch(..) -> [
          html.p([], [html.text("Dispatch")]),
        ]

        Event(target:, ..) -> [
          html.p([], [html.text("Event")]),
          html.p([], [html.text(query.to_readable_string(target))]),
        ]

        Problem(..) -> [
          html.p([], [html.text("Problem")]),
        ]
      },
    ),
  ])
}

fn view_scene_model(scene: Scene) -> Element(message) {
  html.div([attribute.class("model")], [
    html.h4([], [html.text("Model")]),
    html.pre([], [
      scene.simulation
      |> simulate.model
      |> pprint.with_config(pprint.Config(
        pprint.Unstyled,
        pprint.BitArraysAsString,
        pprint.Labels,
      ))
      |> html.text,
    ]),
  ])
}

fn view_scene(scene: Scene, handlers: Handlers(message)) -> Element(message) {
  html.main([attribute.class("scene")], [
    html.div([attribute.class("controls")], [
      html.button([event.on_click(handlers.on_restart)], [
        icon.refresh([]),
      ]),

      html.button([event.on_click(handlers.on_jump(-scene.step))], [
        icon.chevron_double_left([]),
      ]),

      html.button([event.on_click(handlers.on_step_backward)], [
        icon.chevron_left([]),
      ]),

      html.p([attribute.class("step-count")], [
        html.text(int.to_string(scene.step)),
        html.text(" / "),
        html.text(int.to_string(scene.step_count)),
      ]),

      html.button([event.on_click(handlers.on_step_forward)], [
        icon.chevron_right([]),
      ]),

      html.button(
        [event.on_click(handlers.on_jump(scene.step_count - scene.step))],
        [icon.chevron_double_right([])],
      ),
    ]),

    keyed.div([attribute.class("inner")], [
      #(
        scene.name <> int.to_string(scene.key),
        view_scene_renderer(scene, handlers.on_scene_message),
      ),
    ]),
  ])
}

fn view_scene_renderer(
  scene: Scene,
  handle_scene_message: message,
) -> Element(message) {
  element.fragment([
    html.iframe([
      attribute.class("scene-renderer"),

      scene.computed_dimensions
        |> option.map(pair.first)
        |> option.map(fn(width) {
          attribute.style("width", int.to_string(width) <> "px")
        })
        |> option.lazy_unwrap(attribute.none),

      scene.computed_dimensions
        |> option.map(pair.second)
        |> option.map(fn(height) {
          attribute.style("height", int.to_string(height) <> "px")
        })
        |> option.lazy_unwrap(attribute.none),
    ]),

    portal.to("iframe", [portal.root(portal.Relative)], [
      simulate.view(scene.simulation)
      |> element.map(fn(_) { handle_scene_message }),
    ]),
  ])
}

// UTILS -----------------------------------------------------------------------

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(any: a) -> b
