// IMPORTS ---------------------------------------------------------------------

import fable/internal/icon
import fable/internal/route
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import justin
import lustre/attribute
import lustre/dev/query
import lustre/dev/simulate.{type App, type Simulation, Dispatch, Event, Problem}
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
  story: Story,
  chapter_name: String,
  scene: Option(Scene),
  handlers: Handlers(message),
) -> Element(message) {
  element.fragment([
    view_story_sidebar(chapter_name, story, scene, handlers),

    case scene {
      Some(scene) -> {
        view_scene(scene, story, chapter_name, handlers)
      }
      None -> element.none()
    },
  ])
}

fn view_story_sidebar(
  chapter_name: String,
  story: Story,
  scene: Option(Scene),
  handlers: Handlers(message),
) -> Element(message) {
  html.aside([attribute.class("story-sidebar")], [
    view_scene_select(chapter_name, story, scene),

    case scene {
      Some(scene) ->
        element.fragment([
          view_scene_history(scene, handlers),
          view_scene_model(scene),
        ])

      None -> element.none()
    },
  ])
}

fn view_scene_select(
  chapter_name: String,
  story: Story,
  scene: Option(Scene),
) -> Element(message) {
  html.div([attribute.class("scene-select")], [
    html.h2([], [html.text("Scenes")]),
    html.ul([], {
      let keys = dict.keys(story.scenes) |> list.sort(int.compare)
      use id <- list.filter_map(keys)

      let is_active = case scene {
        Some(s) -> s.id == id
        None -> False
      }

      use scene <- result.map(dict.get(story.scenes, id))
      let route =
        route.SceneDisplay(
          chapter: justin.kebab_case(chapter_name),
          story: story.slug,
          scene: id,
        )

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

  let entries = [
    #("Init", Some(string.inspect(scene.arguments))),
    ..list.map(events, fn(event) {
      case event {
        Dispatch(..) -> #("Dispatch", None)
        Event(target:, ..) -> #("Event", Some(query.to_readable_string(target)))
        Problem(..) -> #("Problem", None)
      }
    })
  ]

  html.div([attribute.class("history")], [
    html.h2([], [html.text("History")]),
    html.ul([], {
      use #(title, detail), step <- list.index_map(entries)

      html.li([attribute.classes([#("active", scene.step == step)])], [
        html.button([event.on_click(handlers.on_jump(step - scene.step))], [
          html.p([], [html.text(title)]),
          case detail {
            Some(detail) -> html.pre([], [html.text(detail)])
            None -> element.none()
          },
        ]),
      ])
    }),
  ])
}

fn view_scene_model(scene: Scene) -> Element(message) {
  html.div([attribute.class("model")], [
    html.h2([], [html.text("Model")]),
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

fn view_scene(
  scene: Scene,
  story: Story,
  chapter_name: String,
  handlers: Handlers(message),
) -> Element(message) {
  html.main([attribute.class("scene")], [
    html.header([], [
      html.h2([], [
        html.small([], [
          html.text(chapter_name <> " › " <> story.name),
        ]),
        html.text(scene.name),
      ]),
      case scene.step_count {
        0 | 1 -> element.none()
        _ -> view_scene_controls(scene, handlers)
      },
    ]),
    keyed.div([attribute.class("inner")], [
      #(
        scene.name <> "/" <> int.to_string(scene.key),
        view_scene_renderer(scene, handlers.on_scene_message),
      ),
    ]),
  ])
}

fn view_scene_controls(
  scene: Scene,
  handlers: Handlers(message),
) -> Element(message) {
  html.div([attribute.class("controls")], [
    html.button(
      [
        attribute.aria_label("Restart scene"),
        event.on_click(handlers.on_restart),
      ],
      [icon.refresh([])],
    ),

    html.button(
      [
        attribute.aria_label("Jump to start"),
        event.on_click(handlers.on_jump(-scene.step)),
      ],
      [icon.chevron_double_left([])],
    ),

    html.button(
      [
        attribute.aria_label("Previous step"),
        event.on_click(handlers.on_step_backward),
      ],
      [icon.chevron_left([])],
    ),

    html.p([attribute.class("step-count")], [
      html.text(int.to_string(scene.step)),
      html.text("/"),
      html.text(int.to_string(scene.step_count)),
    ]),

    html.button(
      [
        attribute.class("next"),
        attribute.aria_label("Next step"),
        event.on_click(handlers.on_step_forward),
      ],
      [icon.chevron_right([])],
    ),

    html.button(
      [
        attribute.aria_label("Jump to end"),
        event.on_click(handlers.on_jump(scene.step_count - scene.step)),
      ],
      [icon.chevron_double_right([])],
    ),
  ])
}

fn view_scene_renderer(
  scene: Scene,
  handle_scene_message: message,
) -> Element(message) {
  let dimension_styles = case scene.computed_dimensions {
    Some(#(width, height)) -> [
      attribute.style("width", int.to_string(width) <> "px"),
      attribute.style("height", int.to_string(height) <> "px"),
    ]

    None -> []
  }

  element.fragment([
    html.iframe([attribute.class("scene-renderer"), ..dimension_styles]),

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
