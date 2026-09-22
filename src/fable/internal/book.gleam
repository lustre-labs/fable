// IMPORTS ---------------------------------------------------------------------

import fable/internal/route.{type Route, Index, SceneDisplay, SceneSelect}
import fable/internal/story.{type Scene, type Story}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import justin
import lustre.{type App}
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem

// TYPES -----------------------------------------------------------------------

pub opaque type Book {
  Book(name: String, chapters: Dict(String, Chapter))
}

pub opaque type Chapter {
  Chapter(name: String, stories: Dict(String, Story))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(name: String, chapters: List(Chapter)) -> Book {
  Book(name:, chapters: {
    list.fold(chapters, dict.new(), fn(acc, chapter) {
      use <- bool.guard(dict.size(chapter.stories) == 0, acc)

      dict.insert(acc, justin.kebab_case(chapter.name), chapter)
    })
  })
}

pub fn chapter(name: String, stories: List(Story)) -> Chapter {
  Chapter(name:, stories: {
    list.fold(stories, dict.new(), fn(acc, story) {
      use <- bool.guard(dict.size(story.scenes) == 0, acc)

      dict.insert(acc, story.slug, story)
    })
  })
}

pub fn app() -> App(Book, Model, Message) {
  lustre.application(init:, update:, view:)
}

// QUERIES ---------------------------------------------------------------------

fn find_story(
  chapters: Dict(String, Chapter),
  chapter: String,
  story: String,
) -> Result(Story, Nil) {
  case dict.get(chapters, chapter) {
    Ok(chapter) -> dict.get(chapter.stories, story)
    Error(_) -> Error(Nil)
  }
}

fn find_scene(
  chapters: Dict(String, Chapter),
  chapter: String,
  story: String,
  scene: Int,
) -> Result(Scene, Nil) {
  case find_story(chapters, chapter, story) {
    Ok(story) -> story.start(story, scene)
    Error(_) -> Error(Nil)
  }
}

// MODEL -----------------------------------------------------------------------

pub opaque type Model {
  Model(
    name: String,
    chapters: Dict(String, Chapter),
    route: Route,
    scene: Option(Scene),
  )
}

fn init(book: Book) -> #(Model, Effect(Message)) {
  let assert Ok(here) = modem.initial_uri()

  let route = case route.from_uri(here) {
    Ok(SceneSelect(chapter:, story:) as route) -> {
      let story =
        book.chapters
        |> dict.get(chapter)
        |> result.try(fn(chapter) { dict.get(chapter.stories, story) })

      let scenes =
        story
        |> result.map(fn(story) { dict.size(story.scenes) })
        |> result.unwrap(0)

      case story {
        Ok(story) if scenes == 1 ->
          SceneDisplay(chapter:, story: story.slug, scene: 0)

        Ok(_) | Error(_) -> route
      }
    }

    Ok(route) -> route

    Error(_) -> Index
  }

  let scene = case route {
    SceneDisplay(chapter:, story:, scene:) ->
      book.chapters
      |> dict.get(chapter)
      |> result.try(fn(chapter) { dict.get(chapter.stories, story) })
      |> result.try(story.start(_, scene))
      |> option.from_result

    _ -> None
  }

  let model = Model(name: book.name, chapters: book.chapters, route:, scene:)

  let effect =
    effect.batch([
      case scene {
        Some(_) -> story.inject_interesting_elements()
        None -> effect.none()
      },

      init_router(),
    ])

  #(model, effect)
}

fn init_router() -> Effect(Message) {
  use dispatch, shadow_root <- effect.before_paint
  use request <- do_init_router(shadow_root)
  let message = case route.from_uri(request) {
    Ok(route) -> UserClickedInternalLink(route:)
    Error(_) -> UserClickedExternalLink(to: request)
  }

  dispatch(message)
}

@external(javascript, "./book.ffi.mjs", "initRouter")
fn do_init_router(root: Dynamic, dispatch: fn(Uri) -> Nil) -> Nil

// UPDATE ----------------------------------------------------------------------

pub opaque type Message {
  SceneProducedDiscardableMessage
  UserClickedExternalLink(to: Uri)
  UserClickedInternalLink(route: Route)
  UserClickedJump(steps: Int)
  UserClickedRestart
  UserClickedStepBackward
  UserClickedStepForward
}

fn update(model: Model, message: Message) -> #(Model, Effect(Message)) {
  case message {
    SceneProducedDiscardableMessage -> #(model, effect.none())

    UserClickedExternalLink(to: uri) -> #(model, modem.load(uri))

    UserClickedInternalLink(route:) ->
      case route {
        Index -> #(Model(..model, route:, scene: None), effect.none())

        SceneSelect(chapter:, story:) -> {
          let story = find_story(model.chapters, chapter, story)
          let scenes =
            story
            |> result.map(fn(story) { dict.size(story.scenes) })
            |> result.unwrap(0)

          case story {
            Ok(story) if scenes == 1 -> #(
              Model(..model, route:, scene: None),
              route.push(SceneDisplay(chapter:, story: story.slug, scene: 0)),
            )

            Ok(_) | Error(_) -> #(
              Model(..model, route:, scene: None),
              effect.none(),
            )
          }
        }

        SceneDisplay(chapter:, story:, scene:) ->
          case find_scene(model.chapters, chapter, story, scene) {
            Ok(scene) -> {
              let model = Model(..model, route:, scene: Some(scene))
              let effect = story.inject_interesting_elements()

              #(model, effect)
            }

            Error(_) -> {
              let model = Model(..model, scene: None)
              let effect = route.push(SceneSelect(chapter:, story:))

              #(model, effect)
            }
          }
      }

    UserClickedJump(steps:) -> {
      let scene = model.scene |> option.map(story.jump(_, steps))
      let model = Model(..model, scene:)

      #(model, effect.none())
    }

    UserClickedRestart -> {
      let scene = model.scene |> option.map(story.restart)
      let model = Model(..model, scene:)

      #(model, effect.none())
    }

    UserClickedStepBackward -> {
      let scene = model.scene |> option.map(story.step_backward)
      let model = Model(..model, scene:)

      #(model, effect.none())
    }

    UserClickedStepForward -> {
      let scene = model.scene |> option.map(story.step_forward)
      let model = Model(..model, scene:)

      #(model, effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

const story_handlers = story.Handlers(
  on_scene_message: SceneProducedDiscardableMessage,
  on_restart: UserClickedRestart,
  on_jump: UserClickedJump,
  on_step_backward: UserClickedStepBackward,
  on_step_forward: UserClickedStepForward,
)

fn view(model: Model) -> Element(Message) {
  element.fragment([
    view_sidebar(model.name, model.chapters),

    case model.route {
      SceneSelect(chapter:, story:) | SceneDisplay(chapter:, story:, ..) ->
        case find_story(model.chapters, chapter, story) {
          Ok(story) -> story.view(chapter, story, model.scene, story_handlers)

          Error(_) -> element.none()
        }

      _ -> element.none()
    },
  ])
}

fn view_sidebar(
  name: String,
  chapters: Dict(String, Chapter),
) -> Element(Message) {
  use <- element.memo([element.ref(chapters)])
  let keys = dict.keys(chapters) |> list.sort(string.compare)

  html.section([attribute.class("sidebar")], [
    html.h1([], [html.text(name)]),
    element.fragment(list.filter_map(keys, view_sidebar_chapter(_, chapters))),
  ])
}

fn view_sidebar_chapter(
  key: String,
  chapters: Dict(String, Chapter),
) -> Result(Element(Message), Nil) {
  use chapter <- result.map(dict.get(chapters, key))
  let keys = dict.keys(chapter.stories) |> list.sort(string.compare)

  html.nav([], [
    html.h2([], [html.text(chapter.name)]),
    html.ul([], list.filter_map(keys, view_sidebar_story(_, key, chapter))),
  ])
}

fn view_sidebar_story(
  story: String,
  key: String,
  chapter: Chapter,
) -> Result(Element(Message), Nil) {
  use story <- result.map(dict.get(chapter.stories, story))

  html.li([], [
    html.a([route.href(SceneSelect(chapter: key, story: story.slug))], [
      html.text(story.name),
    ]),
  ])
}
