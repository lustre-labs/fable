// IMPORTS ---------------------------------------------------------------------

import fable/internal/route.{type Route, Index, SceneDisplay, SceneSelect}
import fable/internal/story.{type Scene, type Story}
import gleam/dict.{type Dict}
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
      dict.insert(acc, justin.kebab_case(chapter.name), chapter)
    })
  })
}

pub fn chapter(name: String, stories: List(Story)) -> Chapter {
  Chapter(name:, stories: {
    list.fold(stories, dict.new(), fn(acc, story) {
      dict.insert(acc, story.slug, story)
    })
  })
}

pub fn app() -> App(Book, Model, Message) {
  lustre.application(init:, update:, view: fn(model) {
    element.fragment([html.style([], styles), view(model)])
  })
}

// QUERIES ---------------------------------------------------------------------

fn lookup_story(
  chapters: Dict(String, Chapter),
  chapter: String,
  story: String,
) -> Result(Story, Nil) {
  case dict.get(chapters, chapter) {
    Ok(chapter) -> dict.get(chapter.stories, story)
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
  let route = route.from_uri(here) |> result.unwrap(Index)

  let scene = case route {
    SceneDisplay(chapter:, story:, scene:) ->
      dict.get(book.chapters, chapter)
      |> result.try(fn(chapter) { dict.get(chapter.stories, story) })
      |> result.try(story.start(_, scene))
      |> option.from_result

    _ -> None
  }

  let model = Model(name: book.name, chapters: book.chapters, route:, scene:)

  let effect =
    modem.init(fn(request) {
      case route.from_uri(request) {
        Ok(route) -> UserClickedInternalLink(route:)
        Error(_) -> UserClickedExternalLink(to: request)
      }
    })

  #(model, effect)
}

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
        SceneDisplay(chapter:, story:, scene:) -> {
          let scene =
            dict.get(model.chapters, chapter)
            |> result.try(fn(chapter) { dict.get(chapter.stories, story) })
            |> result.try(story.start(_, scene))

          case scene {
            Ok(scene) -> {
              let model = Model(..model, route:, scene: Some(scene))

              #(model, effect.none())
            }

            Error(_) -> {
              let model = Model(..model, scene: None)
              let effect = route.push(SceneSelect(chapter:, story:))

              #(model, effect)
            }
          }
        }

        _ -> {
          let model = Model(..model, route:, scene: None)

          #(model, effect.none())
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
      SceneSelect(..) as route ->
        case lookup_story(model.chapters, route.chapter, route.story) {
          Ok(story) ->
            story.view(route.chapter, story, model.scene, story_handlers)

          Error(_) -> element.none()
        }

      SceneDisplay(..) as route ->
        case lookup_story(model.chapters, route.chapter, route.story) {
          Ok(story) ->
            story.view(route.chapter, story, model.scene, story_handlers)

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
    element.fragment({
      list.filter_map(keys, fn(key) {
        dict.get(chapters, key)
        |> result.map(view_sidebar_chapter(_, key))
      })
    }),
  ])
}

fn view_sidebar_chapter(chapter: Chapter, key: String) -> Element(Message) {
  let stories = dict.keys(chapter.stories) |> list.sort(string.compare)

  html.nav([], [
    html.h2([], [html.text(chapter.name)]),
    html.ul([], {
      list.filter_map(stories, fn(story_key) {
        use story <- result.map(dict.get(chapter.stories, story_key))

        html.li([], [
          html.a([route.href(SceneSelect(chapter: key, story: story_key))], [
            html.text(story.name),
          ]),
        ])
      })
    }),
  ])
}

// UTILS -----------------------------------------------------------------------

//cog:embed assets/styles.css
const styles = ""
