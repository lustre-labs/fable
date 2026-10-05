// IMPORTS ---------------------------------------------------------------------

import fable/internal/dom
import fable/internal/fuzzy
import fable/internal/route.{type Route, Index, SceneDisplay, SceneSelect}
import fable/internal/story.{type Scene, type Story}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri.{type Uri}
import justin
import lustre.{type App}
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import modem

// TYPES -----------------------------------------------------------------------

pub opaque type Book {
  Book(name: String, order: List(String), chapters: Dict(String, Chapter))
}

pub opaque type Chapter {
  Chapter(name: String, order: List(String), stories: Dict(String, Story))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(name: String, chapters: List(Chapter)) -> Book {
  let order =
    list.map(chapters, fn(chapter) { justin.kebab_case(chapter.name) })

  let chapters =
    list.fold(chapters, dict.new(), fn(acc, chapter) {
      use <- bool.guard(dict.is_empty(chapter.stories), acc)

      dict.insert(acc, justin.kebab_case(chapter.name), chapter)
    })

  Book(name:, order:, chapters:)
}

pub fn chapter(name: String, stories: List(Story)) -> Chapter {
  let order = list.map(stories, fn(story) { story.slug })
  let stories =
    list.fold(stories, dict.new(), fn(acc, story) {
      use <- bool.guard(dict.is_empty(story.scenes), acc)

      dict.insert(acc, story.slug, story)
    })

  Chapter(name:, order:, stories:)
}

pub fn app() -> App(Book, Model, Message) {
  lustre.application(init:, update:, view:)
}

// QUERIES ---------------------------------------------------------------------

fn find_chapter(
  chapters: Dict(String, Chapter),
  chapter: String,
) -> Result(Chapter, Nil) {
  dict.get(chapters, chapter)
}

fn find_story(
  chapters: Dict(String, Chapter),
  chapter: String,
  story: String,
) -> Result(#(Chapter, Story), Nil) {
  use chapter <- result.try(find_chapter(chapters, chapter))
  use story <- result.try(dict.get(chapter.stories, story))
  Ok(#(chapter, story))
}

fn find_scene(
  chapters: Dict(String, Chapter),
  chapter: String,
  story: String,
  scene: Int,
) -> Result(Scene, Nil) {
  use #(_, story) <- result.try(find_story(chapters, chapter, story))
  story.start(story, scene)
}

fn matching_stories(chapter: Chapter, search: String) -> List(Story) {
  use slug <- list.filter_map(chapter.order)
  use story <- result.try(dict.get(chapter.stories, slug))

  case fuzzy.matches(story.name, search) {
    True -> Ok(story)
    False -> Error(Nil)
  }
}

// MODEL -----------------------------------------------------------------------

pub opaque type Model {
  Model(
    name: String,
    order: List(String),
    chapters: Dict(String, Chapter),
    route: Route,
    scene: Option(Scene),
    search: String,
  )
}

fn init(book: Book) -> #(Model, Effect(Message)) {
  let assert Ok(here) = modem.initial_uri()

  let route = case route.from_uri(here) {
    Ok(SceneSelect(chapter:, story:) as route) -> {
      case find_story(book.chapters, chapter, story) {
        Ok(#(_chapter, story)) -> {
          case dict.size(story.scenes) {
            1 -> SceneDisplay(chapter:, story: story.slug, scene: 0)
            _ -> route
          }
        }

        Error(_) -> route
      }
    }

    Ok(route) -> route

    Error(_) -> Index
  }

  let scene = case route {
    SceneDisplay(chapter:, story:, scene:) ->
      find_scene(book.chapters, chapter, story, scene)
      |> option.from_result

    _ -> None
  }

  let model =
    Model(
      name: book.name,
      order: book.order,
      chapters: book.chapters,
      route:,
      scene:,
      search: "",
    )

  let effect =
    effect.batch([
      case scene {
        Some(_) -> story.inject_interesting_elements()
        None -> effect.none()
      },

      init_router(),
      dom.add_global_event_listener("keydown", search_shortcut()),
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
fn do_init_router(_root: Dynamic, _dispatch: fn(Uri) -> Nil) -> Nil {
  Nil
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Message {
  SceneProducedDiscardableMessage
  UserChangedSearch(String)
  UserPressedSearchShortcut
  UserSubmittedSearch
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

    UserChangedSearch(search) -> #(Model(..model, search:), effect.none())

    UserPressedSearchShortcut -> #(model, dom.focus("story-search"))

    UserSubmittedSearch -> {
      let first_match = {
        use slug <- list.find_map(model.order)
        use chapter <- result.try(dict.get(model.chapters, slug))
        use story <- result.try(
          list.first(matching_stories(chapter, model.search)),
        )
        Ok(SceneDisplay(chapter: slug, story: story.slug, scene: 0))
      }

      case first_match {
        Ok(route) if route != model.route -> #(model, route.push(route))
        _ -> #(model, effect.none())
      }
    }

    UserClickedExternalLink(to: uri) -> #(model, modem.load(uri))

    UserClickedInternalLink(route:) -> {
      use <- bool.guard(route == model.route, #(model, effect.none()))

      case route {
        Index -> #(Model(..model, route:, scene: None), effect.none())

        SceneSelect(chapter:, story:) ->
          case find_story(model.chapters, chapter, story) {
            Ok(#(_chapter, story)) -> #(
              Model(..model, route:, scene: None),
              route.push(SceneDisplay(chapter:, story: story.slug, scene: 0)),
            )

            Error(_) -> #(Model(..model, route:, scene: None), effect.none())
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

fn search_shortcut() -> Decoder(event.Handler(Message)) {
  use key <- decode.field("key", decode.string)
  use ctrl <- decode.field("ctrlKey", decode.bool)
  use meta <- decode.field("metaKey", decode.bool)

  let handler = event.handler(UserPressedSearchShortcut, True, False)

  case key == "k" && { ctrl || meta } {
    True -> decode.success(handler)
    False -> decode.failure(handler, "search shortcut")
  }
}

fn submit_search() -> Decoder(event.Handler(Message)) {
  use key <- decode.field("key", decode.string)

  let handler = event.handler(UserSubmittedSearch, True, False)

  case key == "Enter" {
    True -> decode.success(handler)
    False -> decode.failure(handler, "search submission")
  }
}

const story_handlers = story.Handlers(
  on_scene_message: SceneProducedDiscardableMessage,
  on_restart: UserClickedRestart,
  on_jump: UserClickedJump,
  on_step_backward: UserClickedStepBackward,
  on_step_forward: UserClickedStepForward,
)

fn view(model: Model) -> Element(Message) {
  element.fragment([
    view_sidebar(
      model.name,
      model.route,
      model.order,
      model.chapters,
      model.search,
    ),

    case model.route {
      SceneSelect(chapter:, story:) | SceneDisplay(chapter:, story:, ..) -> {
        case find_story(model.chapters, chapter, story) {
          Ok(#(chapter, story)) ->
            story.view(story, chapter.name, model.scene, story_handlers)

          Error(_) -> element.none()
        }
      }

      _ -> element.none()
    },
  ])
}

fn view_sidebar(
  name: String,
  route: Route,
  order: List(String),
  chapters: Dict(String, Chapter),
  search: String,
) -> Element(Message) {
  html.aside([attribute.class("sidebar")], [
    html.h1([], [html.text(name)]),
    html.label([attribute.class("search")], [
      html.input([
        attribute.id("story-search"),
        attribute.type_("search"),
        attribute.aria_label("Search or filter stories"),
        attribute.aria_keyshortcuts("Control+k Meta+k"),
        attribute.placeholder("Search"),
        attribute.value(search),
        event.on_input(UserChangedSearch),
        event.advanced("keydown", submit_search()),
      ]),
      html.kbd([attribute.aria_hidden(True)], [
        html.text("⌘ K"),
      ]),
    ]),
    element.fragment({
      use slug <- list.filter_map(order)
      use chapter <- result.map(dict.get(chapters, slug))
      view_sidebar_chapter(slug, chapter, route, search)
    }),
  ])
}

fn view_sidebar_chapter(
  key: String,
  chapter: Chapter,
  route: Route,
  search: String,
) -> Element(Message) {
  let stories = matching_stories(chapter, search)

  use <- bool.guard(list.is_empty(stories), element.none())

  html.nav([], [
    html.h2([], [html.text(chapter.name)]),
    html.ul([], {
      use story <- list.map(stories)

      let is_active = case route {
        SceneSelect(chapter: c, story: s)
        | SceneDisplay(chapter: c, story: s, ..) -> c == key && s == story.slug
        _ -> False
      }

      html.li([], [
        html.a(
          [
            route.href(SceneSelect(chapter: key, story: story.slug)),
            attribute.classes([#("active", is_active)]),
          ],
          [html.text(story.name)],
        ),
      ])
    }),
  ])
}
