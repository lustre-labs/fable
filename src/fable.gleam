// IMPORTS ---------------------------------------------------------------------

import fable/internal/route.{type Route}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import justin
import lustre
import lustre/attribute
import lustre/dev/simulate.{type App, type Simulation}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/portal
import modem

// TYPES -----------------------------------------------------------------------

///
/// 
pub opaque type Book {
  Book(name: String, path: String, chapters: List(Chapter))
}

///
/// 
pub opaque type Chapter {
  Chapter(name: String, path: String, stories: List(Story))
}

///
/// 
pub opaque type Story {
  Story(
    name: String,
    path: String,
    app: App(Dynamic, Dynamic, Dynamic),
    scenes: List(Scene),
  )
}

///
/// 
type Scene {
  Scene(
    name: String,
    path: String,
    size: Viewport,
    simulation: Simulation(Dynamic, Dynamic),
  )
}

///
/// 
pub opaque type SceneConfig(arguments, model, message) {
  SceneConfig(
    name: String,
    size: Viewport,
    arguments: arguments,
    setup: fn(Simulation(model, message)) -> Simulation(model, message),
  )
}

///
/// 
pub opaque type Viewport {
  Viewport(width: Int, height: Int)
}

// CONSTANTS -------------------------------------------------------------------

pub const mobile: Viewport = Viewport(width: 414, height: 896)

pub const tablet: Viewport = Viewport(width: 834, height: 1112)

pub const desktop: Viewport = Viewport(width: 1024, height: 1280)

// CONSTRUCTORS ----------------------------------------------------------------

///
/// 
pub fn book(name name: String, chapters chapters: List(Chapter)) -> Book {
  Book(name:, path: justin.kebab_case(name), chapters:)
}

///
/// 
pub fn chapter(name name: String, stories stories: List(Story)) -> Chapter {
  Chapter(name:, path: justin.kebab_case(name), stories:)
}

///
/// 
pub fn story(
  name name: String,
  app app: App(arguments, model, message),
  scenes scenes: List(SceneConfig(arguments, model, message)),
) -> Story {
  Story(
    name:,
    path: justin.kebab_case(name),
    app: dangerously_erase_type_information(app),
    scenes: list.map(scenes, fn(scene) {
      Scene(
        name: scene.name,
        path: justin.kebab_case(scene.name),
        size: scene.size,
        simulation: {
          simulate.start(app, scene.arguments)
          |> scene.setup
          |> simulate.restart
          |> dangerously_erase_type_information
        },
      )
    }),
  )
}

///
/// 
pub fn scene(
  name name: String,
  viewport size: Viewport,
  arguments arguments: arguments,
  simulation setup: fn(Simulation(model, message)) -> Simulation(model, message),
) -> SceneConfig(arguments, model, message) {
  SceneConfig(name:, size:, arguments:, setup:)
}

//

///
/// 
pub fn start(book: Book, selector: String) -> Result(Nil, lustre.Error) {
  case portal.register() {
    Ok(_) | Error(lustre.ComponentAlreadyRegistered(..)) ->
      lustre.application(init:, update:, view:)
      |> lustre.start(selector, book)
      |> result.replace(Nil)

    Error(reason) -> Error(reason)
  }
}

// UTILS -----------------------------------------------------------------------

@external(erlang, "gleam@function", "identity")
@external(javascript, "../gleam_stdlib/gleam/function.mjs", "identity")
fn dangerously_erase_type_information(value: a) -> b

// MODEL -----------------------------------------------------------------------

type Model {
  Model(book: Book, route: Route)
}

fn get_chapter(book: Book, path: String) -> Result(Chapter, Nil) {
  use chapter <- list.find(book.chapters)

  chapter.path == path
}

fn get_story(book: Book, chapter: String, path: String) -> Result(Story, Nil) {
  use chapter <- result.try(get_chapter(book, chapter))
  use story <- list.find(chapter.stories)

  story.path == path
}

fn get_scene(
  book: Book,
  chapter: String,
  story: String,
  path: String,
) -> Result(Scene, Nil) {
  use chapter <- result.try(get_chapter(book, chapter))
  use story <- result.try(get_story(book, chapter.path, story))
  use scene <- list.find(story.scenes)

  scene.path == path
}

fn init(book: Book) -> #(Model, Effect(Message)) {
  let assert Ok(here) = modem.initial_uri()
  let route = route.from_uri(here) |> result.unwrap(route.Book)

  let model = Model(book:, route:)
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

type Message {
  SceneProducedDiscardableMessage
  UserClickedExternalLink(to: Uri)
  UserClickedInternalLink(route: Route)
  UserClickedStepBackward(chapter: String, story: String, scene: String)
  UserClickedStepForward(chapter: String, story: String, scene: String)
}

fn update(model: Model, message: Message) -> #(Model, Effect(Message)) {
  case message {
    SceneProducedDiscardableMessage -> #(model, effect.none())

    UserClickedExternalLink(to: uri) -> #(model, modem.load(uri))

    UserClickedInternalLink(route: route.Scene(..) as route) -> {
      let book =
        update_scene(
          model.book,
          route.chapter,
          route.story,
          route.name,
          fn(scene) {
            Scene(..scene, simulation: simulate.restart(scene.simulation))
          },
        )

      let model = Model(book:, route:)

      #(model, effect.none())
    }

    UserClickedInternalLink(route:) -> {
      let model = Model(..model, route:)

      #(model, effect.none())
    }

    UserClickedStepBackward(chapter:, story:, scene:) -> {
      let book =
        update_scene(model.book, chapter, story, scene, fn(scene) {
          Scene(..scene, simulation: simulate.step_back(scene.simulation))
        })

      let model = Model(..model, book:)

      #(model, effect.none())
    }

    UserClickedStepForward(chapter:, story:, scene:) -> {
      let book =
        update_scene(model.book, chapter, story, scene, fn(scene) {
          Scene(..scene, simulation: simulate.step_forward(scene.simulation))
        })

      let model = Model(..model, book:)

      #(model, effect.none())
    }
  }
}

fn update_chapter(
  book: Book,
  name: String,
  run: fn(Chapter) -> Chapter,
) -> Book {
  let chapters =
    list.map(book.chapters, fn(chapter) {
      case chapter.path == name {
        True -> run(chapter)
        False -> chapter
      }
    })

  Book(..book, chapters:)
}

fn update_story(
  chapter: Chapter,
  name: String,
  run: fn(Story) -> Story,
) -> Chapter {
  let stories =
    list.map(chapter.stories, fn(story) {
      case story.path == name {
        True -> run(story)
        False -> story
      }
    })

  Chapter(..chapter, stories:)
}

fn update_scene(
  book: Book,
  chapter: String,
  story: String,
  name: String,
  run: fn(Scene) -> Scene,
) -> Book {
  use chapter <- update_chapter(book, chapter)
  use story <- update_story(chapter, story)
  let scenes =
    list.map(story.scenes, fn(scene) {
      case scene.path == name {
        True -> run(scene)
        False -> scene
      }
    })

  Story(..story, scenes:)
}

// VIEW ------------------------------------------------------------------------

//cog:embed assets/styles.css
const styles = "\u{40}\u{69}\u{6D}\u{70}\u{6F}\u{72}\u{74}\u{20}\u{75}\u{72}\u{6C}\u{28}\u{22}\u{68}\u{74}\u{74}\u{70}\u{73}\u{3A}\u{2F}\u{2F}\u{66}\u{6F}\u{6E}\u{74}\u{73}\u{2E}\u{67}\u{6F}\u{6F}\u{67}\u{6C}\u{65}\u{61}\u{70}\u{69}\u{73}\u{2E}\u{63}\u{6F}\u{6D}\u{2F}\u{63}\u{73}\u{73}\u{32}\u{3F}\u{66}\u{61}\u{6D}\u{69}\u{6C}\u{79}\u{3D}\u{43}\u{61}\u{76}\u{65}\u{61}\u{74}\u{3A}\u{77}\u{67}\u{68}\u{74}\u{40}\u{34}\u{30}\u{30}\u{2E}\u{2E}\u{37}\u{30}\u{30}\u{26}\u{66}\u{61}\u{6D}\u{69}\u{6C}\u{79}\u{3D}\u{4E}\u{65}\u{75}\u{74}\u{6F}\u{6E}\u{3A}\u{69}\u{74}\u{61}\u{6C}\u{2C}\u{77}\u{67}\u{68}\u{74}\u{40}\u{30}\u{2C}\u{32}\u{30}\u{30}\u{3B}\u{30}\u{2C}\u{33}\u{30}\u{30}\u{3B}\u{30}\u{2C}\u{34}\u{30}\u{30}\u{3B}\u{30}\u{2C}\u{37}\u{30}\u{30}\u{3B}\u{30}\u{2C}\u{38}\u{30}\u{30}\u{3B}\u{31}\u{2C}\u{34}\u{30}\u{30}\u{26}\u{66}\u{61}\u{6D}\u{69}\u{6C}\u{79}\u{3D}\u{53}\u{70}\u{61}\u{63}\u{65}\u{2B}\u{47}\u{72}\u{6F}\u{74}\u{65}\u{73}\u{6B}\u{3A}\u{77}\u{67}\u{68}\u{74}\u{40}\u{33}\u{30}\u{30}\u{2E}\u{2E}\u{37}\u{30}\u{30}\u{26}\u{66}\u{61}\u{6D}\u{69}\u{6C}\u{79}\u{3D}\u{53}\u{70}\u{61}\u{63}\u{65}\u{2B}\u{4D}\u{6F}\u{6E}\u{6F}\u{3A}\u{69}\u{74}\u{61}\u{6C}\u{2C}\u{77}\u{67}\u{68}\u{74}\u{40}\u{30}\u{2C}\u{34}\u{30}\u{30}\u{3B}\u{30}\u{2C}\u{37}\u{30}\u{30}\u{3B}\u{31}\u{2C}\u{34}\u{30}\u{30}\u{3B}\u{31}\u{2C}\u{37}\u{30}\u{30}\u{26}\u{64}\u{69}\u{73}\u{70}\u{6C}\u{61}\u{79}\u{3D}\u{73}\u{77}\u{61}\u{70}\u{22}\u{29}\u{3B}\u{A}\u{A}\u{3A}\u{72}\u{6F}\u{6F}\u{74}\u{20}\u{7B}\u{A}\u{20}\u{20}\u{63}\u{6F}\u{6C}\u{6F}\u{72}\u{2D}\u{73}\u{63}\u{68}\u{65}\u{6D}\u{65}\u{3A}\u{20}\u{6C}\u{69}\u{67}\u{68}\u{74}\u{20}\u{64}\u{61}\u{72}\u{6B}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{30}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{34}\u{66}\u{32}\u{66}\u{63}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{31}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{65}\u{38}\u{65}\u{35}\u{66}\u{39}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{32}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{63}\u{64}\u{63}\u{37}\u{66}\u{31}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{34}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{61}\u{63}\u{61}\u{31}\u{65}\u{37}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{36}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{35}\u{35}\u{33}\u{66}\u{63}\u{66}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{38}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{33}\u{61}\u{32}\u{38}\u{61}\u{31}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{39}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{31}\u{38}\u{31}\u{31}\u{34}\u{33}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{62}\u{6C}\u{75}\u{65}\u{2D}\u{39}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{30}\u{66}\u{30}\u{62}\u{32}\u{61}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{30}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{66}\u{31}\u{65}\u{62}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{31}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{65}\u{31}\u{64}\u{35}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{32}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{62}\u{64}\u{61}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{34}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{38}\u{61}\u{35}\u{38}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{36}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{61}\u{33}\u{33}\u{31}\u{30}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{38}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{37}\u{35}\u{32}\u{33}\u{30}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{39}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{33}\u{31}\u{30}\u{66}\u{30}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{6F}\u{72}\u{61}\u{6E}\u{67}\u{65}\u{2D}\u{39}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{31}\u{66}\u{30}\u{39}\u{30}\u{30}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{30}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{65}\u{66}\u{66}\u{63}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{31}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{64}\u{64}\u{66}\u{61}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{32}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{65}\u{62}\u{33}\u{66}\u{33}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{34}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{65}\u{37}\u{36}\u{65}\u{39}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{36}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{61}\u{36}\u{30}\u{31}\u{38}\u{63}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{38}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{37}\u{38}\u{30}\u{31}\u{36}\u{36}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{39}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{33}\u{36}\u{30}\u{30}\u{32}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{70}\u{69}\u{6E}\u{6B}\u{2D}\u{39}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{32}\u{35}\u{30}\u{30}\u{31}\u{66}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{30}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{66}\u{66}\u{38}\u{66}\u{61}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{31}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{65}\u{65}\u{38}\u{65}\u{66}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{32}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{63}\u{62}\u{61}\u{63}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{33}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{63}\u{62}\u{61}\u{63}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{34}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{39}\u{38}\u{36}\u{61}\u{39}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{35}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{39}\u{38}\u{36}\u{61}\u{39}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{36}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{62}\u{32}\u{30}\u{39}\u{33}\u{64}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{37}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{62}\u{32}\u{30}\u{39}\u{33}\u{64}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{38}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{38}\u{31}\u{30}\u{36}\u{32}\u{63}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{39}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{33}\u{61}\u{30}\u{33}\u{31}\u{34}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{72}\u{65}\u{64}\u{2D}\u{39}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{32}\u{37}\u{30}\u{32}\u{30}\u{64}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{30}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{66}\u{61}\u{66}\u{39}\u{66}\u{39}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{31}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{65}\u{66}\u{65}\u{64}\u{65}\u{62}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{32}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{64}\u{30}\u{63}\u{62}\u{63}\u{34}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{34}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{62}\u{32}\u{61}\u{38}\u{39}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{36}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{36}\u{31}\u{35}\u{38}\u{34}\u{64}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{38}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{34}\u{35}\u{33}\u{65}\u{33}\u{37}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{39}\u{30}\u{30}\u{3A}\u{20}\u{23}\u{31}\u{63}\u{31}\u{39}\u{31}\u{36}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{63}\u{6F}\u{6C}\u{6F}\u{75}\u{72}\u{2D}\u{67}\u{72}\u{65}\u{79}\u{2D}\u{39}\u{35}\u{30}\u{3A}\u{20}\u{23}\u{31}\u{31}\u{30}\u{66}\u{30}\u{65}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{66}\u{6F}\u{6E}\u{74}\u{2D}\u{6E}\u{65}\u{75}\u{74}\u{6F}\u{6E}\u{3A}\u{20}\u{22}\u{4E}\u{65}\u{75}\u{74}\u{6F}\u{6E}\u{22}\u{2C}\u{20}\u{47}\u{65}\u{6F}\u{72}\u{67}\u{69}\u{61}\u{2C}\u{20}\u{73}\u{65}\u{72}\u{69}\u{66}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{66}\u{6F}\u{6E}\u{74}\u{2D}\u{73}\u{70}\u{61}\u{63}\u{65}\u{2D}\u{67}\u{72}\u{6F}\u{74}\u{65}\u{73}\u{6B}\u{3A}\u{20}\u{22}\u{53}\u{70}\u{61}\u{63}\u{65}\u{20}\u{47}\u{72}\u{6F}\u{74}\u{65}\u{73}\u{6B}\u{22}\u{2C}\u{20}\u{73}\u{79}\u{73}\u{74}\u{65}\u{6D}\u{2D}\u{75}\u{69}\u{2C}\u{20}\u{73}\u{61}\u{6E}\u{73}\u{2D}\u{73}\u{65}\u{72}\u{69}\u{66}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{66}\u{6F}\u{6E}\u{74}\u{2D}\u{73}\u{70}\u{61}\u{63}\u{65}\u{2D}\u{6D}\u{6F}\u{6E}\u{6F}\u{3A}\u{20}\u{22}\u{53}\u{70}\u{61}\u{63}\u{65}\u{20}\u{4D}\u{6F}\u{6E}\u{6F}\u{22}\u{2C}\u{20}\u{6D}\u{6F}\u{6E}\u{6F}\u{73}\u{70}\u{61}\u{63}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{66}\u{6F}\u{6E}\u{74}\u{2D}\u{63}\u{61}\u{76}\u{65}\u{61}\u{74}\u{3A}\u{20}\u{22}\u{43}\u{61}\u{76}\u{65}\u{61}\u{74}\u{22}\u{2C}\u{20}\u{63}\u{75}\u{72}\u{73}\u{69}\u{76}\u{65}\u{3B}\u{A}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{67}\u{61}\u{70}\u{3A}\u{20}\u{30}\u{2E}\u{32}\u{35}\u{72}\u{65}\u{6D}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{72}\u{61}\u{64}\u{69}\u{75}\u{73}\u{3A}\u{20}\u{34}\u{70}\u{78}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{74}\u{65}\u{78}\u{74}\u{3A}\u{20}\u{31}\u{72}\u{65}\u{6D}\u{3B}\u{A}\u{7D}\u{A}\u{A}\u{2A}\u{2C}\u{A}\u{2A}\u{3A}\u{3A}\u{62}\u{65}\u{66}\u{6F}\u{72}\u{65}\u{2C}\u{A}\u{2A}\u{3A}\u{3A}\u{61}\u{66}\u{74}\u{65}\u{72}\u{20}\u{7B}\u{A}\u{20}\u{20}\u{6D}\u{61}\u{72}\u{67}\u{69}\u{6E}\u{3A}\u{20}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{70}\u{61}\u{64}\u{64}\u{69}\u{6E}\u{67}\u{3A}\u{20}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{62}\u{6F}\u{78}\u{2D}\u{73}\u{69}\u{7A}\u{69}\u{6E}\u{67}\u{3A}\u{20}\u{62}\u{6F}\u{72}\u{64}\u{65}\u{72}\u{2D}\u{62}\u{6F}\u{78}\u{3B}\u{A}\u{7D}\u{A}\u{A}\u{68}\u{74}\u{6D}\u{6C}\u{20}\u{7B}\u{A}\u{20}\u{20}\u{2D}\u{77}\u{65}\u{62}\u{6B}\u{69}\u{74}\u{2D}\u{66}\u{6F}\u{6E}\u{74}\u{2D}\u{73}\u{6D}\u{6F}\u{6F}\u{74}\u{68}\u{69}\u{6E}\u{67}\u{3A}\u{20}\u{61}\u{6E}\u{74}\u{69}\u{61}\u{6C}\u{69}\u{61}\u{73}\u{65}\u{64}\u{3B}\u{A}\u{20}\u{20}\u{2D}\u{77}\u{65}\u{62}\u{6B}\u{69}\u{74}\u{2D}\u{74}\u{65}\u{78}\u{74}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{61}\u{64}\u{6A}\u{75}\u{73}\u{74}\u{3A}\u{20}\u{6E}\u{6F}\u{6E}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{74}\u{65}\u{78}\u{74}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{61}\u{64}\u{6A}\u{75}\u{73}\u{74}\u{3A}\u{20}\u{6E}\u{6F}\u{6E}\u{65}\u{3B}\u{A}\u{20}\u{20}\u{68}\u{61}\u{6E}\u{67}\u{69}\u{6E}\u{67}\u{2D}\u{70}\u{75}\u{6E}\u{63}\u{74}\u{75}\u{61}\u{74}\u{69}\u{6F}\u{6E}\u{3A}\u{20}\u{66}\u{69}\u{72}\u{73}\u{74}\u{20}\u{61}\u{6C}\u{6C}\u{6F}\u{77}\u{2D}\u{65}\u{6E}\u{64}\u{20}\u{6C}\u{61}\u{73}\u{74}\u{3B}\u{A}\u{7D}\u{A}\u{A}\u{62}\u{6F}\u{64}\u{79}\u{20}\u{7B}\u{A}\u{20}\u{20}\u{6D}\u{61}\u{72}\u{67}\u{69}\u{6E}\u{3A}\u{20}\u{30}\u{3B}\u{A}\u{20}\u{20}\u{66}\u{6F}\u{6E}\u{74}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{3A}\u{20}\u{76}\u{61}\u{72}\u{28}\u{2D}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{74}\u{65}\u{78}\u{74}\u{29}\u{3B}\u{A}\u{20}\u{20}\u{6C}\u{69}\u{6E}\u{65}\u{2D}\u{68}\u{65}\u{69}\u{67}\u{68}\u{74}\u{3A}\u{20}\u{72}\u{6F}\u{75}\u{6E}\u{64}\u{28}\u{76}\u{61}\u{72}\u{28}\u{2D}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{74}\u{65}\u{78}\u{74}\u{29}\u{20}\u{2A}\u{20}\u{31}\u{2E}\u{37}\u{2C}\u{20}\u{76}\u{61}\u{72}\u{28}\u{2D}\u{2D}\u{73}\u{69}\u{7A}\u{65}\u{2D}\u{67}\u{61}\u{70}\u{29}\u{29}\u{3B}\u{A}\u{7D}\u{A}"

fn view(model: Model) -> Element(Message) {
  element.fragment([
    html.style([], styles),
    view_route(model),
  ])
}

fn view_route(model: Model) -> Element(Message) {
  case model.route {
    route.Book -> view_book(model.book)

    route.Chapter(name:) ->
      case get_chapter(model.book, name) {
        Ok(chapter) -> view_chapter(chapter)
        Error(_) -> view_unknown(route.to_path(model.route))
      }

    route.Story(chapter:, name:) ->
      case get_story(model.book, chapter, name) {
        Ok(story) -> view_story(chapter, story)
        Error(_) -> view_unknown(route.to_path(model.route))
      }

    route.Scene(chapter:, story:, name:) ->
      case get_scene(model.book, chapter, story, name) {
        Ok(scene) -> view_scene(chapter, story, scene)
        Error(_) -> view_unknown(route.to_path(model.route))
      }

    route.Unknown(path:) -> view_unknown(path)
  }
}

fn view_book(book: Book) -> Element(Message) {
  html.ul([], {
    list.map(book.chapters, fn(chapter) {
      html.li([], [
        html.a([route.href(route.Chapter(name: chapter.path))], [
          html.text(chapter.name),
        ]),
      ])
    })
  })
}

fn view_chapter(chapter: Chapter) -> Element(Message) {
  html.ul([], {
    list.map(chapter.stories, fn(story) {
      html.li([], [
        html.a(
          [route.href(route.Story(chapter: chapter.path, name: story.path))],
          [html.text(story.name)],
        ),
      ])
    })
  })
}

fn view_story(chapter: String, story: Story) -> Element(Message) {
  html.ul([], {
    list.map(story.scenes, fn(scene) {
      html.li([], [
        html.a(
          [
            route.href(route.Scene(
              chapter:,
              story: story.path,
              name: scene.path,
            )),
          ],
          [html.text(scene.name)],
        ),
      ])
    })
  })
}

fn view_scene(
  chapter: String,
  story: String,
  scene: Scene,
) -> Element(Message) {
  html.div([], [
    html.p([], [
      html.button(
        [
          event.on_click(UserClickedStepBackward(
            chapter:,
            story:,
            scene: scene.path,
          )),
        ],
        [html.text("←")],
      ),

      html.button(
        [
          event.on_click(UserClickedStepForward(
            chapter:,
            story:,
            scene: scene.path,
          )),
        ],
        [html.text("→")],
      ),
    ]),

    html.div([attribute.style("display", "flex")], [
      html.iframe([
        attribute.width(scene.size.width),
        attribute.height(scene.size.height),
      ]),

      html.div([], [
        html.ul(
          [],
          list.map(simulate.current_history(scene.simulation), fn(event) {
            html.li([], [html.text(string.inspect(event))])
          }),
        ),
        html.p([], [html.text(string.inspect(simulate.model(scene.simulation)))]),
      ]),
    ]),

    portal.to("iframe", [], [
      html.html([], [
        html.body([], [
          simulate.view(scene.simulation)
          |> element.map(fn(_) { SceneProducedDiscardableMessage }),
        ]),
      ]),
    ]),
  ])
}

fn view_unknown(path: String) -> Element(Message) {
  html.div([], [
    html.p([], [
      html.text("We couldn't find anything at " <> path <> ". Please "),
      html.a([route.href(route.Book)], [html.text("go back")]),
      html.text(" and try again."),
    ]),
  ])
}
