// IMPORTS ---------------------------------------------------------------------

import fable/internal/book.{type Message, type Model}
import fable/internal/story
import gleam/bool
import gleam/function
import gleam/result
import lustre
import lustre/dev/simulate.{type App, type Simulation}
import lustre/element.{type Element}
import lustre/portal

// TYPES -----------------------------------------------------------------------

///
/// 
pub type Book =
  book.Book

///
/// 
pub type Chapter =
  book.Chapter

///
/// 
pub type Story =
  story.Story

///
/// 
pub type Scene(arguments, model, message) =
  story.SceneConfig(arguments, model, message)

// CONSTRUCTORS ----------------------------------------------------------------

///
/// 
pub fn book(name name: String, chapters chapters: List(Chapter)) -> Book {
  book.new(name, chapters)
}

///
/// 
pub fn chapter(name name: String, stories stories: List(Story)) -> Chapter {
  book.chapter(name, stories)
}

///
/// 
pub fn story(
  name name: String,
  template simulation: App(arguments, model, message),
  scenes scenes: List(Scene(arguments, model, message)),
) -> Story {
  story.new(name, simulation, scenes)
}

///
/// 
pub fn static_story(
  name name: String,
  view view: fn(model) -> Element(message),
  scenes scenes: List(Scene(model, model, message)),
) -> Story {
  story.new(
    name,
    simulate.simple(function.identity, fn(model, _) { model }, view),
    scenes,
  )
}

///
/// 
pub fn scene(
  name name: String,
  init arguments: arguments,
) -> Scene(arguments, model, message) {
  story.scene(name, arguments, function.identity)
}

// BUILDERS --------------------------------------------------------------------

///
/// 
pub fn default_step(
  scene: Scene(arguments, model, message),
  step: Int,
) -> Scene(arguments, model, message) {
  story.with_default_step(scene, step)
}

///
/// 
pub fn simulate(
  scene: Scene(arguments, model, message),
  setup: fn(Simulation(model, message)) -> Simulation(model, message),
) -> Scene(arguments, model, message) {
  story.with_simulation(scene, setup)
}

//

///
/// 
pub fn start(book: Book) -> Result(Nil, lustre.Error) {
  use <- bool.guard(is_iframe(), Ok(Nil))

  case portal.register() {
    Ok(_) | Error(lustre.ComponentAlreadyRegistered(..)) ->
      book.app()
      |> isolated_start(book)
      |> result.replace(Nil)

    Error(reason) -> Error(reason)
  }
}

@external(javascript, "./fable.ffi.mjs", "isIframe")
fn is_iframe() -> Bool {
  False
}

@external(javascript, "./fable.ffi.mjs", "isolatedStart")
fn isolated_start(
  _app: lustre.App(Book, Model, Message),
  _book: Book,
) -> Result(Nil, lustre.Error) {
  Error(lustre.NotABrowser)
}
