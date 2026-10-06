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

/// A [book](#book) contains all the configuration required to create a Fable
/// application. They are made up of three important concepts: chapters, stories,
/// and scenes.
/// 
/// - [Chapters](#Chapter) are categories or groups of stories. You might have a
///   chapter for form controls, another for charts, and so on.
/// 
/// - [Stories](#Story) are a specific view function or simulated Lustre
///   application. These are the visual elements that you want to showcase or
///   document using Fable.
/// 
/// - [Scenes](#Scene) are different configurations of a story. These allow you
///   to showcase the story in different states or use Lustre's simulation api
///   to choreograph more-complex interactions.
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

/// Create a new scene for a [story](#story). The `name` of the scene is shown in
/// the scene selection list and is helpful for identifying what each scene
/// demonstrates. The `init` arguments are passed to the stories init function
/// (or used as the model in a [static story](#static_story)) and are one way the
/// 
pub fn scene(
  name name: String,
  init arguments: arguments,
) -> Scene(arguments, model, message) {
  story.scene(name, arguments, function.identity)
}

// BUILDERS --------------------------------------------------------------------

/// Set which step in a scene's [simulation](#simulate) should be selected when
/// first loaded. Out of bounds values will be clamped to the valid range. If this
/// option is not configured, the default step will be 0.
/// 
pub fn default_step(
  scene: Scene(arguments, model, message),
  step: Int,
) -> Scene(arguments, model, message) {
  story.with_default_step(scene, step)
}

/// Use Lustre's simulation api to choreograph more-complex interactions in a
/// scene. This lets you show a sequence of interactions such as clicks or inputs,
/// as well as simulate messages for HTTP responses and other effects.
/// 
/// ```gleam
/// fn successful_sign_up_scene() {
///   let scene = fable.scene(name: "Successful sign up flow", init: Nil)
///   use app <- fable.simulate(scene)
/// 
///   let form = query.element(matching: {
///     query.form |> query.and(query.test_id("sign-up-form"))
///   })
/// 
///   let email_input = 
///     query.descendant(of: form, matching: {
///       query.input |> query.and(query.attribute("name", "email"))
///     })
/// 
///   let password_input = 
///     query.descendant(of: form, matching: {
///       query.input |> query.and(query.attribute("name", "password"))
///     })
/// 
///   let sign_up_fields = [
///     #("email", "lucy@gleam.run"),
///     #("password", "W!BBL3")
///   ]
/// 
///   let new_user = User(email: "lucy@gleam.run", password: "W!BBL3")
/// 
///   app
///   |> simulate.typing(on: email_input, text: "lucy@gleam.run")
///   |> simulate.typing(on: password_input, text: "W!BBL3")  
///   |> simulate.submit(on: form, fields: sign_up_fields)
///   |> simulate.message(ApiRegisteredUser(Ok(new_user)))
/// }
/// ```
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
