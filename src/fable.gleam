// IMPORTS ---------------------------------------------------------------------

import fable/internal/book
import fable/internal/story
import gleam/function
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/dev/query.{type Query}
import lustre/dev/simulate.{type App, type Simulation}
import lustre/element/html
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
  template app: App(arguments, model, message),
  scenes scenes: List(Scene(arguments, model, message)),
) -> Story {
  story.new(name, app, scenes)
}

///
/// 
pub fn scene(
  name name: String,
  init arguments: arguments,
  play setup: fn(Simulation(model, message)) -> Simulation(model, message),
) -> Scene(arguments, model, message) {
  story.scene(name, arguments, setup)
}

///
/// 
pub fn static_scene(
  name name: String,
  init arguments: arguments,
) -> Scene(arguments, model, message) {
  story.scene(name, arguments, function.identity)
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn with_stylesheet(
  book: Book,
  href: String,
  crossorigin crossorigin: Bool,
) -> Book {
  book.add_to_head(book, case crossorigin {
    True ->
      html.link([
        attribute.href(href),
        attribute.rel("stylesheet"),
        attribute.crossorigin("anonymous"),
      ])

    False -> html.link([attribute.href(href), attribute.rel("stylesheet")])
  })
}

// SIMULATED INTERACTIONS ------------------------------------------------------

///
/// 
pub fn click(
  simulation: Simulation(model, message),
  target element: Query,
) -> Simulation(model, message) {
  simulate.event(simulation, on: element, name: "click", data: [])
}

///
/// 
pub fn input(
  simulation: Simulation(model, message),
  target element: Query,
  from start: String,
  enter text: String,
) -> Simulation(model, message) {
  let characters = string.to_graphemes(text)
  let values = list.scan(characters, start, string.append)
  use simulation, value <- list.fold(values, simulation)

  simulate.event(simulation, on: element, name: "input", data: [
    #("target", {
      json.object([
        #("value", json.string(value)),
      ])
    }),
  ])
}

///
/// 
pub fn submit(
  simulation: Simulation(model, message),
  target element: Query,
  fields fields: List(#(String, String)),
) -> Simulation(model, message) {
  simulate.event(simulation, on: element, name: "submit", data: [
    #("detail", {
      json.object([
        #("formData", {
          json.array(fields, fn(field) {
            json.preprocessed_array([
              json.string(field.0),
              json.string(field.1),
            ])
          })
        }),
      ])
    }),
  ])
}

//

///
/// 
pub fn start(book: Book, selector: String) -> Result(Nil, lustre.Error) {
  case portal.register() {
    Ok(_) | Error(lustre.ComponentAlreadyRegistered(..)) ->
      book.app()
      |> lustre.start(selector, book)
      |> result.replace(Nil)

    Error(reason) -> Error(reason)
  }
}
