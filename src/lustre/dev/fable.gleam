// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import lustre/attribute
import lustre/component
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre_fable/book.{Book}
import lustre_fable/story.{StoryBuilder, StoryConfig}
import lustre_fable/value.{type Value, PrimitiveBool, PrimitiveString}

// BOOKS AND CHAPTERS ----------------------------------------------------------

pub type Book =
  book.Book

///
///
pub fn book(title: String) -> Book {
  Book(title:, stylesheets: [], external_stylesheets: [], chapters: [])
}

///
///
pub fn chapter(book: Book, title: String, stories: List(Story)) -> Book {
  Book(..book, chapters: [#(title, stories), ..book.chapters])
}

///
///
pub fn stylesheet(book: Book, css stylesheet: String) -> Book {
  Book(..book, stylesheets: [stylesheet, ..book.stylesheets])
}

///
///
pub fn external_stylesheet(book: Book, url href: String) -> Book {
  Book(..book, external_stylesheets: [href, ..book.external_stylesheets])
}

pub fn start(book: Book) -> Nil {
  let _ = book.start(book)

  Nil
}

// STORIES ---------------------------------------------------------------------

///
///
pub type Story =
  story.StoryConfig

///
///
pub type StoryBulder =
  story.StoryBuilder

///
///
pub type Model =
  story.Model

///
///
pub type Msg =
  story.Msg

// BUILDING STORIES ------------------------------------------------------------

///
///
pub fn story(title: String, builder: fn() -> StoryBulder) -> Story {
  StoryConfig(..builder().run(0), title:)
}

///
///
pub fn scene(view: fn(Model) -> Element(Msg)) -> StoryBulder {
  use _ <- StoryBuilder

  StoryConfig(
    title: "",
    inputs: [],
    options: [component.adopt_styles(False)],
    view:,
  )
}

// INPUTS AND CONTROLS ---------------------------------------------------------

///
///
pub fn input(
  label: String,
  next: fn(fn(Model) -> String, fn(String) -> Msg) -> StoryBulder,
) -> StoryBulder {
  use value, set_value <- control(PrimitiveString, value.as_string, _, next)

  html.label([], [
    html.p([], [html.text(label)]),
    html.input([attribute.value(value), event.on_input(set_value)]),
  ])
}

///
///
pub fn checkbox(
  label: String,
  next: fn(fn(Model) -> Bool, fn(Bool) -> Msg) -> StoryBulder,
) -> StoryBulder {
  use value, set_value <- control(PrimitiveBool, value.as_bool, _, next)

  html.label([], [
    html.p([], [html.text(label)]),
    html.input([
      attribute.checked(value),
      attribute.type_("checkbox"),
      event.on_check(set_value),
    ]),
  ])
}

///
///
pub fn select(
  label: String,
  options: List(#(String, String)),
  next: fn(fn(Model) -> String, fn(String) -> Msg) -> StoryBulder,
) -> StoryBulder {
  use value, set_value <- control(PrimitiveString, value.as_string, _, next)
  let options =
    list.map(options, fn(option) {
      html.option(
        [attribute.value(option.0), attribute.selected(value == option.0)],
        option.1,
      )
    })

  html.label([], [
    html.p([], [html.text(label)]),
    html.select([event.on_change(set_value)], [
      html.option([attribute.value(""), attribute.selected(value == "")], ""),
      ..options
    ]),
  ])
}

fn control(
  wrap: fn(value) -> Value,
  read: fn(Value) -> value,
  view: fn(value, fn(value) -> Msg) -> Element(Msg),
  next: fn(fn(Model) -> value, fn(value) -> Msg) -> StoryBulder,
) -> StoryBulder {
  use key <- StoryBuilder

  let state = fn(model: Model) {
    model.lookup
    |> dict.get(key)
    // If the state hasn't yet been set we'll just default to an empty string. The
    // provided `read` function will default to a value of whatever type is
    // appropriate.
    |> result.unwrap(PrimitiveString(""))
    |> read
  }

  let set_state = fn(value) {
    value
    |> wrap
    |> story.UserEditedValue(key:, value: _)
  }

  let input = fn(model) { view(state(model), set_state) }

  let option =
    component.on_property_change(
      int.to_string(key),
      value.decoder()
        |> decode.map(story.ComponentUpdatedValue(key:, value: _))
        |> decode.map(fn(msg) { echo msg }),
    )

  let StoryConfig(title:, inputs:, options:, view:) =
    next(state, set_state).run(key + 1)

  StoryConfig(
    title:,
    inputs: [input, ..inputs],
    options: [option, ..options],
    view:,
  )
}
