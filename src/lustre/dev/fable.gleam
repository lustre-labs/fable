// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/result
import lustre
import lustre/attribute
import lustre/component
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/fable/book.{Book}
import lustre/fable/story.{StoryBuilder, StoryConfig}

// BOOKS AND CHAPTERS ----------------------------------------------------------

///
///
pub type Book =
  book.Book

///
///
pub opaque type BookOption {
  BookOption(configure: fn(Book) -> Book)
}

///
///
pub fn book(title: String, options: List(BookOption)) -> Book {
  let init = Book(title, [], [], [])
  use book, option <- list.fold(options, init)

  option.configure(book)
}

///
///
pub fn chapter(title: String, stories: List(Story)) -> BookOption {
  use book <- BookOption

  Book(..book, chapters: [#(title, stories), ..book.chapters])
}

///
///
pub fn stylesheet(css stylesheet: String) -> BookOption {
  use book <- BookOption

  Book(..book, stylesheets: [stylesheet, ..book.stylesheets])
}

///
///
pub fn external_stylesheet(url href: String) -> BookOption {
  use book <- BookOption

  Book(..book, external_stylesheets: [href, ..book.external_stylesheets])
}

///
///
pub fn start(book: Book) -> Result(Nil, lustre.Error) {
  book.start(book)
}

// STORIES ---------------------------------------------------------------------

///
///
pub type Story =
  story.StoryConfig

///
///
pub type StoryBuilder =
  story.StoryBuilder

pub opaque type Control(a) {
  Control(get: fn(Controls) -> a, set: fn(a) -> Msg)
}

///
///
pub type Controls =
  story.Controls

///
///
pub type Msg =
  story.Msg

// BUILDING STORIES ------------------------------------------------------------

///
///
pub fn story(title: String, builder: fn() -> StoryBuilder) -> Story {
  StoryConfig(..builder().run(0), title:)
}

///
///
pub fn scene(view: fn(Controls) -> Element(Msg)) -> StoryBuilder {
  use _ <- StoryBuilder

  StoryConfig(
    title: "",
    inputs: [],
    sequences: [],
    options: [component.adopt_styles(False)],
    view:,
  )
}

// INPUTS AND CONTROLS ---------------------------------------------------------

///
///
pub fn get(controls: Controls, control: Control(a)) -> a {
  control.get(controls)
}

///
///
pub fn set(control: Control(a), value: a) -> Msg {
  control.set(value)
}

///
///
pub fn input(
  label label: String,
  default value: String,
  next next: fn(Control(String)) -> StoryBuilder,
) -> StoryBuilder {
  use value, set_value <- control(value, json.string, decode.string, _, next)

  html.label([], [
    html.p([attribute.class("text-sm")], [html.text(label)]),
    html.input([
      attribute.class("border rounded px-2 py-1"),
      attribute.class("focus:outline-none focus:border-blue-500"),
      attribute.value(value),
      event.on_input(set_value),
    ]),
  ])
}

///
///
pub fn checkbox(
  label: String,
  next: fn(Control(Bool)) -> StoryBuilder,
) -> StoryBuilder {
  use value, set_value <- control(False, json.bool, decode.bool, _, next)

  html.label([], [
    html.p([attribute.class("text-sm")], [html.text(label)]),
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
  next: fn(Control(String)) -> StoryBuilder,
) -> StoryBuilder {
  use value, set_value <- control("", json.string, decode.string, _, next)
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
  default: a,
  encode: fn(a) -> Json,
  decoder: Decoder(a),
  view: fn(a, fn(a) -> Msg) -> Element(Msg),
  next: fn(Control(a)) -> StoryBuilder,
) -> StoryBuilder {
  use key <- StoryBuilder

  let state = fn(controls: Controls) {
    controls.lookup
    |> dict.get(key)
    |> result.unwrap(dynamic.nil())
    |> decode.run(decoder)
    |> result.unwrap(default)
  }

  let set_state = fn(value) {
    value
    |> encode
    |> story.UserEditedValue(key:, value: _)
  }

  let input = fn(controls) { view(state(controls), set_state) }

  let option =
    component.on_property_change(
      int.to_string(key),
      decode.dynamic |> decode.map(story.ComponentUpdatedValue(key:, value: _)),
    )

  let StoryConfig(title:, inputs:, sequences:, options:, view:) =
    next(Control(get: state, set: set_state)).run(key + 1)

  StoryConfig(
    title:,
    inputs: [input, ..inputs],
    sequences:,
    options: [option, ..options],
    view:,
  )
}

// SEQUENCES -------------------------------------------------------------------

///
///
pub fn sequence(
  label: String,
  actions: List(Msg),
  next: fn() -> StoryBuilder,
) -> StoryBuilder {
  use key <- StoryBuilder

  let StoryConfig(title:, inputs:, sequences:, options:, view:) =
    next().run(key + 1)

  StoryConfig(
    title:,
    inputs:,
    sequences: [story.Sequence(key, label, actions), ..sequences],
    options:,
    view:,
  )
}
