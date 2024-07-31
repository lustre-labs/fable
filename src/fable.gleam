// IMPORTS ---------------------------------------------------------------------

import fable/middleware
import fable/ui
import funtil.{type Never}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/function
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import gleam/uri
import justin
import lustre
import lustre/attribute.{type Attribute}
import lustre/effect
import lustre/element.{type Element, element}
import lustre/element/html
import lustre/event
import modem

// TYPES -----------------------------------------------------------------------

pub opaque type Book {
  Book(chapters: Dict(String, Dict(String, Element(Never))))
}

pub type Middleware(slice, model) =
  middleware.Middleware(slice, model)

pub type State(slice, model) =
  middleware.State(slice, model)

pub opaque type Story(controls) {
  Story(title: String, name: String, controls: Middleware(controls, controls))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn book() -> Book {
  Book(dict.new())
}

pub fn add_story(book: Book, chapter: String, story: Story(controls)) -> Book {
  case to_component(story) {
    Error(_) -> book
    Ok(component) ->
      Book({
        use stories <- dict.upsert(book.chapters, chapter)

        stories
        |> option.map(dict.insert(_, story.name, component([], [])))
        |> option.unwrap(dict.from_list([#(story.name, component([], []))]))
      })
  }
}

pub fn story(
  title: String,
  component: fn() -> Middleware(controls, controls),
) -> Story(controls) {
  let name =
    title
    |> justin.kebab_case
    |> string.append("-story")

  Story(title, name, component())
}

// CONTROLS --------------------------------------------------------------------

pub fn input(
  label: String,
  next: fn(State(String, model)) -> Middleware(rest, model),
) -> Middleware(#(String, rest), model) {
  use value <- middleware.state("")
  use <- middleware.control(fn(model) {
    html.label([], [
      html.text(label),
      html.input([attribute.value(value.get(model)), event.on_input(value.set)]),
    ])
  })

  next(value)
}

pub fn select(
  label: String,
  options: List(String),
  next: fn(State(String, model)) -> Middleware(rest, model),
) -> Middleware(#(String, rest), model) {
  use value <- middleware.state("")
  let handle_change = fn(event) {
    use selected <- result.try(event.value(event))

    Ok(value.set(selected))
  }

  use <- middleware.control(fn(model) {
    html.label([], [
      html.text(label),
      html.select(
        [attribute.value(value.get(model)), event.on("change", handle_change)],
        list.map(options, fn(option) {
          html.option([attribute.value(option)], option)
        }),
      ),
    ])
  })

  next(value)
}

pub fn checkbox(
  label: String,
  next: fn(State(Bool, model)) -> Middleware(rest, model),
) -> Middleware(#(Bool, rest), model) {
  use value <- middleware.state(False)
  let handle_change = fn(event) {
    use checked <- result.try(dynamic.field("checked", dynamic.bool)(event))

    Ok(value.set(checked))
  }

  use <- middleware.control(fn(model) {
    html.label([], [
      html.input([
        attribute.type_("checkbox"),
        attribute.checked(value.get(model)),
        event.on("change", handle_change),
      ]),
      html.text(label),
    ])
  })

  next(value)
}

//

pub fn render(
  component: fn(model) -> Element(fn(model) -> model),
) -> Middleware(Nil, model) {
  use model, controls <- middleware.return

  ui.component(component(model), controls)
}

//

pub fn to_component(
  story: Story(controls),
) -> Result(
  fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg),
  lustre.Error,
) {
  let app = middleware.compose(story.controls)
  use _ <- result.map(lustre.register(app, story.name))

  fn(attributes, children) { element(story.name, attributes, children) }
}

pub fn to_app(book: Book) -> lustre.App(Nil, _, _) {
  let sidebar =
    book.chapters
    |> dict.to_list
    |> list.map(pair.map_second(_, dict.keys))

  let assert Ok(uri) = modem.initial_uri()

  middleware.compose({
    use route <- middleware.state(uri.path)
    use loaded <- middleware.state(False)

    use <- middleware.effect(fn(model) {
      use <- bool.guard(loaded.get(model), effect.none())
      let set_loaded = effect.from(function.apply1(_, loaded.set(True)))
      let init_router = modem.init(fn(uri) { route.set(uri.path) })

      effect.batch([set_loaded, init_router])
    })

    use state, _ <- middleware.return

    ui.shell(sidebar, case state |> route.get |> uri.path_segments {
      [chapter, story] ->
        book.chapters
        |> dict.get(chapter)
        |> result.then(dict.get(_, story))
        |> result.unwrap(element.none())
        |> element.map(funtil.never)
      _ -> element.none()
    })
  })
}
