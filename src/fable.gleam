// IMPORTS ---------------------------------------------------------------------

import fable/middleware
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

/// A `Book` is a collection of [`Story`](#Story)s, organised by chapter.
///
pub opaque type Book {
  Book(chapters: Dict(String, Dict(String, Element(Never))))
}

///
///
pub type Builder(slice, model) =
  middleware.Middleware(slice, model)

///
///
pub type Control(slice, model) =
  middleware.State(slice, model)

/// A `Story` is an isolated interactive demo of a component or part of your
/// application.
///
pub opaque type Story(controls) {
  Story(title: String, name: String, controls: Builder(controls, controls))
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn book() -> Book {
  Book(dict.new())
}

///
///
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

///
///
pub fn story(
  title: String,
  component: fn() -> Builder(controls, controls),
) -> Story(controls) {
  let name = title |> string.lowercase |> justin.kebab_case

  Story(title, name, component())
}

// CONTROLS --------------------------------------------------------------------

///
///
pub fn input(
  label: String,
  next: fn(Control(String, model)) -> Builder(rest, model),
) -> Builder(#(String, rest), model) {
  use value <- middleware.state("")
  use <- middleware.control(fn(model) {
    html.label([attribute.class("grid grid-cols-3 gap-2")], [
      html.span([attribute.class("col-span-1 text-right")], [html.text(label)]),
      html.input([
        attribute.class("col-span-2 px-2 py-1"),
        attribute.value(value.get(model)),
        event.on_input(value.set),
      ]),
    ])
  })

  next(value)
}

///
///
pub fn select(
  label: String,
  options: List(String),
  next: fn(Control(String, model)) -> Builder(rest, model),
) -> Builder(#(String, rest), model) {
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

///
///
pub fn checkbox(
  label: String,
  next: fn(Control(Bool, model)) -> Builder(rest, model),
) -> Builder(#(Bool, rest), model) {
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

///
///
pub fn render(
  component: fn(model) -> Element(fn(model) -> model),
) -> Builder(Nil, model) {
  use model, controls <- middleware.return

  html.div([attribute.class("w-full h-full flex flex-col")], [
    html.div([attribute.class("flex-1 flex justify-center items-center")], [
      component(model),
    ]),
    html.div(
      [
        attribute.class(
          " max-h-[300px] overflow-y-scroll flex flex-col gap-2 bg-gray-50
            p-4 border-t border-gray-200
          ",
        ),
      ],
      controls,
    ),
  ])
}

//

///
///
pub fn to_component(
  story: Story(controls),
) -> Result(
  fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg),
  lustre.Error,
) {
  let app = middleware.compose(story.controls)
  use _ <- result.map(lustre.register(app, story.name <> "-story"))

  fn(attributes, children) {
    element(story.name <> "-story", attributes, children)
  }
}

///
///
pub fn to_app(book: Book) -> lustre.App(Nil, _, _) {
  let sidebar =
    book.chapters
    |> dict.to_list
    |> list.map(pair.map_second(_, dict.keys))

  let assert Ok(uri) = modem.initial_uri()

  middleware.compose({
    use search <- middleware.state("")
    use route <- middleware.state(uri.path)
    use loaded <- middleware.state(False)

    use <- middleware.effect(fn(model) {
      use <- bool.guard(loaded.get(model), effect.none())
      let set_loaded = effect.from(function.apply1(_, loaded.set(True)))
      let init_router = modem.init(fn(uri) { route.set(uri.path) })

      effect.batch([set_loaded, init_router])
    })

    use state, _ <- middleware.return

    let search_input =
      html.input([
        attribute.value(search.get(state)),
        attribute.placeholder("Search..."),
        event.on_input(search.set),
      ])

    let stories =
      list.map(sidebar, fn(section) {
        element.fragment([
          html.h2([], [html.text(section.0)]),
          element.keyed(
            html.ul([], _),
            section.1
              |> list.filter(string.contains(_, search.get(state)))
              |> list.map(fn(story) {
                let href = "/" <> section.0 <> "/" <> story
                let colour = case route.get(state) == href {
                  True -> "text-blue-500"
                  False -> "text-gray-800"
                }

                let html =
                  html.li([attribute.class("ml-2 " <> colour)], [
                    html.a([attribute.href(href)], [html.text(story)]),
                  ])

                #(story, html)
              }),
          ),
        ])
      })

    html.div([attribute.class("flex w-screen h-screen items-stretch")], [
      html.aside(
        [
          attribute.class(
            "w-[300px] h-full bg-gray-100 overflow-y-scroll p-4 flex flex-col gap-4",
          ),
        ],
        [
          html.h1([attribute.class("text-2xl")], [html.text("Fable")]),
          search_input,
          html.nav([], stories),
        ],
      ),
      html.main([attribute.class("flex-1")], [
        case state |> route.get |> uri.path_segments {
          [chapter, story] ->
            book.chapters
            |> dict.get(chapter)
            |> result.then(dict.get(_, story))
            |> result.unwrap(element.none())
            |> element.map(funtil.never)
          _ -> element.none()
        },
      ]),
    ])
  })
}
