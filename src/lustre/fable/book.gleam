// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/fable/chapter.{type Chapter, Chapter}
import lustre/fable/document
import lustre/fable/route.{type Route}
import lustre/fable/story.{type Story, type StoryConfig}
import lustre/fable/ui/icon
import lustre/fable/ui/layout.{type Layout, Layout}
import lustre/fable/window
import modem

// TYPES -----------------------------------------------------------------------

pub type Book {
  Book(
    title: String,
    stylesheets: List(String),
    external_stylesheets: List(String),
    chapters: List(#(String, List(StoryConfig))),
  )
}

// MAIN ------------------------------------------------------------------------

pub fn start(book: Book) {
  let app = lustre.application(init:, update:, view:)
  let chapters =
    list.filter_map(book.chapters, fn(chapter) {
      chapter.init(
        chapter.0,
        chapter.1,
        book.stylesheets,
        book.external_stylesheets,
      )
    })

  let args =
    Args(
      is_mobile: window.match_media("(width < 64rem)"),
      title: book.title,
      chapters:,
    )

  use runtime <- result.try(lustre.start(app, "#app", args))

  //
  window.watch_media("(width < 64rem)", fn(is_mobile) {
    lustre.dispatch(BrowserChangedDimensions(is_mobile:))
    |> lustre.send(to: runtime)
  })

  //
  window.add_event_listener("keydown", fn(event) {
    let decoder = {
      use key <- decode.field("key", decode.string)

      case key {
        "/" -> decode.success(UserFocusedSearch)
        _ -> decode.failure(UserFocusedSearch, "/")
      }
    }

    decode.run(event, decoder)
    |> result.map(lustre.dispatch)
    |> result.map(lustre.send(message: _, to: runtime))
    |> result.unwrap(Nil)
  })

  Ok(Nil)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(
    layout: Layout,
    route: Route,
    title: String,
    filter: String,
    chapters: List(Chapter),
    filtered: List(Chapter),
  )
}

type Args {
  Args(is_mobile: Bool, title: String, chapters: List(Chapter))
}

fn init(args: Args) -> #(Model, Effect(Msg)) {
  let route =
    modem.initial_uri()
    |> result.map(route.parse)
    |> result.unwrap(route.Index)

  let model =
    Model(
      layout: Layout(
        is_mobile: args.is_mobile,
        is_sidebar_open: !args.is_mobile,
      ),
      route:,
      title: args.title,
      filter: "",
      chapters: args.chapters,
      filtered: args.chapters,
    )

  let effect =
    modem.init(fn(uri) {
      uri
      |> route.parse
      |> UserNavigatedTo
    })

  #(model, effect)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  BrowserChangedDimensions(is_mobile: Bool)
  UserFocusedSearch
  UserNavigatedTo(route: Route)
  UserToggledSidebar
  UserTypedSearch(filter: String)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case echo msg {
    BrowserChangedDimensions(is_mobile) -> {
      let is_sidebar_open = !is_mobile
      let layout = Layout(is_mobile:, is_sidebar_open:)
      let model = Model(..model, layout:, filtered: model.chapters)

      #(model, effect.none())
    }

    UserFocusedSearch -> {
      let effect = document.focus("input[type='search']")

      #(model, effect)
    }

    UserNavigatedTo(route) -> {
      let model = Model(..model, route:, filter: "", filtered: model.chapters)

      #(model, effect.none())
    }

    UserToggledSidebar -> {
      let is_sidebar_open = !model.layout.is_sidebar_open
      let layout = Layout(..model.layout, is_sidebar_open:)
      let model = Model(..model, layout:)

      #(model, effect.none())
    }

    UserTypedSearch(filter: "") -> {
      let model = Model(..model, filter: "", filtered: model.chapters)

      #(model, effect.none())
    }

    UserTypedSearch(filter:) -> {
      let filtered =
        list.filter_map(model.chapters, fn(chapter) {
          let stories =
            list.filter(chapter.stories, fn(story) {
              string.contains(
                string.lowercase(story.title),
                string.lowercase(filter),
              )
            })

          case stories {
            [] -> Error(Nil)
            _ -> Ok(Chapter(chapter.title, chapter.route, stories:))
          }
        })
      let model = Model(..model, filter:, filtered:)

      #(model, effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  layout.shell(
    model.layout,
    on_sidebar_toggle: UserToggledSidebar,
    header: [],
    sidebar: [
      html.h1([attribute.class("flex items-center min-h-12 bg-blue-50")], [
        html.span([attribute.class("text-lg text-blue-500 font-semibold")], [
          html.text(model.title),
        ]),
      ]),
      html.div([], [
        html.div(
          [
            attribute.class("flex items-center gap-2 border rounded px-3 py-2"),
            attribute.class("focus-within:border-blue-500"),
          ],
          [
            html.span([], [icon.search([attribute.class("size-4")])]),
            html.input([
              attribute.class("flex-1 w-full focus:outline-none"),
              attribute.value(model.filter),
              attribute.type_("search"),
              attribute.placeholder("Press / to search..."),
              event.on_input(UserTypedSearch),
            ]),
          ],
        ),
      ]),
      view_chapters(model.filtered, model.filter, model.route),
    ],
    main: case model.route {
      route.Index -> []
      route.NotFound -> []
      route.Chapter(chapter:) -> {
        case list.find(model.chapters, fn(c) { c.route == chapter }) {
          Ok(chapter) -> [chapter.view(chapter)]
          Error(_) -> []
        }
      }
      route.Story(story:, ..) -> [element.element(story <> "-story", [], [])]
    },
  )
}

fn view_chapters(
  chapters: List(Chapter),
  filter: String,
  current: Route,
) -> Element(msg) {
  html.nav([attribute.class("space-y-4")], {
    list.map(chapters, fn(chapter) {
      view_nav_chapter(chapter, filter, current)
    })
  })
}

fn view_nav_chapter(
  chapter: Chapter,
  filter: String,
  current: Route,
) -> Element(msg) {
  let route = route.Chapter(chapter: chapter.route)
  let classes =
    attribute.classes([
      #("font-semibold hover:underline", True),
      #("text-blue-500", route == current),
    ])

  html.div([attribute.class("space-y-2")], [
    html.a([route.href(route), classes], [html.text(chapter.title)]),
    html.ul([], {
      list.map(chapter.stories, fn(story) {
        html.li([attribute.class("pl-2")], [
          view_nav_story(chapter, story, filter, current),
        ])
      })
    }),
  ])
}

fn view_nav_story(
  chapter: Chapter,
  story: Story,
  filter: String,
  current: Route,
) -> Element(msg) {
  let route = route.Story(chapter: chapter.route, story: story.route)
  let classes =
    attribute.classes([
      #("hover:underline", True),
      #("text-blue-500", route == current),
    ])

  html.a(
    [route.href(route), classes],
    case split_on_filter(story.title, filter) {
      #(_, "", "") -> [html.text(story.title)]
      #(before, filter, after) -> [
        html.text(before),
        html.span([attribute.class("underline text-blue-500")], [
          html.text(filter),
        ]),
        html.text(after),
      ]
    },
  )
}

//

fn split_on_filter(title: String, filter: String) -> #(String, String, String) {
  // We want to support case-insensitive searching so we need to convert both the
  // title and the filter to lowercase first.
  case string.split_once(string.lowercase(title), string.lowercase(filter)) {
    Error(_) -> #(title, "", "")
    Ok(#(_, after)) -> {
      let before =
        string.drop_end(title, string.length(filter) + string.length(after))

      let filter =
        string.slice(title, string.length(before), string.length(filter))

      let after =
        string.slice(
          title,
          string.length(before) + string.length(filter),
          string.length(title),
        )

      #(before, filter, after)
    }
  }
}
