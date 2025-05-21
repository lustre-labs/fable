import gleam/dict.{type Dict}
import gleam/result
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre_fable/chapter.{type Chapter}
import lustre_fable/route.{type Route}

pub fn breadcrumb(route: Route, chapters: Dict(String, Chapter)) -> Element(msg) {
  let separator =
    html.span([attribute.class("text-stone-400 user-select-none")], [
      html.text("/"),
    ])

  let link = fn(route, label) {
    html.a([route.href(route)], [html.text(label)])
  }

  let current = fn(label) {
    html.a([attribute.href("#"), attribute.aria_current("page")], [
      html.text(label),
    ])
  }

  html.nav([attribute.class("flex gap-4")], case route {
    route.Index | route.NotFound -> []

    route.Chapter(chapter:) ->
      case dict.get(chapters, chapter) {
        Error(_) -> []
        Ok(c) -> [separator, current(c.title)]
      }

    route.Story(chapter:, story:) -> {
      let c = dict.get(chapters, chapter)
      let s = c |> result.then(chapter.story(_, story))

      case c, s {
        Ok(c), Ok(s) -> [
          separator,
          link(route.Chapter(chapter:), c.title),
          separator,
          current(s.title),
        ]

        Ok(c), _ -> [separator, current(c.title)]

        _, _ -> []
      }
    }
  })
}
