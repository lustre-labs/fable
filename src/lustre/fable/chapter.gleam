// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/fable/route
import lustre/fable/story.{type Story, type StoryConfig}
import lustre/fable/ui/icon

// TYPES -----------------------------------------------------------------------

pub type Chapter {
  Chapter(title: String, route: String, stories: List(Story))
}

///
///
pub fn init(
  title: String,
  stories: List(StoryConfig),
  stylesheets: List(String),
  external_stylesheets: List(String),
) -> Result(Chapter, lustre.Error) {
  let assert Ok(re) = regexp.from_string("[^a-zA-Z0-9]+")
  let route =
    title
    |> regexp.replace(re, _, "-")
    |> string.lowercase

  stories
  |> list.try_map(story.register(_, stylesheets, external_stylesheets))
  |> result.map(Chapter(title:, route:, stories: _))
}

///
///
pub fn story(chapter: Chapter, slug: String) -> Result(Story, Nil) {
  use story <- list.find(chapter.stories)

  story.route == slug
}

//

pub fn view(chapter: Chapter) -> Element(msg) {
  html.div(
    [attribute.class("inline-grid w-full justify-center gap-4 px-2 py-4")],
    {
      use story <- list.map(chapter.stories)
      let route = route.Story(chapter: chapter.route, story: story.route)

      element.fragment([
        html.section([attribute.class("w-full min-w-max")], [
          html.h2([attribute.class("text-lg font-semibold underline")], [
            html.a([route.href(route)], [html.text(story.title)]),
          ]),
          html.div([attribute.class("relative")], [
            icon.lock([
              attribute.class("absolute -top-2 right-2 size-3 bg-white"),
            ]),
            element.element(
              story.scene,
              [
                attribute.class("block"),
                attribute.class(
                  "rounded p-4 border-dashed border border-blue-500/0",
                ),
                attribute.class("transition hover:border-blue-500/100"),
              ],
              [],
            ),
          ]),
        ]),
        html.hr([attribute.class("w-full h-px bg-stone-200")]),
      ])
    },
  )
}
