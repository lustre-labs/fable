// IMPORTS ---------------------------------------------------------------------

import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn shell(
  chapters: List(#(String, List(String))),
  component: Element(msg),
) -> Element(msg) {
  html.body([attribute.class("flex w-screen h-screen items-stretch")], [
    html.aside(
      [attribute.class("w-[300px] h-full bg-gray-100 overflow-y-scroll")],
      {
        use #(title, stories) <- list.map(chapters)
        html.nav([], [
          html.p([], [html.text(title)]),
          ..list.map(stories, fn(story) {
            html.a([attribute.href("/" <> title <> "/" <> story)], [
              html.text(story),
            ])
          })
        ])
      },
    ),
    html.main([attribute.class("flex-1")], [component]),
  ])
}

pub fn component(
  render: Element(msg),
  controls: List(Element(msg)),
) -> Element(msg) {
  html.div([attribute.class("w-full h-full")], [
    html.div(
      [attribute.class("w-full h-full flex justify-center items-center")],
      [render],
    ),
    html.div([attribute.class("h-[300px] overflow-y-scroll")], controls),
  ])
}
