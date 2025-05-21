import gleam/uri.{type Uri}
import lustre/attribute.{type Attribute}

pub type Route {
  Index
  Chapter(chapter: String)
  Story(chapter: String, story: String)
  NotFound
}

///
///
pub fn parse(uri: Uri) -> Route {
  case uri.path_segments(uri.path) {
    [] -> Index
    ["chapter", chapter] -> Chapter(chapter:)
    ["chapter", chapter, "story", story] -> Story(chapter:, story:)
    _ -> NotFound
  }
}

///
///
pub fn href(route: Route) -> Attribute(msg) {
  attribute.href(case route {
    Index -> "/"
    Chapter(chapter:) -> "/chapter/" <> chapter
    Story(chapter:, story:) -> "/chapter/" <> chapter <> "/story/" <> story
    NotFound -> "#"
  })
}
