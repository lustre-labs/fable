// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/uri.{type Uri}
import lustre/attribute.{type Attribute}
import modem

// TYPES -----------------------------------------------------------------------

///
/// 
pub type Route {
  Book
  Chapter(name: String)
  Story(chapter: String, name: String)
  Scene(chapter: String, story: String, name: String)
  Unknown(path: String)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
/// 
pub fn from_uri(request: Uri) -> Result(Route, Uri) {
  let assert Ok(location) = modem.initial_uri()

  use <- bool.guard(request.host != location.host, Error(request))
  use <- bool.guard(request.port != location.port, Error(request))

  case uri.path_segments(request.path) {
    [] -> Ok(Book)
    [name] -> Ok(Chapter(name:))
    [chapter, name] -> Ok(Story(chapter:, name:))
    [chapter, story, name] -> Ok(Scene(chapter:, story:, name:))

    _ -> Ok(Unknown(path: request.path))
  }
}

// CONVERSIONS -----------------------------------------------------------------

pub fn to_path(route: Route) -> String {
  case route {
    Book | Unknown(..) -> "/"
    Chapter(name:) -> "/" <> name
    Story(chapter:, name:) -> "/" <> chapter <> "/" <> name
    Scene(chapter:, story:, name:) ->
      "/" <> chapter <> "/" <> story <> "/" <> name
  }
}

pub fn to_key(route: Route) -> Result(String, Nil) {
  case route {
    Scene(chapter:, story:, name:) ->
      Ok("/" <> chapter <> "/" <> story <> "/" <> name)

    _ -> Error(Nil)
  }
}

///
/// 
pub fn href(route: Route) -> Attribute(message) {
  attribute.href(to_path(route))
}
