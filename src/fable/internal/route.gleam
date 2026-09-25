// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/uri.{type Uri}
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}
import modem

// TYPES -----------------------------------------------------------------------

///
/// 
pub type Route {
  Index
  SceneSelect(chapter: String, story: String)
  SceneDisplay(chapter: String, story: String, scene: Int)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
/// 
pub fn from_uri(request: Uri) -> Result(Route, Uri) {
  let assert Ok(location) = modem.initial_uri()

  use <- bool.guard(request.host != None && request.host != location.host, {
    Error(request)
  })

  use <- bool.guard(request.port != None && request.port != location.port, {
    Error(request)
  })

  case uri.path_segments(request.path) {
    [chapter, story, scene] ->
      case int.parse(scene) {
        Ok(scene) -> Ok(SceneDisplay(chapter:, story:, scene:))
        Error(_) -> Ok(SceneSelect(chapter:, story:))
      }

    [chapter, story] -> Ok(SceneSelect(chapter:, story:))

    _ -> Ok(Index)
  }
}

fn is_external(request: Option(a), location: Option(a)) -> Bool {
  case request {
    None -> False
    Some(_) -> request != location
  }
}

// CONVERSIONS -----------------------------------------------------------------

pub fn path(route: Route) -> String {
  case route {
    Index -> "/"

    SceneSelect(chapter:, story:) -> "/" <> chapter <> "/" <> story

    SceneDisplay(chapter:, story:, scene:) ->
      "/" <> chapter <> "/" <> story <> "/" <> int.to_string(scene)
  }
}

///
/// 
pub fn href(route: Route) -> Attribute(message) {
  attribute.href(path(route))
}

// EFFECTS ---------------------------------------------------------------------

pub fn push(route: Route) -> Effect(message) {
  modem.push(path(route), None, None)
}
