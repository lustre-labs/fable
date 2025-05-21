import gleam/string

/// This `escape` function will work on all targets, beware that the version
/// specifically optimised for Erlang will be _way faster_ than this one when
/// running on the BEAM. That's why this fallback implementation is only ever
/// used when running on the JS backend.
///
pub fn escape(text: String) -> String {
  do_escape(text)
}

@external(javascript, "../../houdini.ffi.mjs", "do_escape")
fn do_escape(text: String) -> String {
  text
  |> string.replace(">", "&gt;")
  |> string.replace("<", "&lt;")
  |> string.replace("&", "&amp;")
  |> string.replace("'", "&#39;")
  |> string.replace("\"", "&quot;")
}
