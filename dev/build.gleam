import gleam/json
import gleam/string
import simplifile

pub fn main() -> Nil {
  let assert Ok(styles) = simplifile.read("priv/fable.css")
  let styles = styles |> json.string |> json.to_string

  let module_path = "src/fable.ffi.mjs"
  let assert Ok(module) = simplifile.read(module_path)
  let assert [before, rest] = string.split(module, "// <<INJECT STYLES>>\n")
  let assert [_, after] = string.split(rest, "// <<END STYLES>>")

  let assert Ok(_) =
    simplifile.write(
      to: module_path,
      contents: before
        <> "// <<INJECT STYLES>>\nconst stylesheet = "
        <> styles
        <> ";\n\n// <<END STYLES>>"
        <> after,
    )

  Nil
}
