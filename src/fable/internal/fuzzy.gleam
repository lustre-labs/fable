//// `NoRedInk/elm-simple-fuzzy`-inspired primitive fuzzy search.

import gleam/list
import gleam/string

///
///
pub fn filter(candidates: List(String), query: String) -> List(String) {
  list.filter(candidates, matches(_, query))
}

///
///
pub fn matches(candidate: String, query: String) -> Bool {
  match_characters(normalize(candidate), normalize(query))
}

fn normalize(text: String) -> List(Int) {
  text
  |> string.lowercase
  |> string.to_utf_codepoints
  |> list.map(string.utf_codepoint_to_int)
  |> list.filter(fn(char) {
    // [a-z0-9]
    char >= 97 && char <= 122 || char >= 48 && char <= 57
  })
}

fn match_characters(candidate: List(Int), query: List(Int)) -> Bool {
  case candidate, query {
    _, [] -> True
    [], _ -> False
    [char, ..remaining], [wanted, ..rest] if char == wanted ->
      match_characters(remaining, rest)
    [_, ..remaining], _ -> match_characters(remaining, query)
  }
}
