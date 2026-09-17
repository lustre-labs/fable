// IMPORTS ---------------------------------------------------------------------

import demo/counter_story
import fable

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let book =
    fable.book("Demo", [
      fable.chapter("A chapter", [
        counter_story.setup(),
      ]),
    ])

  let assert Ok(_) = fable.start(book, "#app")
}
