import lustre/attribute
import lustre/dev/fable
import lustre/element
import lustre/element/html

pub fn headings_story() {
  use <- fable.story("Headings")
  let view = fn(_) {
    element.fragment([
      html.h1([], [html.text("h1. Bootstrap heading")]),
      html.h2([], [html.text("h2. Bootstrap heading")]),
      html.h3([], [html.text("h3. Bootstrap heading")]),
      html.h4([], [html.text("h4. Bootstrap heading")]),
      html.h5([], [html.text("h5. Bootstrap heading")]),
      html.h6([], [html.text("h6. Bootstrap heading")]),
    ])
  }

  fable.scene(view)
}

pub fn customising_headings_story() {
  use <- fable.story("Customising headings")
  let view = fn(_) {
    element.fragment([
      html.h3([], [
        html.text("Fancy display heading"),
        html.small([attribute.class("text-body-secondary")], [
          html.text("With faded secondary text"),
        ]),
      ]),
    ])
  }

  fable.scene(view)
}

pub fn display_headings_story() {
  use <- fable.story("Display headings")
  let view = fn(_) {
    element.fragment([
      html.h1([attribute.class("display-1")], [html.text("Display 1")]),
      html.h1([attribute.class("display-2")], [html.text("Display 2")]),
      html.h1([attribute.class("display-3")], [html.text("Display 3")]),
      html.h1([attribute.class("display-4")], [html.text("Display 4")]),
      html.h1([attribute.class("display-5")], [html.text("Display 5")]),
      html.h1([attribute.class("display-6")], [html.text("Display 6")]),
    ])
  }

  fable.scene(view)
}

pub fn inline_text_story() {
  use <- fable.story("Inline text elements")
  let view = fn(_) {
    element.fragment([
      html.p([], [
        html.text("You can use the mark tag to "),
        html.mark([], [html.text("highlight")]),
        html.text(" text."),
      ]),
      html.p([], [
        html.del([], [
          html.text("This line of text is meant to be treated as deleted text."),
        ]),
      ]),
      html.p([], [
        html.s([], [
          html.text(
            "This line of text is meant to be treated as no longer accurate.",
          ),
        ]),
      ]),
      html.p([], [
        html.ins([], [
          html.text(
            "This line of text is meant to be treated as an addition to the document.",
          ),
        ]),
      ]),
      html.p([], [
        html.u([], [html.text("This line of text will render as underlined.")]),
      ]),
      html.p([], [
        html.small([], [
          html.text("This line of text is meant to be treated as fine print."),
        ]),
      ]),
      html.p([], [
        html.strong([], [html.text("This line rendered as bold text.")]),
      ]),
      html.p([], [
        html.em([], [html.text("This line rendered as italicized text.")]),
      ]),
    ])
  }

  fable.scene(view)
}
