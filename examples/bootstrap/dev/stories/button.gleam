import lustre/attribute
import lustre/dev/fable
import lustre/element
import lustre/element/html

pub fn variants_story() {
  use <- fable.story("Button variants")
  let view = fn(_) {
    element.fragment([
      html.button(
        [attribute.type_("button"), attribute.class("btn btn-primary")],
        [html.text("Primary")],
      ),
      html.button(
        [attribute.type_("button"), attribute.class("btn btn-secondary")],
        [html.text("Secondary")],
      ),
      html.button(
        [attribute.type_("button"), attribute.class("btn btn-success")],
        [html.text("Success")],
      ),
      html.button(
        [attribute.type_("button"), attribute.class("btn btn-danger")],
        [html.text("Danger")],
      ),
      html.button(
        [attribute.type_("button"), attribute.class("btn btn-warning")],
        [html.text("Warning")],
      ),
      html.button([attribute.type_("button"), attribute.class("btn btn-info")], [
        html.text("Info"),
      ]),
      html.button(
        [attribute.type_("button"), attribute.class("btn btn-light")],
        [html.text("Light")],
      ),
      html.button([attribute.type_("button"), attribute.class("btn btn-dark")], [
        html.text("Dark"),
      ]),
      html.button([attribute.type_("button"), attribute.class("btn btn-link")], [
        html.text("Link"),
      ]),
    ])
  }

  fable.scene(view)
}
