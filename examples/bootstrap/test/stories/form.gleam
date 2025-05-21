import lustre/attribute
import lustre/dev/fable
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

fn input(
  value value: String,
  addon addon: #(String, Bool),
  on_input handle_input: fn(String) -> msg,
) -> Element(msg) {
  let aside = {
    html.span([attribute.class("input-group-text")], [html.text(addon.0)])
  }

  html.div([attribute.class("input-group mb-3")], [
    case addon.1 {
      True -> element.none()
      False -> aside
    },
    html.input([
      attribute.value(value),
      attribute.type_("text"),
      attribute.class("form-control"),
      event.on_input(handle_input),
    ]),
    case addon.1 {
      True -> aside
      False -> element.none()
    },
  ])
}

pub fn addon_story() {
  // Start a new story, give it a nice title
  use <- fable.story("Input addons")

  // Configure the state for our story. Each piece of state gets its own control
  // in the control panel, but can also be read and updated from the view too.
  use value, set_value <- fable.input("Value")
  use addon, _ <- fable.input("Addon")
  use flip, _ <- fable.checkbox("Flip addon")

  // The actual view, the thing we want to demo.
  let view = fn(model) {
    input(
      value: value(model),
      addon: #(addon(model), flip(model)),
      on_input: set_value,
    )
  }

  // And render the scene!
  fable.scene(view)
}
