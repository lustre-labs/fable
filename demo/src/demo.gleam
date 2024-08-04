import fable
import lustre
import ui/field

pub fn main() {
  fable.book()
  |> fable.add_story("components", field.field_story())
  |> fable.add_story("components", field.field_with_label_story())
  |> fable.add_story("components", field.field_with_message_story())
  |> fable.to_app
  |> lustre.start("#app", Nil)
}
