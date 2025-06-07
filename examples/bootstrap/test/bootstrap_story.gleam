import lustre/dev/fable
import stories/button
import stories/form
import stories/typography

const bootstrap = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.6/dist/css/bootstrap.min.css"

pub fn main() {
  let storybook =
    fable.book("Bootstrap", [
      fable.external_stylesheet(bootstrap),
      fable.chapter("Content", [
        typography.headings_story(),
        typography.customising_headings_story(),
        typography.display_headings_story(),
        typography.inline_text_story(),
      ]),
      fable.chapter("Components", [button.variants_story()]),
      fable.chapter("Forms", [form.addon_story()]),
    ])

  let assert Ok(_) = fable.start(storybook)
}
