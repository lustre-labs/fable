import * as $fable from "../lustre_fable/lustre/dev/fable.mjs";
import { toList } from "./gleam.mjs";
import * as $button from "./stories/button.mjs";
import * as $form from "./stories/form.mjs";
import * as $typography from "./stories/typography.mjs";

const bootstrap = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.6/dist/css/bootstrap.min.css";

export function main() {
  let _pipe = $fable.book("Bootstrap");
  let _pipe$1 = $fable.external_stylesheet(_pipe, bootstrap);
  let _pipe$2 = $fable.chapter(
    _pipe$1,
    "Content",
    toList([
      $typography.headings_story(),
      $typography.customising_headings_story(),
      $typography.display_headings_story(),
      $typography.inline_text_story(),
    ]),
  );
  let _pipe$3 = $fable.chapter(
    _pipe$2,
    "Components",
    toList([$button.variants_story()]),
  );
  let _pipe$4 = $fable.chapter(_pipe$3, "Forms", toList([$form.addon_story()]));
  return $fable.start(_pipe$4);
}
