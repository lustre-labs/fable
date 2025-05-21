import * as $attribute from "../../lustre/lustre/attribute.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import * as $html from "../../lustre/lustre/element/html.mjs";
import * as $fable from "../../lustre_fable/lustre/dev/fable.mjs";
import { toList } from "../gleam.mjs";

export function headings_story() {
  return $fable.story(
    "Headings",
    () => {
      let view = (_) => {
        return $element.fragment(
          toList([
            $html.h1(toList([]), toList([$html.text("h1. Bootstrap heading")])),
            $html.h2(toList([]), toList([$html.text("h2. Bootstrap heading")])),
            $html.h3(toList([]), toList([$html.text("h3. Bootstrap heading")])),
            $html.h4(toList([]), toList([$html.text("h4. Bootstrap heading")])),
            $html.h5(toList([]), toList([$html.text("h5. Bootstrap heading")])),
            $html.h6(toList([]), toList([$html.text("h6. Bootstrap heading")])),
          ]),
        );
      };
      return $fable.scene(view);
    },
  );
}

export function customising_headings_story() {
  return $fable.story(
    "Customising headings",
    () => {
      let view = (_) => {
        return $element.fragment(
          toList([
            $html.h3(
              toList([]),
              toList([
                $html.text("Fancy display heading"),
                $html.small(
                  toList([$attribute.class$("text-body-secondary")]),
                  toList([$html.text("With faded secondary text")]),
                ),
              ]),
            ),
          ]),
        );
      };
      return $fable.scene(view);
    },
  );
}

export function display_headings_story() {
  return $fable.story(
    "Display headings",
    () => {
      let view = (_) => {
        return $element.fragment(
          toList([
            $html.h1(
              toList([$attribute.class$("display-1")]),
              toList([$html.text("Display 1")]),
            ),
            $html.h1(
              toList([$attribute.class$("display-2")]),
              toList([$html.text("Display 2")]),
            ),
            $html.h1(
              toList([$attribute.class$("display-3")]),
              toList([$html.text("Display 3")]),
            ),
            $html.h1(
              toList([$attribute.class$("display-4")]),
              toList([$html.text("Display 4")]),
            ),
            $html.h1(
              toList([$attribute.class$("display-5")]),
              toList([$html.text("Display 5")]),
            ),
            $html.h1(
              toList([$attribute.class$("display-6")]),
              toList([$html.text("Display 6")]),
            ),
          ]),
        );
      };
      return $fable.scene(view);
    },
  );
}

export function inline_text_story() {
  return $fable.story(
    "Inline text elements",
    () => {
      let view = (_) => {
        return $element.fragment(
          toList([
            $html.p(
              toList([]),
              toList([
                $html.text("You can use the mark tag to "),
                $html.mark(toList([]), toList([$html.text("highlight")])),
                $html.text(" text."),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.del(
                  toList([]),
                  toList([
                    $html.text(
                      "This line of text is meant to be treated as deleted text.",
                    ),
                  ]),
                ),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.s(
                  toList([]),
                  toList([
                    $html.text(
                      "This line of text is meant to be treated as no longer accurate.",
                    ),
                  ]),
                ),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.ins(
                  toList([]),
                  toList([
                    $html.text(
                      "This line of text is meant to be treated as an addition to the document.",
                    ),
                  ]),
                ),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.u(
                  toList([]),
                  toList([
                    $html.text("This line of text will render as underlined."),
                  ]),
                ),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.small(
                  toList([]),
                  toList([
                    $html.text(
                      "This line of text is meant to be treated as fine print.",
                    ),
                  ]),
                ),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.strong(
                  toList([]),
                  toList([$html.text("This line rendered as bold text.")]),
                ),
              ]),
            ),
            $html.p(
              toList([]),
              toList([
                $html.em(
                  toList([]),
                  toList([$html.text("This line rendered as italicized text.")]),
                ),
              ]),
            ),
          ]),
        );
      };
      return $fable.scene(view);
    },
  );
}
