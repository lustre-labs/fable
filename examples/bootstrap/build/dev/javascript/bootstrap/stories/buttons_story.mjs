import * as $attribute from "../../lustre/lustre/attribute.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import * as $html from "../../lustre/lustre/element/html.mjs";
import * as $fable from "../../lustre_fable/lustre/dev/fable.mjs";
import { toList } from "../gleam.mjs";

export function button_variants_story() {
  return $fable.story(
    "Button variants",
    () => {
      let view = (_) => {
        return $element.fragment(
          toList([
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-primary"),
              ]),
              toList([$html.text("Primary")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-secondary"),
              ]),
              toList([$html.text("Secondary")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-success"),
              ]),
              toList([$html.text("Success")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-danger"),
              ]),
              toList([$html.text("Danger")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-warning"),
              ]),
              toList([$html.text("Warning")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-info"),
              ]),
              toList([$html.text("Info")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-light"),
              ]),
              toList([$html.text("Light")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-dark"),
              ]),
              toList([$html.text("Dark")]),
            ),
            $html.button(
              toList([
                $attribute.type_("button"),
                $attribute.class$("btn btn-link"),
              ]),
              toList([$html.text("Link")]),
            ),
          ]),
        );
      };
      return $fable.scene(view);
    },
  );
}
