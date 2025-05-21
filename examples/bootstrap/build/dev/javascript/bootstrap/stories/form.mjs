import * as $attribute from "../../lustre/lustre/attribute.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import * as $html from "../../lustre/lustre/element/html.mjs";
import * as $event from "../../lustre/lustre/event.mjs";
import * as $fable from "../../lustre_fable/lustre/dev/fable.mjs";
import { toList } from "../gleam.mjs";

function input(value, addon, handle_input) {
  let aside = $html.span(
    toList([$attribute.class$("input-group-text")]),
    toList([$html.text(addon[0])]),
  );
  return $html.div(
    toList([$attribute.class$("input-group mb-3")]),
    toList([
      (() => {
        let $ = addon[1];
        if ($) {
          return $element.none();
        } else {
          return aside;
        }
      })(),
      $html.input(
        toList([
          $attribute.value(value),
          $attribute.type_("text"),
          $attribute.class$("form-control"),
          $event.on_input(handle_input),
        ]),
      ),
      (() => {
        let $ = addon[1];
        if ($) {
          return aside;
        } else {
          return $element.none();
        }
      })(),
    ]),
  );
}

export function addon_story() {
  return $fable.story(
    "Input addons",
    () => {
      return $fable.input(
        "Value",
        (value, set_value) => {
          return $fable.input(
            "Addon",
            (addon, _) => {
              return $fable.checkbox(
                "Flip addon",
                (flip, _) => {
                  let view = (model) => {
                    return input(
                      value(model),
                      [addon(model), flip(model)],
                      set_value,
                    );
                  };
                  return $fable.scene(view);
                },
              );
            },
          );
        },
      );
    },
  );
}
