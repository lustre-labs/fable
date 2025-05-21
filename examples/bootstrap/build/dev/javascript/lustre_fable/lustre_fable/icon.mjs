import * as $attribute from "../../lustre/lustre/attribute.mjs";
import { attribute } from "../../lustre/lustre/attribute.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import * as $html from "../../lustre/lustre/element/html.mjs";
import * as $svg from "../../lustre/lustre/element/svg.mjs";
import { toList, prepend as listPrepend } from "../gleam.mjs";

export function sidebar(attributes) {
  return $html.svg(
    listPrepend(
      attribute("width", "24"),
      listPrepend(
        attribute("height", "24"),
        listPrepend(
          attribute("viewBox", "0 0 24 24"),
          listPrepend(
            attribute("fill", "none"),
            listPrepend(
              attribute("stroke", "currentColor"),
              listPrepend(
                attribute("stroke-width", "2"),
                listPrepend(
                  attribute("stroke-linecap", "round"),
                  listPrepend(attribute("stroke-linejoin", "round"), attributes),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
    toList([
      $svg.rect(
        toList([
          attribute("width", "18"),
          attribute("height", "18"),
          attribute("x", "3"),
          attribute("y", "3"),
          attribute("rx", "2"),
        ]),
      ),
      $svg.path(toList([attribute("d", "M15 3v18")])),
    ]),
  );
}
