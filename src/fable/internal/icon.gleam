import lustre/attribute.{type Attribute, attribute}
import lustre/element.{type Element}
import lustre/element/svg

pub fn chevron_left(attributes: List(Attribute(message))) -> Element(message) {
  svg.svg(
    [
      attribute.class("lucide lucide-chevron-left"),
      attribute("stroke-linejoin", "round"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-width", "2"),
      attribute("stroke", "currentColor"),
      attribute("fill", "none"),
      attribute("viewBox", "0 0 24 24"),
      attribute("height", "24"),
      attribute("width", "24"),
      ..attributes
    ],
    [svg.path([attribute("d", "m15 18-6-6 6-6")])],
  )
}

pub fn chevron_double_left(
  attributes: List(Attribute(message)),
) -> Element(message) {
  svg.svg(
    [
      attribute.class("lucide lucide-chevrons-left"),
      attribute("stroke-linejoin", "round"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-width", "2"),
      attribute("stroke", "currentColor"),
      attribute("fill", "none"),
      attribute("viewBox", "0 0 24 24"),
      attribute("height", "24"),
      attribute("width", "24"),
      ..attributes
    ],
    [
      svg.path([attribute("d", "m11 17-5-5 5-5")]),
      svg.path([attribute("d", "m18 17-5-5 5-5")]),
    ],
  )
}

pub fn chevron_right(attributes: List(Attribute(message))) -> Element(message) {
  svg.svg(
    [
      attribute.class("lucide lucide-chevron-right"),
      attribute("stroke-linejoin", "round"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-width", "2"),
      attribute("stroke", "currentColor"),
      attribute("fill", "none"),
      attribute("viewBox", "0 0 24 24"),
      attribute("height", "24"),
      attribute("width", "24"),
      ..attributes
    ],
    [svg.path([attribute("d", "m9 18 6-6-6-6")])],
  )
}

pub fn chevron_double_right(
  attributes: List(Attribute(message)),
) -> Element(message) {
  svg.svg(
    [
      attribute.class("lucide lucide-chevrons-right"),
      attribute("stroke-linejoin", "round"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-width", "2"),
      attribute("stroke", "currentColor"),
      attribute("fill", "none"),
      attribute("viewBox", "0 0 24 24"),
      attribute("height", "24"),
      attribute("width", "24"),
      ..attributes
    ],
    [
      svg.path([attribute("d", "m6 17 5-5-5-5")]),
      svg.path([attribute("d", "m13 17 5-5-5-5")]),
    ],
  )
}

pub fn refresh(attributes: List(Attribute(message))) -> Element(message) {
  svg.svg(
    [
      attribute.class("lucide lucide-refresh-ccw"),
      attribute("stroke-linejoin", "round"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-width", "2"),
      attribute("stroke", "currentColor"),
      attribute("fill", "none"),
      attribute("viewBox", "0 0 24 24"),
      attribute("height", "24"),
      attribute("width", "24"),
      ..attributes
    ],
    [
      svg.path([
        attribute("d", "M21 12a9 9 0 0 0-9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"),
      ]),
      svg.path([attribute("d", "M3 3v5h5")]),
      svg.path([
        attribute("d", "M3 12a9 9 0 0 0 9 9 9.75 9.75 0 0 0 6.74-2.74L21 16"),
      ]),
      svg.path([attribute("d", "M16 16h5v5")]),
    ],
  )
}
