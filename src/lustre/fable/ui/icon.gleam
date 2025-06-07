import lustre/attribute.{type Attribute, attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg

pub fn sidebar(attributes: List(Attribute(msg))) -> Element(msg) {
  html.svg(
    [
      attribute("width", "24"),
      attribute("height", "24"),
      attribute("viewBox", "0 0 24 24"),
      attribute("fill", "none"),
      attribute("stroke", "currentColor"),
      attribute("stroke-width", "2"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-linejoin", "round"),
      ..attributes
    ],
    [
      svg.rect([
        attribute("width", "18"),
        attribute("height", "18"),
        attribute("x", "3"),
        attribute("y", "3"),
        attribute("rx", "2"),
      ]),
      svg.path([attribute("d", "M15 3v18")]),
    ],
  )
}

pub fn hamburger(attributes: List(Attribute(msg))) -> Element(msg) {
  html.svg(
    [
      attribute("width", "24"),
      attribute("height", "24"),
      attribute("viewBox", "0 0 24 24"),
      attribute("fill", "none"),
      attribute("stroke", "currentColor"),
      attribute("stroke-width", "2"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-linejoin", "round"),
      ..attributes
    ],
    [
      svg.path([attribute("d", "M4 12h16")]),
      svg.path([attribute("d", "M4 18h16")]),
      svg.path([attribute("d", "M4 6h16")]),
    ],
  )
}

pub fn search(attributes: List(Attribute(msg))) -> Element(msg) {
  html.svg(
    [
      attribute("width", "24"),
      attribute("height", "24"),
      attribute("viewBox", "0 0 24 24"),
      attribute("fill", "none"),
      attribute("stroke", "currentColor"),
      attribute("stroke-width", "2"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-linejoin", "round"),
      ..attributes
    ],
    [
      svg.path([attribute("d", "m21 21-4.34-4.34")]),
      svg.circle([
        attribute("cx", "11"),
        attribute("cy", "11"),
        attribute("r", "8"),
      ]),
    ],
  )
}

pub fn link(attributes: List(Attribute(msg))) -> Element(msg) {
  html.svg(
    [
      attribute("width", "24"),
      attribute("height", "24"),
      attribute("viewBox", "0 0 24 24"),
      attribute("fill", "none"),
      attribute("stroke", "currentColor"),
      attribute("stroke-width", "2"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-linejoin", "round"),
      ..attributes
    ],
    [
      svg.path([
        attribute("d", {
          "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"
        }),
      ]),
      svg.path([
        attribute("d", {
          "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"
        }),
      ]),
    ],
  )
}

pub fn lock(attributes: List(Attribute(msg))) -> Element(msg) {
  html.svg(
    [
      attribute("width", "24"),
      attribute("height", "24"),
      attribute("viewBox", "0 0 24 24"),
      attribute("fill", "none"),
      attribute("stroke", "currentColor"),
      attribute("stroke-width", "2"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-linejoin", "round"),
      ..attributes
    ],
    [
      svg.rect([
        attribute("width", "18"),
        attribute("height", "11"),
        attribute("x", "3"),
        attribute("y", "11"),
        attribute("rx", "2"),
        attribute("ry", "2"),
      ]),
      svg.path([attribute("d", "M7 11V7a5 5 0 0 1 10 0v4")]),
    ],
  )
}
