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
