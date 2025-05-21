import * as $function from "../../gleam_stdlib/gleam/function.mjs";
import { identity as coerce } from "../../gleam_stdlib/gleam/function.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../../gleam_stdlib/gleam/string_tree.mjs";
import { toList } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import * as $mutable_map from "../lustre/internals/mutable_map.mjs";
import * as $events from "../lustre/vdom/events.mjs";
import * as $vnode from "../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../lustre/vdom/vnode.mjs";

export function element(tag, attributes, children) {
  return $vnode.element(
    "",
    $function.identity,
    "",
    tag,
    attributes,
    children,
    $mutable_map.new$(),
    false,
    false,
  );
}

export function namespaced(namespace, tag, attributes, children) {
  return $vnode.element(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    children,
    $mutable_map.new$(),
    false,
    false,
  );
}

export function advanced(
  namespace,
  tag,
  attributes,
  children,
  self_closing,
  void$
) {
  return $vnode.element(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    children,
    $mutable_map.new$(),
    self_closing,
    void$,
  );
}

export function text(content) {
  return $vnode.text("", $function.identity, content);
}

export function none() {
  return $vnode.text("", $function.identity, "");
}

function count_fragment_children(loop$children, loop$count) {
  while (true) {
    let children = loop$children;
    let count = loop$count;
    if (children.hasLength(0)) {
      return count;
    } else if (children.atLeastLength(1) && children.head instanceof Fragment) {
      let children_count = children.head.children_count;
      let rest = children.tail;
      loop$children = rest;
      loop$count = count + children_count;
    } else {
      let rest = children.tail;
      loop$children = rest;
      loop$count = count + 1;
    }
  }
}

export function fragment(children) {
  return $vnode.fragment(
    "",
    $function.identity,
    children,
    $mutable_map.new$(),
    count_fragment_children(children, 0),
  );
}

export function unsafe_raw_html(namespace, tag, attributes, inner_html) {
  return $vnode.unsafe_inner_html(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    inner_html,
  );
}

export function map(element, f) {
  let mapper = coerce($events.compose_mapper(coerce(f), element.mapper));
  if (element instanceof Fragment) {
    let children = element.children;
    let keyed_children = element.keyed_children;
    let _record = element;
    return new Fragment(
      _record.kind,
      _record.key,
      mapper,
      coerce(children),
      coerce(keyed_children),
      _record.children_count,
    );
  } else if (element instanceof Element) {
    let attributes = element.attributes;
    let children = element.children;
    let keyed_children = element.keyed_children;
    let _record = element;
    return new Element(
      _record.kind,
      _record.key,
      mapper,
      _record.namespace,
      _record.tag,
      coerce(attributes),
      coerce(children),
      coerce(keyed_children),
      _record.self_closing,
      _record.void,
    );
  } else if (element instanceof UnsafeInnerHtml) {
    let attributes = element.attributes;
    let _record = element;
    return new UnsafeInnerHtml(
      _record.kind,
      _record.key,
      mapper,
      _record.namespace,
      _record.tag,
      coerce(attributes),
      _record.inner_html,
    );
  } else {
    return coerce(element);
  }
}

export function to_string(element) {
  return $vnode.to_string(element);
}

export function to_document_string(el) {
  let _pipe = $vnode.to_string(
    (() => {
      if (el instanceof Element && el.tag === "html") {
        return el;
      } else if (el instanceof Element && el.tag === "head") {
        return element("html", toList([]), toList([el]));
      } else if (el instanceof Element && el.tag === "body") {
        return element("html", toList([]), toList([el]));
      } else {
        return element(
          "html",
          toList([]),
          toList([element("body", toList([]), toList([el]))]),
        );
      }
    })(),
  );
  return ((_capture) => { return $string.append("<!doctype html>\n", _capture); })(
    _pipe,
  );
}

export function to_string_tree(element) {
  return $vnode.to_string_tree(element);
}

export function to_document_string_tree(el) {
  let _pipe = $vnode.to_string_tree(
    (() => {
      if (el instanceof Element && el.tag === "html") {
        return el;
      } else if (el instanceof Element && el.tag === "head") {
        return element("html", toList([]), toList([el]));
      } else if (el instanceof Element && el.tag === "body") {
        return element("html", toList([]), toList([el]));
      } else {
        return element(
          "html",
          toList([]),
          toList([element("body", toList([]), toList([el]))]),
        );
      }
    })(),
  );
  return $string_tree.prepend(_pipe, "<!doctype html>\n");
}

export function to_readable_string(el) {
  return $vnode.to_snapshot(el);
}
