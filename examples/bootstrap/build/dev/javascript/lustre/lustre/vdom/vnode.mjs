import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../../../gleam_stdlib/gleam/string_tree.mjs";
import * as $houdini from "../../../houdini/houdini.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $json_object_builder from "../../lustre/internals/json_object_builder.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";

export class Fragment extends $CustomType {
  constructor(kind, key, mapper, children, keyed_children, children_count) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.children = children;
    this.keyed_children = keyed_children;
    this.children_count = children_count;
  }
}

export class Element extends $CustomType {
  constructor(kind, key, mapper, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.keyed_children = keyed_children;
    this.self_closing = self_closing;
    this.void = void$;
  }
}

export class Text extends $CustomType {
  constructor(kind, key, mapper, content) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.content = content;
  }
}

export class UnsafeInnerHtml extends $CustomType {
  constructor(kind, key, mapper, namespace, tag, attributes, inner_html) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.inner_html = inner_html;
  }
}

function is_void_element(tag, namespace) {
  if (namespace === "") {
    if (tag === "area") {
      return true;
    } else if (tag === "base") {
      return true;
    } else if (tag === "br") {
      return true;
    } else if (tag === "col") {
      return true;
    } else if (tag === "embed") {
      return true;
    } else if (tag === "hr") {
      return true;
    } else if (tag === "img") {
      return true;
    } else if (tag === "input") {
      return true;
    } else if (tag === "link") {
      return true;
    } else if (tag === "meta") {
      return true;
    } else if (tag === "param") {
      return true;
    } else if (tag === "source") {
      return true;
    } else if (tag === "track") {
      return true;
    } else if (tag === "wbr") {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}

export function advance(node) {
  if (node instanceof Fragment) {
    let children_count = node.children_count;
    return 1 + children_count;
  } else {
    return 1;
  }
}

function text_to_json(kind, key, content) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "content", content);
  return $json_object_builder.build(_pipe$2);
}

function unsafe_inner_html_to_json(
  kind,
  key,
  namespace,
  tag,
  attributes,
  inner_html
) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "namespace", namespace);
  let _pipe$3 = $json_object_builder.string(_pipe$2, "tag", tag);
  let _pipe$4 = $json_object_builder.list(
    _pipe$3,
    "attributes",
    attributes,
    $vattr.to_json,
  );
  let _pipe$5 = $json_object_builder.string(_pipe$4, "inner_html", inner_html);
  return $json_object_builder.build(_pipe$5);
}

export const fragment_kind = 0;

export function fragment(key, mapper, children, keyed_children, children_count) {
  return new Fragment(
    fragment_kind,
    key,
    mapper,
    children,
    keyed_children,
    children_count,
  );
}

export const element_kind = 1;

export function element(
  key,
  mapper,
  namespace,
  tag,
  attributes,
  children,
  keyed_children,
  self_closing,
  void$
) {
  return new Element(
    element_kind,
    key,
    mapper,
    namespace,
    tag,
    $vattr.prepare(attributes),
    children,
    keyed_children,
    self_closing,
    void$ || is_void_element(tag, namespace),
  );
}

export const text_kind = 2;

export function text(key, mapper, content) {
  return new Text(text_kind, key, mapper, content);
}

export const unsafe_inner_html_kind = 3;

export function unsafe_inner_html(
  key,
  mapper,
  namespace,
  tag,
  attributes,
  inner_html
) {
  return new UnsafeInnerHtml(
    unsafe_inner_html_kind,
    key,
    mapper,
    namespace,
    tag,
    $vattr.prepare(attributes),
    inner_html,
  );
}

function children_to_snapshot_builder(
  loop$html,
  loop$children,
  loop$raw_text,
  loop$indent
) {
  while (true) {
    let html = loop$html;
    let children = loop$children;
    let raw_text = loop$raw_text;
    let indent = loop$indent;
    if (children.atLeastLength(2) &&
    children.head instanceof Text &&
    children.tail.head instanceof Text) {
      let a = children.head.content;
      let b = children.tail.head.content;
      let rest = children.tail.tail;
      loop$html = html;
      loop$children = listPrepend(
        new Text(text_kind, "", $function.identity, a + b),
        rest,
      );
      loop$raw_text = raw_text;
      loop$indent = indent;
    } else if (children.atLeastLength(1) && children.head instanceof Fragment) {
      let child = children.head;
      let rest = children.tail;
      let _pipe = child;
      let _pipe$1 = do_to_snapshot_builder(_pipe, raw_text, indent);
      let _pipe$2 = ((_capture) => {
        return $string_tree.append_tree(html, _capture);
      })(_pipe$1);
      loop$html = _pipe$2;
      loop$children = rest;
      loop$raw_text = raw_text;
      loop$indent = indent;
    } else if (children.atLeastLength(1)) {
      let child = children.head;
      let rest = children.tail;
      let _pipe = child;
      let _pipe$1 = do_to_snapshot_builder(_pipe, raw_text, indent);
      let _pipe$2 = $string_tree.append(_pipe$1, "\n");
      let _pipe$3 = ((_capture) => {
        return $string_tree.append_tree(html, _capture);
      })(_pipe$2);
      loop$html = _pipe$3;
      loop$children = rest;
      loop$raw_text = raw_text;
      loop$indent = indent;
    } else {
      return html;
    }
  }
}

function do_to_snapshot_builder(node, raw_text, indent) {
  let spaces = $string.repeat("  ", indent);
  if (node instanceof Text && node.content === "") {
    return $string_tree.new$();
  } else if (node instanceof Text && (raw_text)) {
    let content = node.content;
    return $string_tree.from_strings(toList([spaces, content]));
  } else if (node instanceof Text) {
    let content = node.content;
    return $string_tree.from_strings(toList([spaces, $houdini.escape(content)]));
  } else if (node instanceof Fragment && node.children.hasLength(0)) {
    return $string_tree.new$();
  } else if (node instanceof Fragment) {
    let children = node.children;
    let _pipe = $string_tree.new$();
    return children_to_snapshot_builder(_pipe, children, raw_text, indent);
  } else if (node instanceof Element && (node.self_closing)) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let self_closing = node.self_closing;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.prepend(_pipe, spaces);
    let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
    return $string_tree.append(_pipe$2, "/>");
  } else if (node instanceof Element && (node.void)) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let void$ = node.void;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.prepend(_pipe, spaces);
    let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
    return $string_tree.append(_pipe$2, ">");
  } else if (node instanceof Element && node.children.hasLength(0)) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.prepend(_pipe, spaces);
    let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
    let _pipe$3 = $string_tree.append(_pipe$2, ">");
    return $string_tree.append(_pipe$3, ("</" + tag) + ">");
  } else if (node instanceof Element) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let children = node.children;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.prepend(_pipe, spaces);
    let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
    let _pipe$3 = $string_tree.append(_pipe$2, ">\n");
    let _pipe$4 = children_to_snapshot_builder(
      _pipe$3,
      children,
      raw_text,
      indent + 1,
    );
    let _pipe$5 = $string_tree.append(_pipe$4, spaces);
    return $string_tree.append(_pipe$5, ("</" + tag) + ">");
  } else {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let inner_html = node.inner_html;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    let _pipe$2 = $string_tree.append(_pipe$1, ">");
    let _pipe$3 = $string_tree.append(_pipe$2, inner_html);
    return $string_tree.append(_pipe$3, ("</" + tag) + ">");
  }
}

export function to_snapshot(node) {
  let _pipe = node;
  let _pipe$1 = do_to_snapshot_builder(_pipe, false, 0);
  return $string_tree.to_string(_pipe$1);
}

function children_to_string_tree(html, children) {
  return $list.fold(
    children,
    html,
    (html, child) => {
      let _pipe = child;
      let _pipe$1 = to_string_tree(_pipe);
      return ((_capture) => { return $string_tree.append_tree(html, _capture); })(
        _pipe$1,
      );
    },
  );
}

export function to_string_tree(node) {
  if (node instanceof Text && node.content === "") {
    return $string_tree.new$();
  } else if (node instanceof Text) {
    let content = node.content;
    return $string_tree.from_string($houdini.escape(content));
  } else if (node instanceof Fragment) {
    let children = node.children;
    return children_to_string_tree($string_tree.new$(), children);
  } else if (node instanceof Element && (node.self_closing)) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let self_closing = node.self_closing;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    return $string_tree.append(_pipe$1, "/>");
  } else if (node instanceof Element && (node.void)) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let void$ = node.void;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    return $string_tree.append(_pipe$1, ">");
  } else if (node instanceof Element) {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let children = node.children;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    let _pipe$2 = $string_tree.append(_pipe$1, ">");
    let _pipe$3 = children_to_string_tree(_pipe$2, children);
    return $string_tree.append(_pipe$3, ("</" + tag) + ">");
  } else {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let inner_html = node.inner_html;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    let _pipe$2 = $string_tree.append(_pipe$1, ">");
    let _pipe$3 = $string_tree.append(_pipe$2, inner_html);
    return $string_tree.append(_pipe$3, ("</" + tag) + ">");
  }
}

export function to_string(node) {
  let _pipe = node;
  let _pipe$1 = to_string_tree(_pipe);
  return $string_tree.to_string(_pipe$1);
}

function fragment_to_json(kind, key, children, children_count) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.list(
    _pipe$1,
    "children",
    children,
    to_json,
  );
  let _pipe$3 = $json_object_builder.int(
    _pipe$2,
    "children_count",
    children_count,
  );
  return $json_object_builder.build(_pipe$3);
}

export function to_json(node) {
  if (node instanceof Fragment) {
    let kind = node.kind;
    let key = node.key;
    let children = node.children;
    let children_count = node.children_count;
    return fragment_to_json(kind, key, children, children_count);
  } else if (node instanceof Element) {
    let kind = node.kind;
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let children = node.children;
    return element_to_json(kind, key, namespace, tag, attributes, children);
  } else if (node instanceof Text) {
    let kind = node.kind;
    let key = node.key;
    let content = node.content;
    return text_to_json(kind, key, content);
  } else {
    let kind = node.kind;
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let inner_html = node.inner_html;
    return unsafe_inner_html_to_json(
      kind,
      key,
      namespace,
      tag,
      attributes,
      inner_html,
    );
  }
}

function element_to_json(kind, key, namespace, tag, attributes, children) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "namespace", namespace);
  let _pipe$3 = $json_object_builder.string(_pipe$2, "tag", tag);
  let _pipe$4 = $json_object_builder.list(
    _pipe$3,
    "attributes",
    attributes,
    $vattr.to_json,
  );
  let _pipe$5 = $json_object_builder.list(
    _pipe$4,
    "children",
    children,
    to_json,
  );
  return $json_object_builder.build(_pipe$5);
}

function set_fragment_key(
  loop$key,
  loop$children,
  loop$index,
  loop$new_children,
  loop$keyed_children
) {
  while (true) {
    let key = loop$key;
    let children = loop$children;
    let index = loop$index;
    let new_children = loop$new_children;
    let keyed_children = loop$keyed_children;
    if (children.hasLength(0)) {
      return [$list.reverse(new_children), keyed_children];
    } else if (children.atLeastLength(1) &&
    children.head instanceof Fragment &&
    (children.head.key === "")) {
      let node = children.head;
      let children$1 = children.tail;
      let child_key = (key + "::") + $int.to_string(index);
      let $ = set_fragment_key(
        child_key,
        node.children,
        0,
        $constants.empty_list,
        $mutable_map.new$(),
      );
      let node_children = $[0];
      let node_keyed_children = $[1];
      let _block;
      let _record = node;
      _block = new Fragment(
        _record.kind,
        _record.key,
        _record.mapper,
        node_children,
        node_keyed_children,
        _record.children_count,
      );
      let new_node = _block;
      let new_children$1 = listPrepend(new_node, new_children);
      let index$1 = index + 1;
      loop$key = key;
      loop$children = children$1;
      loop$index = index$1;
      loop$new_children = new_children$1;
      loop$keyed_children = keyed_children;
    } else if (children.atLeastLength(1) && (children.head.key !== "")) {
      let node = children.head;
      let children$1 = children.tail;
      let child_key = (key + "::") + node.key;
      let keyed_node = to_keyed(child_key, node);
      let new_children$1 = listPrepend(keyed_node, new_children);
      let keyed_children$1 = $mutable_map.insert(
        keyed_children,
        child_key,
        keyed_node,
      );
      let index$1 = index + 1;
      loop$key = key;
      loop$children = children$1;
      loop$index = index$1;
      loop$new_children = new_children$1;
      loop$keyed_children = keyed_children$1;
    } else {
      let node = children.head;
      let children$1 = children.tail;
      let new_children$1 = listPrepend(node, new_children);
      let index$1 = index + 1;
      loop$key = key;
      loop$children = children$1;
      loop$index = index$1;
      loop$new_children = new_children$1;
      loop$keyed_children = keyed_children;
    }
  }
}

export function to_keyed(key, node) {
  if (node instanceof Element) {
    let _record = node;
    return new Element(
      _record.kind,
      key,
      _record.mapper,
      _record.namespace,
      _record.tag,
      _record.attributes,
      _record.children,
      _record.keyed_children,
      _record.self_closing,
      _record.void,
    );
  } else if (node instanceof Text) {
    let _record = node;
    return new Text(_record.kind, key, _record.mapper, _record.content);
  } else if (node instanceof UnsafeInnerHtml) {
    let _record = node;
    return new UnsafeInnerHtml(
      _record.kind,
      key,
      _record.mapper,
      _record.namespace,
      _record.tag,
      _record.attributes,
      _record.inner_html,
    );
  } else {
    let children = node.children;
    let $ = set_fragment_key(
      key,
      children,
      0,
      $constants.empty_list,
      $mutable_map.new$(),
    );
    let children$1 = $[0];
    let keyed_children = $[1];
    let _record = node;
    return new Fragment(
      _record.kind,
      key,
      _record.mapper,
      children$1,
      keyed_children,
      _record.children_count,
    );
  }
}
