import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../../gleam.mjs";
import * as $attribute from "../../lustre/attribute.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $path from "../../lustre/vdom/path.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";
import { Attribute } from "../../lustre/vdom/vattr.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../../lustre/vdom/vnode.mjs";

class FindElement extends $CustomType {
  constructor(matching) {
    super();
    this.matching = matching;
  }
}

class FindChild extends $CustomType {
  constructor(of, matching) {
    super();
    this.of = of;
    this.matching = matching;
  }
}

class FindDescendant extends $CustomType {
  constructor(of, matching) {
    super();
    this.of = of;
    this.matching = matching;
  }
}

class All extends $CustomType {
  constructor(of) {
    super();
    this.of = of;
  }
}

class Type extends $CustomType {
  constructor(namespace, tag) {
    super();
    this.namespace = namespace;
    this.tag = tag;
  }
}

class HasAttribute extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}

class HasClass extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

class HasStyle extends $CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}

class Contains extends $CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
}

export function element(selector) {
  return new FindElement(selector);
}

export function child(parent, selector) {
  return new FindChild(parent, selector);
}

export function descendant(parent, selector) {
  return new FindDescendant(parent, selector);
}

export function and(first, second) {
  if (first instanceof All && first.of.hasLength(0)) {
    return new All(toList([second]));
  } else if (first instanceof All) {
    let others = first.of;
    return new All(listPrepend(second, others));
  } else {
    return new All(toList([first, second]));
  }
}

export function tag(value) {
  return new Type("", value);
}

export function namespaced(namespace, tag) {
  return new Type(namespace, tag);
}

export function attribute(name, value) {
  return new HasAttribute(name, value);
}

export function class$(name) {
  return new HasClass(name);
}

export function style(name, value) {
  return new HasStyle(name, value);
}

export function id(name) {
  return new HasAttribute("id", name);
}

export function data(name, value) {
  return new HasAttribute("data-" + name, value);
}

export function test_id(value) {
  return data("test-id", value);
}

export function aria(name, value) {
  return new HasAttribute("aria-" + name, value);
}

export function text(content) {
  return new Contains(content);
}

function text_content(element, inline, content) {
  if (element instanceof Fragment) {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element &&
  (!inline || (element.namespace !== ""))) {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "a") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "abbr") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "acronym") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "b") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "bdo") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "big") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "br") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "button") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "cite") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "code") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "dfn") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "em") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "i") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "img") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "input") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "kbd") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "label") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "map") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "object") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "output") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "q") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "samp") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "script") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "select") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "small") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "span") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "strong") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "sub") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "sup") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "textarea") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "time") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "tt") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element && element.tag === "var") {
    return $list.fold(
      element.children,
      content,
      (content, child) => { return text_content(child, true, content); },
    );
  } else if (element instanceof Element) {
    let rule = "display:inline";
    let is_inline = $list.any(
      element.attributes,
      (attribute) => {
        if (attribute instanceof Attribute && attribute.name === "style") {
          let value = attribute.value;
          return $string.contains(value, rule);
        } else {
          return false;
        }
      },
    );
    if (is_inline) {
      return $list.fold(
        element.children,
        content,
        (content, child) => { return text_content(child, true, content); },
      );
    } else {
      return content;
    }
  } else if (element instanceof Text) {
    return content + element.content;
  } else {
    return content;
  }
}

function matches(element, selector) {
  if (selector instanceof All) {
    let selectors = selector.of;
    return $list.all(
      selectors,
      (_capture) => { return matches(element, _capture); },
    );
  } else if (element instanceof Element && selector instanceof Type) {
    let namespace = element.namespace;
    let tag$1 = element.tag;
    return (namespace === selector.namespace) && (tag$1 === selector.tag);
  } else if (element instanceof UnsafeInnerHtml && selector instanceof Type) {
    let namespace = element.namespace;
    let tag$1 = element.tag;
    return (namespace === selector.namespace) && (tag$1 === selector.tag);
  } else if (element instanceof Element &&
  selector instanceof HasAttribute &&
  selector.value === "") {
    let attributes = element.attributes;
    let name = selector.name;
    return $list.any(
      attributes,
      (attribute) => {
        if (attribute instanceof Attribute) {
          return attribute.name === name;
        } else {
          return false;
        }
      },
    );
  } else if (element instanceof UnsafeInnerHtml &&
  selector instanceof HasAttribute &&
  selector.value === "") {
    let attributes = element.attributes;
    let name = selector.name;
    return $list.any(
      attributes,
      (attribute) => {
        if (attribute instanceof Attribute) {
          return attribute.name === name;
        } else {
          return false;
        }
      },
    );
  } else if (element instanceof Element && selector instanceof HasAttribute) {
    let attributes = element.attributes;
    let name = selector.name;
    let value = selector.value;
    return $list.contains(attributes, $attribute.attribute(name, value));
  } else if (element instanceof UnsafeInnerHtml &&
  selector instanceof HasAttribute) {
    let attributes = element.attributes;
    let name = selector.name;
    let value = selector.value;
    return $list.contains(attributes, $attribute.attribute(name, value));
  } else if (element instanceof Element && selector instanceof HasClass) {
    let attributes = element.attributes;
    let name = selector.name;
    return $list.fold_until(
      $string.split(name, " "),
      true,
      (_, class$) => {
        let name$1 = $string.trim_end(class$);
        let matches$1 = $list.any(
          attributes,
          (attribute) => {
            if (attribute instanceof Attribute && attribute.name === "class") {
              let value = attribute.value;
              return (((value === name$1) || $string.starts_with(
                value,
                name$1 + " ",
              )) || $string.ends_with(value, " " + name$1)) || $string.contains(
                value,
                (" " + name$1) + " ",
              );
            } else {
              return false;
            }
          },
        );
        if (matches$1) {
          return new $list.Continue(true);
        } else {
          return new $list.Stop(false);
        }
      },
    );
  } else if (element instanceof UnsafeInnerHtml && selector instanceof HasClass) {
    let attributes = element.attributes;
    let name = selector.name;
    return $list.fold_until(
      $string.split(name, " "),
      true,
      (_, class$) => {
        let name$1 = $string.trim_end(class$);
        let matches$1 = $list.any(
          attributes,
          (attribute) => {
            if (attribute instanceof Attribute && attribute.name === "class") {
              let value = attribute.value;
              return (((value === name$1) || $string.starts_with(
                value,
                name$1 + " ",
              )) || $string.ends_with(value, " " + name$1)) || $string.contains(
                value,
                (" " + name$1) + " ",
              );
            } else {
              return false;
            }
          },
        );
        if (matches$1) {
          return new $list.Continue(true);
        } else {
          return new $list.Stop(false);
        }
      },
    );
  } else if (element instanceof Element && selector instanceof HasStyle) {
    let attributes = element.attributes;
    let name = selector.name;
    let value = selector.value;
    let rule = ((name + ":") + value) + ";";
    return $list.any(
      attributes,
      (attribute) => {
        if (attribute instanceof Attribute && attribute.name === "style") {
          let value$1 = attribute.value;
          return $string.contains(value$1, rule);
        } else {
          return false;
        }
      },
    );
  } else if (element instanceof UnsafeInnerHtml && selector instanceof HasStyle) {
    let attributes = element.attributes;
    let name = selector.name;
    let value = selector.value;
    let rule = ((name + ":") + value) + ";";
    return $list.any(
      attributes,
      (attribute) => {
        if (attribute instanceof Attribute && attribute.name === "style") {
          let value$1 = attribute.value;
          return $string.contains(value$1, rule);
        } else {
          return false;
        }
      },
    );
  } else if (element instanceof Element && selector instanceof Contains) {
    let content = selector.content;
    let _pipe = element;
    let _pipe$1 = text_content(_pipe, false, "");
    return $string.contains(_pipe$1, content);
  } else {
    return false;
  }
}

function find_matching_in_list(
  loop$elements,
  loop$selector,
  loop$path,
  loop$index
) {
  while (true) {
    let elements = loop$elements;
    let selector = loop$selector;
    let path = loop$path;
    let index = loop$index;
    if (elements.hasLength(0)) {
      return new Error(undefined);
    } else if (elements.atLeastLength(1) && elements.head instanceof Fragment) {
      let first = elements.head;
      let rest = elements.tail;
      loop$elements = $list.append(first.children, rest);
      loop$selector = selector;
      loop$path = path;
      loop$index = index + 1;
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = matches(first, selector);
      if ($) {
        return new Ok([first, $path.add(path, index, first.key)]);
      } else {
        loop$elements = rest;
        loop$selector = selector;
        loop$path = path;
        loop$index = index + 1;
      }
    }
  }
}

function find_direct_child(parent, selector, path) {
  if (parent instanceof Element) {
    let children = parent.children;
    return find_matching_in_list(children, selector, path, 0);
  } else if (parent instanceof Fragment) {
    let children = parent.children;
    return find_matching_in_list(children, selector, path, 1);
  } else if (parent instanceof UnsafeInnerHtml) {
    return new Error(undefined);
  } else {
    return new Error(undefined);
  }
}

function find_all_matching_in_list(loop$elements, loop$selector) {
  while (true) {
    let elements = loop$elements;
    let selector = loop$selector;
    if (elements.hasLength(0)) {
      return toList([]);
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = matches(first, selector);
      if ($) {
        return listPrepend(first, find_all_matching_in_list(rest, selector));
      } else {
        loop$elements = rest;
        loop$selector = selector;
      }
    }
  }
}

function find_all_direct_children(parent, selector) {
  if (parent instanceof Element) {
    let children = parent.children;
    return find_all_matching_in_list(children, selector);
  } else if (parent instanceof Fragment) {
    let children = parent.children;
    return find_all_matching_in_list(children, selector);
  } else if (parent instanceof UnsafeInnerHtml) {
    return toList([]);
  } else {
    return toList([]);
  }
}

function sort_selectors(selectors) {
  return $list.sort(
    $list.flat_map(
      selectors,
      (selector) => {
        if (selector instanceof All) {
          let selectors$1 = selector.of;
          return selectors$1;
        } else {
          return toList([selector]);
        }
      },
    ),
    (a, b) => {
      if (a instanceof All) {
        throw makeError(
          "panic",
          "lustre/dev/query",
          749,
          "",
          "`All` selectors should be flattened",
          {}
        )
      } else if (b instanceof All) {
        throw makeError(
          "panic",
          "lustre/dev/query",
          749,
          "",
          "`All` selectors should be flattened",
          {}
        )
      } else if (a instanceof Type && b instanceof Type) {
        let $ = $string.compare(a.namespace, b.namespace);
        if ($ instanceof $order.Eq) {
          return $string.compare(a.tag, b.tag);
        } else {
          let order = $;
          return order;
        }
      } else if (a instanceof Type) {
        return new $order.Lt();
      } else if (b instanceof Type) {
        return new $order.Gt();
      } else if (a instanceof HasAttribute &&
      a.name === "id" &&
      b instanceof HasAttribute &&
      b.name === "id") {
        return $string.compare(a.value, b.value);
      } else if (a instanceof HasAttribute && a.name === "id") {
        return new $order.Lt();
      } else if (b instanceof HasAttribute && b.name === "id") {
        return new $order.Gt();
      } else if (a instanceof HasAttribute && b instanceof HasAttribute) {
        let $ = $string.compare(a.name, b.name);
        if ($ instanceof $order.Eq) {
          return $string.compare(a.value, b.value);
        } else {
          let order = $;
          return order;
        }
      } else if (a instanceof HasAttribute) {
        return new $order.Lt();
      } else if (b instanceof HasAttribute) {
        return new $order.Gt();
      } else if (a instanceof HasStyle && b instanceof HasStyle) {
        return $string.compare(a.name, b.name);
      } else if (a instanceof HasStyle) {
        return new $order.Lt();
      } else if (b instanceof HasStyle) {
        return new $order.Gt();
      } else if (a instanceof HasClass && b instanceof HasClass) {
        return $string.compare(a.name, b.name);
      } else if (a instanceof HasClass) {
        return new $order.Lt();
      } else if (b instanceof HasClass) {
        return new $order.Gt();
      } else {
        return $string.compare(a.content, b.content);
      }
    },
  );
}

function selector_to_readable_string(selector) {
  if (selector instanceof All && selector.of.hasLength(0)) {
    return "";
  } else if (selector instanceof Type &&
  selector.namespace === "" &&
  selector.tag === "") {
    return "";
  } else if (selector instanceof HasAttribute && selector.name === "") {
    return "";
  } else if (selector instanceof HasClass && selector.name === "") {
    return "";
  } else if (selector instanceof HasStyle && selector.name === "") {
    return "";
  } else if (selector instanceof HasStyle && selector.value === "") {
    return "";
  } else if (selector instanceof Contains && selector.content === "") {
    return "";
  } else if (selector instanceof All) {
    let selectors = selector.of;
    let _pipe = selectors;
    let _pipe$1 = sort_selectors(_pipe);
    let _pipe$2 = $list.map(_pipe$1, selector_to_readable_string);
    return $string.concat(_pipe$2);
  } else if (selector instanceof Type && selector.namespace === "") {
    let tag$1 = selector.tag;
    return tag$1;
  } else if (selector instanceof Type) {
    let namespace = selector.namespace;
    let tag$1 = selector.tag;
    return (namespace + ":") + tag$1;
  } else if (selector instanceof HasAttribute && selector.name === "id") {
    let value = selector.value;
    return "#" + value;
  } else if (selector instanceof HasAttribute && selector.value === "") {
    let name = selector.name;
    return ("[" + name) + "]";
  } else if (selector instanceof HasAttribute) {
    let name = selector.name;
    let value = selector.value;
    return ((("[" + name) + "=\"") + value) + "\"]";
  } else if (selector instanceof HasClass) {
    let name = selector.name;
    return "." + name;
  } else if (selector instanceof HasStyle) {
    let name = selector.name;
    let value = selector.value;
    return ((("[style*=\"" + name) + ":") + value) + "\"]";
  } else {
    let content = selector.content;
    return (":contains(\"" + content) + "\")";
  }
}

export function to_readable_string(query) {
  if (query instanceof FindElement) {
    let selector = query.matching;
    return selector_to_readable_string(selector);
  } else if (query instanceof FindChild) {
    let parent = query.of;
    let selector = query.matching;
    return (to_readable_string(parent) + " > ") + selector_to_readable_string(
      selector,
    );
  } else {
    let parent = query.of;
    let selector = query.matching;
    return (to_readable_string(parent) + " ") + selector_to_readable_string(
      selector,
    );
  }
}

function find_all_in_list(elements, query) {
  if (elements.hasLength(0)) {
    return toList([]);
  } else {
    let first = elements.head;
    let rest = elements.tail;
    let first_matches = find_all(first, query);
    let rest_matches = find_all_in_list(rest, query);
    return $list.append(first_matches, rest_matches);
  }
}

export function find_all(root, query) {
  if (query instanceof FindElement) {
    let selector = query.matching;
    let $ = matches(root, selector);
    if ($) {
      return listPrepend(root, find_all_in_children(root, query));
    } else {
      return find_all_in_children(root, query);
    }
  } else if (query instanceof FindChild) {
    let parent = query.of;
    let selector = query.matching;
    let _pipe = root;
    let _pipe$1 = find_all(_pipe, parent);
    return $list.flat_map(
      _pipe$1,
      (_capture) => { return find_all_direct_children(_capture, selector); },
    );
  } else {
    let parent = query.of;
    let selector = query.matching;
    let _pipe = root;
    let _pipe$1 = find_all(_pipe, parent);
    return $list.flat_map(
      _pipe$1,
      (_capture) => { return find_all_descendants(_capture, selector); },
    );
  }
}

function find_all_in_children(element, query) {
  if (element instanceof Element) {
    let children = element.children;
    return find_all_in_list(children, query);
  } else if (element instanceof Fragment) {
    let children = element.children;
    return find_all_in_list(children, query);
  } else if (element instanceof UnsafeInnerHtml) {
    return toList([]);
  } else {
    return toList([]);
  }
}

function find_all_descendants_in_list(elements, selector) {
  if (elements.hasLength(0)) {
    return toList([]);
  } else {
    let first = elements.head;
    let rest = elements.tail;
    let first_matches = find_all_descendants(first, selector);
    let rest_matches = find_all_descendants_in_list(rest, selector);
    return $list.append(first_matches, rest_matches);
  }
}

function find_all_descendants(parent, selector) {
  let direct_matches = find_all_direct_children(parent, selector);
  let _block;
  if (parent instanceof Element) {
    let children = parent.children;
    _block = find_all_descendants_in_list(children, selector);
  } else if (parent instanceof Fragment) {
    let children = parent.children;
    _block = find_all_descendants_in_list(children, selector);
  } else if (parent instanceof UnsafeInnerHtml) {
    _block = toList([]);
  } else {
    _block = toList([]);
  }
  let descendant_matches = _block;
  return $list.append(direct_matches, descendant_matches);
}

function find_in_list(loop$elements, loop$query, loop$path, loop$index) {
  while (true) {
    let elements = loop$elements;
    let query = loop$query;
    let path = loop$path;
    let index = loop$index;
    if (elements.hasLength(0)) {
      return new Error(undefined);
    } else if (elements.atLeastLength(1) && elements.head instanceof Fragment) {
      let first = elements.head;
      let rest = elements.tail;
      loop$elements = $list.append(first.children, rest);
      loop$query = query;
      loop$path = path;
      loop$index = index + 1;
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = find_path(first, query, index, path);
      if ($.isOk()) {
        let element$1 = $[0];
        return new Ok(element$1);
      } else {
        loop$elements = rest;
        loop$query = query;
        loop$path = path;
        loop$index = index + 1;
      }
    }
  }
}

export function find_path(root, query, index, path) {
  if (query instanceof FindElement) {
    let selector = query.matching;
    let $ = matches(root, selector);
    if ($) {
      return new Ok(
        [
          root,
          (() => {
            let _pipe = path;
            return $path.add(_pipe, index, root.key);
          })(),
        ],
      );
    } else {
      return find_in_children(root, query, index, path);
    }
  } else if (query instanceof FindChild) {
    let parent = query.of;
    let selector = query.matching;
    let $ = find_path(root, parent, index, path);
    if ($.isOk()) {
      let element$1 = $[0][0];
      let path$1 = $[0][1];
      return find_direct_child(element$1, selector, path$1);
    } else {
      return new Error(undefined);
    }
  } else {
    let parent = query.of;
    let selector = query.matching;
    let $ = find_path(root, parent, index, path);
    if ($.isOk()) {
      let element$1 = $[0][0];
      let path$1 = $[0][1];
      return find_descendant(element$1, selector, path$1);
    } else {
      return new Error(undefined);
    }
  }
}

function find_in_children(element, query, index, path) {
  if (element instanceof Element) {
    let children = element.children;
    return find_in_list(
      children,
      query,
      (() => {
        let _pipe = path;
        return $path.add(_pipe, index, element.key);
      })(),
      0,
    );
  } else if (element instanceof Fragment) {
    let children = element.children;
    return find_in_list(children, query, path, index + 1);
  } else if (element instanceof UnsafeInnerHtml) {
    return new Error(undefined);
  } else {
    return new Error(undefined);
  }
}

export function find(root, query) {
  let $ = find_path(root, query, 0, $path.root);
  if ($.isOk()) {
    let element$1 = $[0][0];
    return new Ok(element$1);
  } else {
    return new Error(undefined);
  }
}

function find_descendant_in_list(
  loop$elements,
  loop$selector,
  loop$path,
  loop$index
) {
  while (true) {
    let elements = loop$elements;
    let selector = loop$selector;
    let path = loop$path;
    let index = loop$index;
    if (elements.hasLength(0)) {
      return new Error(undefined);
    } else {
      let first = elements.head;
      let rest = elements.tail;
      let $ = matches(first, selector);
      if ($) {
        return new Ok([first, $path.add(path, index, first.key)]);
      } else {
        let child$1 = $path.add(path, index, first.key);
        let $1 = find_descendant(first, selector, child$1);
        if ($1.isOk()) {
          let element$1 = $1[0];
          return new Ok(element$1);
        } else {
          loop$elements = rest;
          loop$selector = selector;
          loop$path = path;
          loop$index = index + 1;
        }
      }
    }
  }
}

function find_descendant(parent, selector, path) {
  let $ = find_direct_child(parent, selector, path);
  if ($.isOk()) {
    let element$1 = $[0];
    return new Ok(element$1);
  } else {
    if (parent instanceof Element) {
      let children = parent.children;
      return find_descendant_in_list(children, selector, path, 0);
    } else if (parent instanceof Fragment) {
      let children = parent.children;
      return find_descendant_in_list(children, selector, path, 1);
    } else if (parent instanceof UnsafeInnerHtml) {
      return new Error(undefined);
    } else {
      return new Error(undefined);
    }
  }
}
