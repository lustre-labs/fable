import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { prepend as listPrepend } from "../../gleam.mjs";
import * as $attribute from "../../lustre/attribute.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";

function extract_keyed_children(children) {
  let init = [$mutable_map.new$(), $constants.empty_list, 0];
  let $ = $list.fold(
    children,
    init,
    (_use0, _use1) => {
      let keyed_children = _use0[0];
      let children$1 = _use0[1];
      let children_count = _use0[2];
      let key = _use1[0];
      let element$1 = _use1[1];
      let keyed_element = $vnode.to_keyed(key, element$1);
      let _block;
      if (key === "") {
        _block = keyed_children;
      } else {
        _block = $mutable_map.insert(keyed_children, key, keyed_element);
      }
      let keyed_children$1 = _block;
      return [
        keyed_children$1,
        listPrepend(keyed_element, children$1),
        children_count + 1,
      ];
    },
  );
  let keyed_children = $[0];
  let children$1 = $[1];
  let children_count = $[2];
  return [keyed_children, $list.reverse(children$1), children_count];
}

export function element(tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return $vnode.element(
    "",
    $function.identity,
    "",
    tag,
    attributes,
    children$1,
    keyed_children,
    false,
    false,
  );
}

export function namespaced(namespace, tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return $vnode.element(
    "",
    $function.identity,
    namespace,
    tag,
    attributes,
    children$1,
    keyed_children,
    false,
    false,
  );
}

export function fragment(children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  let children_count = $[2];
  return $vnode.fragment(
    "",
    $function.identity,
    children$1,
    keyed_children,
    children_count,
  );
}

export function ul(attributes, children) {
  return element("ul", attributes, children);
}

export function ol(attributes, children) {
  return element("ol", attributes, children);
}

export function div(attributes, children) {
  return element("div", attributes, children);
}

export function tbody(attributes, children) {
  return element("tbody", attributes, children);
}

export function dl(attributes, children) {
  return element("dl", attributes, children);
}
