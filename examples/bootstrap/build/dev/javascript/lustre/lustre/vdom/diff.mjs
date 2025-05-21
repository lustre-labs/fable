import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../../gleam_stdlib/gleam/order.mjs";
import * as $set from "../../../gleam_stdlib/gleam/set.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType, isEqual } from "../../gleam.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $events from "../../lustre/vdom/events.mjs";
import * as $patch from "../../lustre/vdom/patch.mjs";
import { Patch } from "../../lustre/vdom/patch.mjs";
import * as $path from "../../lustre/vdom/path.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";
import { Attribute, Event, Property } from "../../lustre/vdom/vattr.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";
import { Element, Fragment, Text, UnsafeInnerHtml } from "../../lustre/vdom/vnode.mjs";

export class Diff extends $CustomType {
  constructor(patch, events) {
    super();
    this.patch = patch;
    this.events = events;
  }
}

class AttributeChange extends $CustomType {
  constructor(added, removed, events) {
    super();
    this.added = added;
    this.removed = removed;
    this.events = events;
  }
}

function is_controlled(events, namespace, tag, path) {
  if (tag === "input" && (namespace === "")) {
    return $events.has_dispatched_events(events, path);
  } else if (tag === "select" && (namespace === "")) {
    return $events.has_dispatched_events(events, path);
  } else if (tag === "textarea" && (namespace === "")) {
    return $events.has_dispatched_events(events, path);
  } else {
    return false;
  }
}

function diff_attributes(
  loop$controlled,
  loop$path,
  loop$mapper,
  loop$events,
  loop$old,
  loop$new,
  loop$added,
  loop$removed
) {
  while (true) {
    let controlled = loop$controlled;
    let path = loop$path;
    let mapper = loop$mapper;
    let events = loop$events;
    let old = loop$old;
    let new$ = loop$new;
    let added = loop$added;
    let removed = loop$removed;
    if (old.hasLength(0) && new$.hasLength(0)) {
      return new AttributeChange(added, removed, events);
    } else if (old.atLeastLength(1) &&
    old.head instanceof Event &&
    new$.hasLength(0)) {
      let prev = old.head;
      let name = old.head.name;
      let old$1 = old.tail;
      let removed$1 = listPrepend(prev, removed);
      let events$1 = $events.remove_event(events, path, name);
      loop$controlled = controlled;
      loop$path = path;
      loop$mapper = mapper;
      loop$events = events$1;
      loop$old = old$1;
      loop$new = new$;
      loop$added = added;
      loop$removed = removed$1;
    } else if (old.atLeastLength(1) && new$.hasLength(0)) {
      let prev = old.head;
      let old$1 = old.tail;
      let removed$1 = listPrepend(prev, removed);
      loop$controlled = controlled;
      loop$path = path;
      loop$mapper = mapper;
      loop$events = events;
      loop$old = old$1;
      loop$new = new$;
      loop$added = added;
      loop$removed = removed$1;
    } else if (old.hasLength(0) &&
    new$.atLeastLength(1) &&
    new$.head instanceof Event) {
      let next = new$.head;
      let name = new$.head.name;
      let handler = new$.head.handler;
      let new$1 = new$.tail;
      let added$1 = listPrepend(next, added);
      let events$1 = $events.add_event(events, mapper, path, name, handler);
      loop$controlled = controlled;
      loop$path = path;
      loop$mapper = mapper;
      loop$events = events$1;
      loop$old = old;
      loop$new = new$1;
      loop$added = added$1;
      loop$removed = removed;
    } else if (old.hasLength(0) && new$.atLeastLength(1)) {
      let next = new$.head;
      let new$1 = new$.tail;
      let added$1 = listPrepend(next, added);
      loop$controlled = controlled;
      loop$path = path;
      loop$mapper = mapper;
      loop$events = events;
      loop$old = old;
      loop$new = new$1;
      loop$added = added$1;
      loop$removed = removed;
    } else {
      let prev = old.head;
      let remaining_old = old.tail;
      let next = new$.head;
      let remaining_new = new$.tail;
      let $ = $vattr.compare(prev, next);
      if (prev instanceof Attribute &&
      $ instanceof Eq &&
      next instanceof Attribute) {
        let _block;
        let $1 = next.name;
        if ($1 === "value") {
          _block = controlled || (prev.value !== next.value);
        } else if ($1 === "checked") {
          _block = controlled || (prev.value !== next.value);
        } else if ($1 === "selected") {
          _block = controlled || (prev.value !== next.value);
        } else {
          _block = prev.value !== next.value;
        }
        let has_changes = _block;
        let _block$1;
        if (has_changes) {
          _block$1 = listPrepend(next, added);
        } else {
          _block$1 = added;
        }
        let added$1 = _block$1;
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else if (prev instanceof Property &&
      $ instanceof Eq &&
      next instanceof Property) {
        let _block;
        let $1 = next.name;
        if ($1 === "scrollLeft") {
          _block = true;
        } else if ($1 === "scrollRight") {
          _block = true;
        } else if ($1 === "value") {
          _block = controlled || (!isEqual(prev.value, next.value));
        } else if ($1 === "checked") {
          _block = controlled || (!isEqual(prev.value, next.value));
        } else if ($1 === "selected") {
          _block = controlled || (!isEqual(prev.value, next.value));
        } else {
          _block = !isEqual(prev.value, next.value);
        }
        let has_changes = _block;
        let _block$1;
        if (has_changes) {
          _block$1 = listPrepend(next, added);
        } else {
          _block$1 = added;
        }
        let added$1 = _block$1;
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else if (prev instanceof Event &&
      $ instanceof Eq &&
      next instanceof Event) {
        let name = next.name;
        let handler = next.handler;
        let has_changes = ((((prev.prevent_default !== next.prevent_default) || (prev.stop_propagation !== next.stop_propagation)) || (prev.immediate !== next.immediate)) || (prev.debounce !== next.debounce)) || (prev.throttle !== next.throttle);
        let _block;
        if (has_changes) {
          _block = listPrepend(next, added);
        } else {
          _block = added;
        }
        let added$1 = _block;
        let events$1 = $events.add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else if (prev instanceof Event && $ instanceof Eq) {
        let name = prev.name;
        let added$1 = listPrepend(next, added);
        let removed$1 = listPrepend(prev, removed);
        let events$1 = $events.remove_event(events, path, name);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed$1;
      } else if ($ instanceof Eq && next instanceof Event) {
        let name = next.name;
        let handler = next.handler;
        let added$1 = listPrepend(next, added);
        let removed$1 = listPrepend(prev, removed);
        let events$1 = $events.add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed$1;
      } else if ($ instanceof Eq) {
        let added$1 = listPrepend(next, added);
        let removed$1 = listPrepend(prev, removed);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed$1;
      } else if ($ instanceof Gt && next instanceof Event) {
        let name = next.name;
        let handler = next.handler;
        let added$1 = listPrepend(next, added);
        let events$1 = $events.add_event(events, mapper, path, name, handler);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else if ($ instanceof Gt) {
        let added$1 = listPrepend(next, added);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = added$1;
        loop$removed = removed;
      } else if (prev instanceof Event && $ instanceof Lt) {
        let name = prev.name;
        let removed$1 = listPrepend(prev, removed);
        let events$1 = $events.remove_event(events, path, name);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events$1;
        loop$old = remaining_old;
        loop$new = new$;
        loop$added = added;
        loop$removed = removed$1;
      } else {
        let removed$1 = listPrepend(prev, removed);
        loop$controlled = controlled;
        loop$path = path;
        loop$mapper = mapper;
        loop$events = events;
        loop$old = remaining_old;
        loop$new = new$;
        loop$added = added;
        loop$removed = removed$1;
      }
    }
  }
}

function do_diff(
  loop$old,
  loop$old_keyed,
  loop$new,
  loop$new_keyed,
  loop$moved,
  loop$moved_offset,
  loop$removed,
  loop$node_index,
  loop$patch_index,
  loop$path,
  loop$changes,
  loop$children,
  loop$mapper,
  loop$events
) {
  while (true) {
    let old = loop$old;
    let old_keyed = loop$old_keyed;
    let new$ = loop$new;
    let new_keyed = loop$new_keyed;
    let moved = loop$moved;
    let moved_offset = loop$moved_offset;
    let removed = loop$removed;
    let node_index = loop$node_index;
    let patch_index = loop$patch_index;
    let path = loop$path;
    let changes = loop$changes;
    let children = loop$children;
    let mapper = loop$mapper;
    let events = loop$events;
    if (old.hasLength(0) && new$.hasLength(0)) {
      return new Diff(
        new Patch(patch_index, removed, changes, children),
        events,
      );
    } else if (old.atLeastLength(1) && new$.hasLength(0)) {
      let prev = old.head;
      let old$1 = old.tail;
      let _block;
      let $ = (prev.key === "") || !$set.contains(moved, prev.key);
      if ($) {
        _block = removed + $vnode.advance(prev);
      } else {
        _block = removed;
      }
      let removed$1 = _block;
      let events$1 = $events.remove_child(events, path, node_index, prev);
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed$1;
      loop$node_index = node_index;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = changes;
      loop$children = children;
      loop$mapper = mapper;
      loop$events = events$1;
    } else if (old.hasLength(0) && new$.atLeastLength(1)) {
      let events$1 = $events.add_children(
        events,
        mapper,
        path,
        node_index,
        new$,
      );
      let insert = $patch.insert(new$, node_index - moved_offset);
      let changes$1 = listPrepend(insert, changes);
      return new Diff(
        new Patch(patch_index, removed, changes$1, children),
        events$1,
      );
    } else if (old.atLeastLength(1) &&
    new$.atLeastLength(1) &&
    (old.head.key !== new$.head.key)) {
      let prev = old.head;
      let old_remaining = old.tail;
      let next = new$.head;
      let new_remaining = new$.tail;
      let next_did_exist = $mutable_map.get(old_keyed, next.key);
      let prev_does_exist = $mutable_map.get(new_keyed, prev.key);
      let prev_has_moved = $set.contains(moved, prev.key);
      if (prev_does_exist.isOk() && next_did_exist.isOk() && (prev_has_moved)) {
        loop$old = old_remaining;
        loop$old_keyed = old_keyed;
        loop$new = new$;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset - $vnode.advance(prev);
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = changes;
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events;
      } else if (prev_does_exist.isOk() && next_did_exist.isOk()) {
        let match = next_did_exist[0];
        let count = $vnode.advance(next);
        let before = node_index - moved_offset;
        let move = $patch.move(next.key, before, count);
        let changes$1 = listPrepend(move, changes);
        let moved$1 = $set.insert(moved, next.key);
        let moved_offset$1 = moved_offset + count;
        loop$old = listPrepend(match, old);
        loop$old_keyed = old_keyed;
        loop$new = new$;
        loop$new_keyed = new_keyed;
        loop$moved = moved$1;
        loop$moved_offset = moved_offset$1;
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = changes$1;
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events;
      } else if (!prev_does_exist.isOk() && next_did_exist.isOk()) {
        let count = $vnode.advance(prev);
        let moved_offset$1 = moved_offset - count;
        let events$1 = $events.remove_child(events, path, node_index, prev);
        let remove = $patch.remove_key(prev.key, count);
        let changes$1 = listPrepend(remove, changes);
        loop$old = old_remaining;
        loop$old_keyed = old_keyed;
        loop$new = new$;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset$1;
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = changes$1;
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events$1;
      } else if (prev_does_exist.isOk() && !next_did_exist.isOk()) {
        let before = node_index - moved_offset;
        let count = $vnode.advance(next);
        let events$1 = $events.add_child(events, mapper, path, node_index, next);
        let insert = $patch.insert(toList([next]), before);
        let changes$1 = listPrepend(insert, changes);
        loop$old = old;
        loop$old_keyed = old_keyed;
        loop$new = new_remaining;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset + count;
        loop$removed = removed;
        loop$node_index = node_index + count;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = changes$1;
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events$1;
      } else {
        let prev_count = $vnode.advance(prev);
        let next_count = $vnode.advance(next);
        let change = $patch.replace(node_index - moved_offset, prev_count, next);
        let _block;
        let _pipe = events;
        let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev);
        _block = $events.add_child(_pipe$1, mapper, path, node_index, next);
        let events$1 = _block;
        loop$old = old_remaining;
        loop$old_keyed = old_keyed;
        loop$new = new_remaining;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = (moved_offset - prev_count) + next_count;
        loop$removed = removed;
        loop$node_index = node_index + next_count;
        loop$patch_index = patch_index;
        loop$path = path;
        loop$changes = listPrepend(change, changes);
        loop$children = children;
        loop$mapper = mapper;
        loop$events = events$1;
      }
    } else if (old.atLeastLength(1) &&
    old.head instanceof Fragment &&
    new$.atLeastLength(1) &&
    new$.head instanceof Fragment) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$.head;
      let new$1 = new$.tail;
      let node_index$1 = node_index + 1;
      let prev_count = prev.children_count;
      let next_count = next.children_count;
      let composed_mapper = $events.compose_mapper(mapper, next.mapper);
      let child = do_diff(
        prev.children,
        prev.keyed_children,
        next.children,
        next.keyed_children,
        $constants.empty_set(),
        moved_offset,
        0,
        node_index$1,
        -1,
        path,
        $constants.empty_list,
        children,
        composed_mapper,
        events,
      );
      let _block;
      let $ = child.patch.removed > 0;
      if ($) {
        let remove_from = (node_index$1 + next_count) - moved_offset;
        let patch = $patch.remove(remove_from, child.patch.removed);
        _block = $list.append(child.patch.changes, listPrepend(patch, changes));
      } else {
        _block = $list.append(child.patch.changes, changes);
      }
      let changes$1 = _block;
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = (moved_offset + next_count) - prev_count;
      loop$removed = removed;
      loop$node_index = node_index$1 + next_count;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = changes$1;
      loop$children = child.patch.children;
      loop$mapper = mapper;
      loop$events = child.events;
    } else if (old.atLeastLength(1) &&
    old.head instanceof Element &&
    new$.atLeastLength(1) &&
    new$.head instanceof Element &&
    ((old.head.namespace === new$.head.namespace) && (old.head.tag === new$.head.tag))) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$.head;
      let new$1 = new$.tail;
      let composed_mapper = $events.compose_mapper(mapper, next.mapper);
      let child_path = $path.add(path, node_index, next.key);
      let controlled = is_controlled(
        events,
        next.namespace,
        next.tag,
        child_path,
      );
      let $ = diff_attributes(
        controlled,
        child_path,
        composed_mapper,
        events,
        prev.attributes,
        next.attributes,
        $constants.empty_list,
        $constants.empty_list,
      );
      let added_attrs = $.added;
      let removed_attrs = $.removed;
      let events$1 = $.events;
      let _block;
      if (added_attrs.hasLength(0) && removed_attrs.hasLength(0)) {
        _block = $constants.empty_list;
      } else {
        _block = toList([$patch.update(added_attrs, removed_attrs)]);
      }
      let initial_child_changes = _block;
      let child = do_diff(
        prev.children,
        prev.keyed_children,
        next.children,
        next.keyed_children,
        $constants.empty_set(),
        0,
        0,
        0,
        node_index,
        child_path,
        initial_child_changes,
        $constants.empty_list,
        composed_mapper,
        events$1,
      );
      let _block$1;
      let $1 = child.patch;
      if ($1 instanceof Patch &&
      $1.removed === 0 &&
      $1.changes.hasLength(0) &&
      $1.children.hasLength(0)) {
        _block$1 = children;
      } else {
        _block$1 = listPrepend(child.patch, children);
      }
      let children$1 = _block$1;
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = changes;
      loop$children = children$1;
      loop$mapper = mapper;
      loop$events = child.events;
    } else if (old.atLeastLength(1) &&
    old.head instanceof Text &&
    new$.atLeastLength(1) &&
    new$.head instanceof Text &&
    (old.head.content === new$.head.content)) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$.head;
      let new$1 = new$.tail;
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = changes;
      loop$children = children;
      loop$mapper = mapper;
      loop$events = events;
    } else if (old.atLeastLength(1) &&
    old.head instanceof Text &&
    new$.atLeastLength(1) &&
    new$.head instanceof Text) {
      let old$1 = old.tail;
      let next = new$.head;
      let new$1 = new$.tail;
      let child = $patch.new$(
        node_index,
        0,
        toList([$patch.replace_text(next.content)]),
        $constants.empty_list,
      );
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = changes;
      loop$children = listPrepend(child, children);
      loop$mapper = mapper;
      loop$events = events;
    } else if (old.atLeastLength(1) &&
    old.head instanceof UnsafeInnerHtml &&
    new$.atLeastLength(1) &&
    new$.head instanceof UnsafeInnerHtml) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$.head;
      let new$1 = new$.tail;
      let composed_mapper = $events.compose_mapper(mapper, next.mapper);
      let child_path = $path.add(path, node_index, next.key);
      let $ = diff_attributes(
        false,
        child_path,
        composed_mapper,
        events,
        prev.attributes,
        next.attributes,
        $constants.empty_list,
        $constants.empty_list,
      );
      let added_attrs = $.added;
      let removed_attrs = $.removed;
      let events$1 = $.events;
      let _block;
      if (added_attrs.hasLength(0) && removed_attrs.hasLength(0)) {
        _block = $constants.empty_list;
      } else {
        _block = toList([$patch.update(added_attrs, removed_attrs)]);
      }
      let child_changes = _block;
      let _block$1;
      let $1 = prev.inner_html === next.inner_html;
      if ($1) {
        _block$1 = child_changes;
      } else {
        _block$1 = listPrepend(
          $patch.replace_inner_html(next.inner_html),
          child_changes,
        );
      }
      let child_changes$1 = _block$1;
      let _block$2;
      if (child_changes$1.hasLength(0)) {
        _block$2 = children;
      } else {
        _block$2 = listPrepend(
          $patch.new$(node_index, 0, child_changes$1, toList([])),
          children,
        );
      }
      let children$1 = _block$2;
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = changes;
      loop$children = children$1;
      loop$mapper = mapper;
      loop$events = events$1;
    } else {
      let prev = old.head;
      let old_remaining = old.tail;
      let next = new$.head;
      let new_remaining = new$.tail;
      let prev_count = $vnode.advance(prev);
      let next_count = $vnode.advance(next);
      let change = $patch.replace(node_index - moved_offset, prev_count, next);
      let _block;
      let _pipe = events;
      let _pipe$1 = $events.remove_child(_pipe, path, node_index, prev);
      _block = $events.add_child(_pipe$1, mapper, path, node_index, next);
      let events$1 = _block;
      loop$old = old_remaining;
      loop$old_keyed = old_keyed;
      loop$new = new_remaining;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = (moved_offset - prev_count) + next_count;
      loop$removed = removed;
      loop$node_index = node_index + next_count;
      loop$patch_index = patch_index;
      loop$path = path;
      loop$changes = listPrepend(change, changes);
      loop$children = children;
      loop$mapper = mapper;
      loop$events = events$1;
    }
  }
}

export function diff(events, old, new$) {
  return do_diff(
    toList([old]),
    $mutable_map.new$(),
    toList([new$]),
    $mutable_map.new$(),
    $constants.empty_set(),
    0,
    0,
    0,
    0,
    $path.root,
    $constants.empty_list,
    $constants.empty_list,
    $function.identity,
    $events.tick(events),
  );
}
