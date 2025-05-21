import * as $json from "../../../gleam_json/gleam/json.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $json_object_builder from "../../lustre/internals/json_object_builder.mjs";
import * as $events from "../../lustre/vdom/events.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";
import * as $vnode from "../../lustre/vdom/vnode.mjs";

export class Diff extends $CustomType {
  constructor(patch, events) {
    super();
    this.patch = patch;
    this.events = events;
  }
}

export class Patch extends $CustomType {
  constructor(index, removed, changes, children) {
    super();
    this.index = index;
    this.removed = removed;
    this.changes = changes;
    this.children = children;
  }
}

export class ReplaceText extends $CustomType {
  constructor(kind, content) {
    super();
    this.kind = kind;
    this.content = content;
  }
}

export class ReplaceInnerHtml extends $CustomType {
  constructor(kind, inner_html) {
    super();
    this.kind = kind;
    this.inner_html = inner_html;
  }
}

export class Update extends $CustomType {
  constructor(kind, added, removed) {
    super();
    this.kind = kind;
    this.added = added;
    this.removed = removed;
  }
}

export class Move extends $CustomType {
  constructor(kind, key, before, count) {
    super();
    this.kind = kind;
    this.key = key;
    this.before = before;
    this.count = count;
  }
}

export class RemoveKey extends $CustomType {
  constructor(kind, key, count) {
    super();
    this.kind = kind;
    this.key = key;
    this.count = count;
  }
}

export class Replace extends $CustomType {
  constructor(kind, from, count, with$) {
    super();
    this.kind = kind;
    this.from = from;
    this.count = count;
    this.with = with$;
  }
}

export class Insert extends $CustomType {
  constructor(kind, children, before) {
    super();
    this.kind = kind;
    this.children = children;
    this.before = before;
  }
}

export class Remove extends $CustomType {
  constructor(kind, from, count) {
    super();
    this.kind = kind;
    this.from = from;
    this.count = count;
  }
}

export function new$(index, removed, changes, children) {
  return new Patch(index, removed, changes, children);
}

export function is_empty(patch) {
  if (patch instanceof Patch &&
  patch.removed === 0 &&
  patch.changes.hasLength(0) &&
  patch.children.hasLength(0)) {
    return true;
  } else {
    return false;
  }
}

export function add_child(parent, child) {
  let $ = is_empty(child);
  if ($) {
    return parent;
  } else {
    let _record = parent;
    return new Patch(
      _record.index,
      _record.removed,
      _record.changes,
      listPrepend(child, parent.children),
    );
  }
}

function replace_text_to_json(kind, content) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "content", content);
  return $json_object_builder.build(_pipe$1);
}

function replace_inner_html_to_json(kind, inner_html) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "inner_html", inner_html);
  return $json_object_builder.build(_pipe$1);
}

function update_to_json(kind, added, removed) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.list(_pipe, "added", added, $vattr.to_json);
  let _pipe$2 = $json_object_builder.list(
    _pipe$1,
    "removed",
    removed,
    $vattr.to_json,
  );
  return $json_object_builder.build(_pipe$2);
}

function move_to_json(kind, key, before, count) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["key", $json.string(key)],
      ["before", $json.int(before)],
      ["count", $json.int(count)],
    ]),
  );
}

function remove_key_to_json(kind, key, count) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["key", $json.string(key)],
      ["count", $json.int(count)],
    ]),
  );
}

function replace_to_json(kind, from, count, with$) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["from", $json.int(from)],
      ["count", $json.int(count)],
      ["with", $vnode.to_json(with$)],
    ]),
  );
}

function insert_to_json(kind, children, before) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["children", $json.array(children, $vnode.to_json)],
      ["before", $json.int(before)],
    ]),
  );
}

function remove_to_json(kind, from, count) {
  return $json.object(
    toList([
      ["kind", $json.int(kind)],
      ["from", $json.int(from)],
      ["count", $json.int(count)],
    ]),
  );
}

function change_to_json(change) {
  if (change instanceof ReplaceText) {
    let kind = change.kind;
    let content = change.content;
    return replace_text_to_json(kind, content);
  } else if (change instanceof ReplaceInnerHtml) {
    let kind = change.kind;
    let inner_html = change.inner_html;
    return replace_inner_html_to_json(kind, inner_html);
  } else if (change instanceof Update) {
    let kind = change.kind;
    let added = change.added;
    let removed = change.removed;
    return update_to_json(kind, added, removed);
  } else if (change instanceof Move) {
    let kind = change.kind;
    let key = change.key;
    let before = change.before;
    let count = change.count;
    return move_to_json(kind, key, before, count);
  } else if (change instanceof RemoveKey) {
    let kind = change.kind;
    let key = change.key;
    let count = change.count;
    return remove_key_to_json(kind, key, count);
  } else if (change instanceof Replace) {
    let kind = change.kind;
    let from = change.from;
    let count = change.count;
    let with$ = change.with;
    return replace_to_json(kind, from, count, with$);
  } else if (change instanceof Insert) {
    let kind = change.kind;
    let children = change.children;
    let before = change.before;
    return insert_to_json(kind, children, before);
  } else {
    let kind = change.kind;
    let from = change.from;
    let count = change.count;
    return remove_to_json(kind, from, count);
  }
}

export function to_json(patch) {
  let _pipe = $json_object_builder.new$();
  let _pipe$1 = $json_object_builder.int(_pipe, "index", patch.index);
  let _pipe$2 = $json_object_builder.int(_pipe$1, "removed", patch.removed);
  let _pipe$3 = $json_object_builder.list(
    _pipe$2,
    "changes",
    patch.changes,
    change_to_json,
  );
  let _pipe$4 = $json_object_builder.list(
    _pipe$3,
    "children",
    patch.children,
    to_json,
  );
  return $json_object_builder.build(_pipe$4);
}

export const replace_text_kind = 0;

export function replace_text(content) {
  return new ReplaceText(replace_text_kind, content);
}

export const replace_inner_html_kind = 1;

export function replace_inner_html(inner_html) {
  return new ReplaceInnerHtml(replace_inner_html_kind, inner_html);
}

export const update_kind = 2;

export function update(added, removed) {
  return new Update(update_kind, added, removed);
}

export const move_kind = 3;

export function move(key, before, count) {
  return new Move(move_kind, key, before, count);
}

export const remove_key_kind = 4;

export function remove_key(key, count) {
  return new RemoveKey(remove_key_kind, key, count);
}

export const replace_kind = 5;

export function replace(from, count, with$) {
  return new Replace(replace_kind, from, count, with$);
}

export const insert_kind = 6;

export function insert(children, before) {
  return new Insert(insert_kind, children, before);
}

export const remove_kind = 7;

export function remove(from, count) {
  return new Remove(remove_kind, from, count);
}
