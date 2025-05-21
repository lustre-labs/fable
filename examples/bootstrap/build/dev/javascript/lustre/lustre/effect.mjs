import * as $process from "../../gleam_erlang/gleam/erlang/process.mjs";
import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

class Effect extends $CustomType {
  constructor(synchronous, before_paint, after_paint) {
    super();
    this.synchronous = synchronous;
    this.before_paint = before_paint;
    this.after_paint = after_paint;
  }
}

class Actions extends $CustomType {
  constructor(dispatch, emit, select, root) {
    super();
    this.dispatch = dispatch;
    this.emit = emit;
    this.select = select;
    this.root = root;
  }
}

function do_comap_select(_, _1, _2) {
  return undefined;
}

function do_comap_actions(actions, f) {
  return new Actions(
    (msg) => { return actions.dispatch(f(msg)); },
    actions.emit,
    (selector) => { return do_comap_select(actions, selector, f); },
    actions.root,
  );
}

function do_map(effects, f) {
  return $list.map(
    effects,
    (effect) => {
      return (actions) => { return effect(do_comap_actions(actions, f)); };
    },
  );
}

export function map(effect, f) {
  return new Effect(
    do_map(effect.synchronous, f),
    do_map(effect.before_paint, f),
    do_map(effect.after_paint, f),
  );
}

export function perform(effect, dispatch, emit, select, root) {
  let actions = new Actions(dispatch, emit, select, root);
  return $list.each(effect.synchronous, (run) => { return run(actions); });
}

const empty = /* @__PURE__ */ new Effect(
  /* @__PURE__ */ toList([]),
  /* @__PURE__ */ toList([]),
  /* @__PURE__ */ toList([]),
);

export function none() {
  return empty;
}

export function from(effect) {
  let task = (actions) => {
    let dispatch = actions.dispatch;
    return effect(dispatch);
  };
  let _record = empty;
  return new Effect(toList([task]), _record.before_paint, _record.after_paint);
}

export function before_paint(effect) {
  let task = (actions) => {
    let root = actions.root();
    let dispatch = actions.dispatch;
    return effect(dispatch, root);
  };
  let _record = empty;
  return new Effect(_record.synchronous, toList([task]), _record.after_paint);
}

export function after_paint(effect) {
  let task = (actions) => {
    let root = actions.root();
    let dispatch = actions.dispatch;
    return effect(dispatch, root);
  };
  let _record = empty;
  return new Effect(_record.synchronous, _record.before_paint, toList([task]));
}

export function event(name, data) {
  let task = (actions) => { return actions.emit(name, data); };
  let _record = empty;
  return new Effect(toList([task]), _record.before_paint, _record.after_paint);
}

export function select(_) {
  return empty;
}

export function batch(effects) {
  return $list.fold(
    effects,
    empty,
    (acc, eff) => {
      return new Effect(
        $list.fold(eff.synchronous, acc.synchronous, $list.prepend),
        $list.fold(eff.before_paint, acc.before_paint, $list.prepend),
        $list.fold(eff.after_paint, acc.after_paint, $list.prepend),
      );
    },
  );
}
