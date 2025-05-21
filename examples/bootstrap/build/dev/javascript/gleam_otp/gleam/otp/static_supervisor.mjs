import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";

export class OneForOne extends $CustomType {}

export class OneForAll extends $CustomType {}

export class RestForOne extends $CustomType {}

export class Never extends $CustomType {}

export class AnySignificant extends $CustomType {}

export class AllSignificant extends $CustomType {}

class Builder extends $CustomType {
  constructor(strategy, intensity, period, auto_shutdown, children) {
    super();
    this.strategy = strategy;
    this.intensity = intensity;
    this.period = period;
    this.auto_shutdown = auto_shutdown;
    this.children = children;
  }
}

export class Permanent extends $CustomType {}

export class Transient extends $CustomType {}

export class Temporary extends $CustomType {}

export class Worker extends $CustomType {
  constructor(shutdown_ms) {
    super();
    this.shutdown_ms = shutdown_ms;
  }
}

export class Supervisor extends $CustomType {}

class ChildBuilder extends $CustomType {
  constructor(id, starter, restart, significant, child_type) {
    super();
    this.id = id;
    this.starter = starter;
    this.restart = restart;
    this.significant = significant;
    this.child_type = child_type;
  }
}

export function new$(strategy) {
  return new Builder(strategy, 2, 5, new Never(), toList([]));
}

export function restart_tolerance(builder, intensity, period) {
  let _record = builder;
  return new Builder(
    _record.strategy,
    intensity,
    period,
    _record.auto_shutdown,
    _record.children,
  );
}

export function auto_shutdown(builder, value) {
  let _record = builder;
  return new Builder(
    _record.strategy,
    _record.intensity,
    _record.period,
    value,
    _record.children,
  );
}

export function add(builder, child) {
  let _record = builder;
  return new Builder(
    _record.strategy,
    _record.intensity,
    _record.period,
    _record.auto_shutdown,
    listPrepend(child, builder.children),
  );
}

export function worker_child(id, starter) {
  return new ChildBuilder(
    id,
    () => {
      let _pipe = starter();
      return $result.map_error(_pipe, $dynamic.from);
    },
    new Permanent(),
    false,
    new Worker(5000),
  );
}

export function supervisor_child(id, starter) {
  return new ChildBuilder(
    id,
    () => {
      let _pipe = starter();
      return $result.map_error(_pipe, $dynamic.from);
    },
    new Permanent(),
    false,
    new Supervisor(),
  );
}

export function significant(child, significant) {
  let _record = child;
  return new ChildBuilder(
    _record.id,
    _record.starter,
    _record.restart,
    significant,
    _record.child_type,
  );
}

export function timeout(child, ms) {
  let $ = child.child_type;
  if ($ instanceof Worker) {
    let _record = child;
    return new ChildBuilder(
      _record.id,
      _record.starter,
      _record.restart,
      _record.significant,
      new Worker(ms),
    );
  } else {
    return child;
  }
}

export function restart(child, restart) {
  let _record = child;
  return new ChildBuilder(
    _record.id,
    _record.starter,
    restart,
    _record.significant,
    _record.child_type,
  );
}

export function init(start_data) {
  return new Ok(start_data);
}
