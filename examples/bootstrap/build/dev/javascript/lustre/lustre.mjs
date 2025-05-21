import * as $actor from "../gleam_otp/gleam/otp/actor.mjs";
import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import { identity as coerce } from "../gleam_stdlib/gleam/function.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { Error, CustomType as $CustomType } from "./gleam.mjs";
import * as $component from "./lustre/component.mjs";
import * as $effect from "./lustre/effect.mjs";
import * as $element from "./lustre/element.mjs";
import * as $constants from "./lustre/internals/constants.mjs";
import { make_component as register } from "./lustre/runtime/client/component.ffi.mjs";
import { send, is_browser, is_registered } from "./lustre/runtime/client/runtime.ffi.mjs";
import { start as do_start } from "./lustre/runtime/client/spa.ffi.mjs";
import { start as start_server_component } from "./lustre/runtime/server/runtime.ffi.mjs";
import * as $runtime from "./lustre/runtime/server/runtime.mjs";

export { is_browser, is_registered, register, send, start_server_component };

class App extends $CustomType {
  constructor(init, update, view, config) {
    super();
    this.init = init;
    this.update = update;
    this.view = view;
    this.config = config;
  }
}

export class ActorError extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class BadComponentName extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ComponentAlreadyRegistered extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ElementNotFound extends $CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}

export class NotABrowser extends $CustomType {}

export function component(init, update, view, options) {
  return new App(init, update, view, $component.new$(options));
}

export function application(init, update, view) {
  return new App(init, update, view, $component.new$($constants.empty_list));
}

export function element(view) {
  return application(
    (_) => { return [undefined, $effect.none()]; },
    (_, _1) => { return [undefined, $effect.none()]; },
    (_) => { return view; },
  );
}

export function simple(init, update, view) {
  let init$1 = (start_args) => { return [init(start_args), $effect.none()]; };
  let update$1 = (model, msg) => { return [update(model, msg), $effect.none()]; };
  return application(init$1, update$1, view);
}

export function dispatch(msg) {
  return new $runtime.EffectDispatchedMessage(msg);
}

export function shutdown() {
  return new $runtime.SystemRequestedShutdown();
}

export function start(app, selector, start_args) {
  return $bool.guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => { return do_start(app, selector, start_args); },
  );
}
