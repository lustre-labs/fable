import * as $process from "../../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $json from "../../../../gleam_json/gleam/json.mjs";
import * as $actor from "../../../../gleam_otp/gleam/otp/actor.mjs";
import { Spec } from "../../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dict from "../../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $function from "../../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../../../gleam_stdlib/gleam/set.mjs";
import { Error, CustomType as $CustomType } from "../../../gleam.mjs";
import * as $effect from "../../../lustre/effect.mjs";
import * as $transport from "../../../lustre/runtime/transport.mjs";
import * as $diff from "../../../lustre/vdom/diff.mjs";
import { Diff, diff } from "../../../lustre/vdom/diff.mjs";
import * as $events from "../../../lustre/vdom/events.mjs";
import * as $vnode from "../../../lustre/vdom/vnode.mjs";
import {
  throw_server_component_error as handle_effect,
  throw_server_component_error as broadcast,
  throw_server_component_error as loop,
  throw_server_component_error as start,
} from "../client/runtime.ffi.mjs";

export { start };

export class State extends $CustomType {
  constructor(self, selector, base_selector, model, update, view, config, vdom, events, subscribers, callbacks) {
    super();
    this.self = self;
    this.selector = selector;
    this.base_selector = base_selector;
    this.model = model;
    this.update = update;
    this.view = view;
    this.config = config;
    this.vdom = vdom;
    this.events = events;
    this.subscribers = subscribers;
    this.callbacks = callbacks;
  }
}

export class Config extends $CustomType {
  constructor(open_shadow_root, adopt_styles, attributes, properties) {
    super();
    this.open_shadow_root = open_shadow_root;
    this.adopt_styles = adopt_styles;
    this.attributes = attributes;
    this.properties = properties;
  }
}

export class ClientDispatchedMessage extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class ClientRegisteredSubject extends $CustomType {
  constructor(client) {
    super();
    this.client = client;
  }
}

export class ClientDeregisteredSubject extends $CustomType {
  constructor(client) {
    super();
    this.client = client;
  }
}

export class ClientRegisteredCallback extends $CustomType {
  constructor(callback) {
    super();
    this.callback = callback;
  }
}

export class ClientDeregisteredCallback extends $CustomType {
  constructor(callback) {
    super();
    this.callback = callback;
  }
}

export class EffectAddedSelector extends $CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}

export class EffectDispatchedMessage extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class EffectEmitEvent extends $CustomType {
  constructor(name, data) {
    super();
    this.name = name;
    this.data = data;
  }
}

export class SelfDispatchedMessages extends $CustomType {
  constructor(messages, effect) {
    super();
    this.messages = messages;
    this.effect = effect;
  }
}

export class SystemRequestedShutdown extends $CustomType {}

function handle_attribute_change(attributes, name, value) {
  let $ = $dict.get(attributes, name);
  if (!$.isOk()) {
    return new Error(undefined);
  } else {
    let handler = $[0];
    return handler(value);
  }
}

function handle_property_change(properties, name, value) {
  let $ = $dict.get(properties, name);
  if (!$.isOk()) {
    return new Error(undefined);
  } else {
    let decoder = $[0];
    let _pipe = $decode.run(value, decoder);
    return $result.replace_error(_pipe, undefined);
  }
}

function handle_client_message(state, message) {
  if (message instanceof $transport.Batch) {
    let messages = message.messages;
    return $list.fold(messages, state, handle_client_message);
  } else if (message instanceof $transport.AttributeChanged) {
    let name = message.name;
    let value = message.value;
    let $ = handle_attribute_change(state.config.attributes, name, value);
    if (!$.isOk()) {
      return state;
    } else {
      let msg = $[0];
      let $1 = state.update(state.model, msg);
      let model = $1[0];
      let effect = $1[1];
      let vdom = state.view(model);
      handle_effect(state.self, effect);
      let _record = state;
      return new State(
        _record.self,
        _record.selector,
        _record.base_selector,
        model,
        _record.update,
        _record.view,
        _record.config,
        vdom,
        _record.events,
        _record.subscribers,
        _record.callbacks,
      );
    }
  } else if (message instanceof $transport.PropertyChanged) {
    let name = message.name;
    let value = message.value;
    let $ = handle_property_change(state.config.properties, name, value);
    if (!$.isOk()) {
      return state;
    } else {
      let msg = $[0];
      let $1 = state.update(state.model, msg);
      let model = $1[0];
      let effect = $1[1];
      let vdom = state.view(model);
      handle_effect(state.self, effect);
      let _record = state;
      return new State(
        _record.self,
        _record.selector,
        _record.base_selector,
        model,
        _record.update,
        _record.view,
        _record.config,
        vdom,
        _record.events,
        _record.subscribers,
        _record.callbacks,
      );
    }
  } else {
    let path = message.path;
    let name = message.name;
    let event = message.event;
    let $ = $events.handle(state.events, path, name, event);
    if (!$[1].isOk()) {
      let events = $[0];
      let _record = state;
      return new State(
        _record.self,
        _record.selector,
        _record.base_selector,
        _record.model,
        _record.update,
        _record.view,
        _record.config,
        _record.vdom,
        events,
        _record.subscribers,
        _record.callbacks,
      );
    } else {
      let events = $[0];
      let message$1 = $[1][0];
      let $1 = state.update(state.model, message$1);
      let model = $1[0];
      let effect = $1[1];
      let vdom = state.view(model);
      handle_effect(state.self, effect);
      let _record = state;
      return new State(
        _record.self,
        _record.selector,
        _record.base_selector,
        model,
        _record.update,
        _record.view,
        _record.config,
        vdom,
        events,
        _record.subscribers,
        _record.callbacks,
      );
    }
  }
}
