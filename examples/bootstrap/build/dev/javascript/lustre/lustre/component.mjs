import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import { attribute } from "../lustre/attribute.mjs";
import * as $effect from "../lustre/effect.mjs";
import * as $element from "../lustre/element.mjs";
import * as $html from "../lustre/element/html.mjs";
import * as $constants from "../lustre/internals/constants.mjs";
import * as $runtime from "../lustre/runtime/server/runtime.mjs";
import {
  set_form_value as do_set_form_value,
  clear_form_value as do_clear_form_value,
  set_pseudo_state as do_set_pseudo_state,
  remove_pseudo_state as do_remove_pseudo_state,
} from "./runtime/client/component.ffi.mjs";

class Config extends $CustomType {
  constructor(open_shadow_root, adopt_styles, attributes, properties, is_form_associated, on_form_autofill, on_form_reset, on_form_restore) {
    super();
    this.open_shadow_root = open_shadow_root;
    this.adopt_styles = adopt_styles;
    this.attributes = attributes;
    this.properties = properties;
    this.is_form_associated = is_form_associated;
    this.on_form_autofill = on_form_autofill;
    this.on_form_reset = on_form_reset;
    this.on_form_restore = on_form_restore;
  }
}

class Option extends $CustomType {
  constructor(apply) {
    super();
    this.apply = apply;
  }
}

export function new$(options) {
  let init = new Config(
    false,
    true,
    $constants.empty_dict(),
    $constants.empty_dict(),
    false,
    $constants.option_none,
    $constants.option_none,
    $constants.option_none,
  );
  return $list.fold(
    options,
    init,
    (config, option) => { return option.apply(config); },
  );
}

export function on_attribute_change(name, decoder) {
  return new Option(
    (config) => {
      let attributes = $dict.insert(config.attributes, name, decoder);
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        _record.adopt_styles,
        attributes,
        _record.properties,
        _record.is_form_associated,
        _record.on_form_autofill,
        _record.on_form_reset,
        _record.on_form_restore,
      );
    },
  );
}

export function on_property_change(name, decoder) {
  return new Option(
    (config) => {
      let properties = $dict.insert(config.properties, name, decoder);
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        _record.adopt_styles,
        _record.attributes,
        properties,
        _record.is_form_associated,
        _record.on_form_autofill,
        _record.on_form_reset,
        _record.on_form_restore,
      );
    },
  );
}

export function form_associated() {
  return new Option(
    (config) => {
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        _record.adopt_styles,
        _record.attributes,
        _record.properties,
        true,
        _record.on_form_autofill,
        _record.on_form_reset,
        _record.on_form_restore,
      );
    },
  );
}

export function on_form_autofill(handler) {
  return new Option(
    (config) => {
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        _record.adopt_styles,
        _record.attributes,
        _record.properties,
        true,
        new Some(handler),
        _record.on_form_reset,
        _record.on_form_restore,
      );
    },
  );
}

export function on_form_reset(msg) {
  return new Option(
    (config) => {
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        _record.adopt_styles,
        _record.attributes,
        _record.properties,
        true,
        _record.on_form_autofill,
        new Some(msg),
        _record.on_form_restore,
      );
    },
  );
}

export function on_form_restore(handler) {
  return new Option(
    (config) => {
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        _record.adopt_styles,
        _record.attributes,
        _record.properties,
        true,
        _record.on_form_autofill,
        _record.on_form_reset,
        new Some(handler),
      );
    },
  );
}

export function open_shadow_root(open) {
  return new Option(
    (config) => {
      let _record = config;
      return new Config(
        open,
        _record.adopt_styles,
        _record.attributes,
        _record.properties,
        _record.is_form_associated,
        _record.on_form_autofill,
        _record.on_form_reset,
        _record.on_form_restore,
      );
    },
  );
}

export function adopt_styles(adopt) {
  return new Option(
    (config) => {
      let _record = config;
      return new Config(
        _record.open_shadow_root,
        adopt,
        _record.attributes,
        _record.properties,
        _record.is_form_associated,
        _record.on_form_autofill,
        _record.on_form_reset,
        _record.on_form_restore,
      );
    },
  );
}

export function to_server_component_config(config) {
  return new $runtime.Config(
    config.open_shadow_root,
    config.adopt_styles,
    config.attributes,
    config.properties,
  );
}

export function default_slot(attributes, fallback) {
  return $html.slot(attributes, fallback);
}

export function named_slot(name, attributes, fallback) {
  return $html.slot(listPrepend(attribute("name", name), attributes), fallback);
}

export function part(name) {
  return attribute("part", name);
}

export function exportparts(names) {
  return attribute("exportparts", $string.join(names, ", "));
}

export function slot(name) {
  return attribute("slot", name);
}

export function set_form_value(value) {
  return $effect.before_paint(
    (_, root) => { return do_set_form_value(root, value); },
  );
}

export function clear_form_value() {
  return $effect.before_paint(
    (_, root) => { return do_clear_form_value(root); },
  );
}

export function set_pseudo_state(value) {
  return $effect.before_paint(
    (_, root) => { return do_set_pseudo_state(root, value); },
  );
}

export function remove_pseudo_state(value) {
  return $effect.before_paint(
    (_, root) => { return do_remove_pseudo_state(root, value); },
  );
}
