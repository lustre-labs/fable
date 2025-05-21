import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $pair from "../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, toList } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import * as $effect from "../lustre/effect.mjs";
import * as $constants from "../lustre/internals/constants.mjs";
import * as $vattr from "../lustre/vdom/vattr.mjs";
import { Event } from "../lustre/vdom/vattr.mjs";

export function emit(event, data) {
  return $effect.event(event, data);
}

function is_immediate_event(name) {
  if (name === "input") {
    return true;
  } else if (name === "change") {
    return true;
  } else if (name === "focus") {
    return true;
  } else if (name === "focusin") {
    return true;
  } else if (name === "focusout") {
    return true;
  } else if (name === "blur") {
    return true;
  } else if (name === "select") {
    return true;
  } else {
    return false;
  }
}

export function on(name, handler) {
  return $vattr.event(
    name,
    handler,
    $constants.empty_list,
    false,
    false,
    is_immediate_event(name),
    0,
    0,
  );
}

export function prevent_default(event) {
  if (event instanceof Event) {
    let _record = event;
    return new Event(
      _record.kind,
      _record.name,
      _record.handler,
      _record.include,
      true,
      _record.stop_propagation,
      _record.immediate,
      _record.debounce,
      _record.throttle,
    );
  } else {
    return event;
  }
}

export function stop_propagation(event) {
  if (event instanceof Event) {
    let _record = event;
    return new Event(
      _record.kind,
      _record.name,
      _record.handler,
      _record.include,
      _record.prevent_default,
      true,
      _record.immediate,
      _record.debounce,
      _record.throttle,
    );
  } else {
    return event;
  }
}

export function debounce(event, delay) {
  if (event instanceof Event) {
    let _record = event;
    return new Event(
      _record.kind,
      _record.name,
      _record.handler,
      _record.include,
      _record.prevent_default,
      _record.stop_propagation,
      _record.immediate,
      $int.max(0, delay),
      _record.throttle,
    );
  } else {
    return event;
  }
}

export function throttle(event, delay) {
  if (event instanceof Event) {
    let _record = event;
    return new Event(
      _record.kind,
      _record.name,
      _record.handler,
      _record.include,
      _record.prevent_default,
      _record.stop_propagation,
      _record.immediate,
      _record.debounce,
      $int.max(0, delay),
    );
  } else {
    return event;
  }
}

export function on_click(msg) {
  return on("click", $decode.success(msg));
}

export function on_mouse_down(msg) {
  return on("mousedown", $decode.success(msg));
}

export function on_mouse_up(msg) {
  return on("mouseup", $decode.success(msg));
}

export function on_mouse_enter(msg) {
  return on("mouseenter", $decode.success(msg));
}

export function on_mouse_leave(msg) {
  return on("mouseleave", $decode.success(msg));
}

export function on_mouse_over(msg) {
  return on("mouseover", $decode.success(msg));
}

export function on_mouse_out(msg) {
  return on("mouseout", $decode.success(msg));
}

export function on_keypress(msg) {
  return on(
    "keypress",
    $decode.field(
      "key",
      $decode.string,
      (key) => {
        let _pipe = key;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
}

export function on_keydown(msg) {
  return on(
    "keydown",
    $decode.field(
      "key",
      $decode.string,
      (key) => {
        let _pipe = key;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
}

export function on_keyup(msg) {
  return on(
    "keyup",
    $decode.field(
      "key",
      $decode.string,
      (key) => {
        let _pipe = key;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
}

export function on_input(msg) {
  return on(
    "input",
    $decode.subfield(
      toList(["target", "value"]),
      $decode.string,
      (value) => { return $decode.success(msg(value)); },
    ),
  );
}

export function on_change(msg) {
  return on(
    "change",
    $decode.subfield(
      toList(["target", "value"]),
      $decode.string,
      (value) => { return $decode.success(msg(value)); },
    ),
  );
}

export function on_check(msg) {
  return on(
    "change",
    $decode.subfield(
      toList(["target", "checked"]),
      $decode.bool,
      (checked) => { return $decode.success(msg(checked)); },
    ),
  );
}

function formdata_decoder() {
  let string_value_decoder = $decode.field(
    0,
    $decode.string,
    (key) => {
      return $decode.field(
        1,
        $decode.one_of(
          $decode.map($decode.string, (var0) => { return new Ok(var0); }),
          toList([$decode.success(new Error(undefined))]),
        ),
        (value) => {
          let _pipe = value;
          let _pipe$1 = $result.map(
            _pipe,
            (_capture) => { return $pair.new$(key, _capture); },
          );
          return $decode.success(_pipe$1);
        },
      );
    },
  );
  let _pipe = string_value_decoder;
  let _pipe$1 = $decode.list(_pipe);
  return $decode.map(_pipe$1, $result.values);
}

export function on_submit(msg) {
  let _pipe = on(
    "submit",
    $decode.subfield(
      toList(["detail", "formData"]),
      formdata_decoder(),
      (formdata) => {
        let _pipe = formdata;
        let _pipe$1 = msg(_pipe);
        return $decode.success(_pipe$1);
      },
    ),
  );
  return prevent_default(_pipe);
}

export function on_focus(msg) {
  return on("focus", $decode.success(msg));
}

export function on_blur(msg) {
  return on("blur", $decode.success(msg));
}
