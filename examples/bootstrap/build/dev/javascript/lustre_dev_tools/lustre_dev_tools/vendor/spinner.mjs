import * as $ansi from "../../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $deque from "../../../gleam_deque/gleam/deque.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $repeatedly from "../../../repeatedly/repeatedly.mjs";
import { toList, CustomType as $CustomType, makeError } from "../../gleam.mjs";

class Spinner extends $CustomType {
  constructor(repeater, frames) {
    super();
    this.repeater = repeater;
    this.frames = frames;
  }
}

class State extends $CustomType {
  constructor(text, frames, colour) {
    super();
    this.text = text;
    this.frames = frames;
    this.colour = colour;
  }
}

class Builder extends $CustomType {
  constructor(frames, text, colour) {
    super();
    this.frames = frames;
    this.text = text;
    this.colour = colour;
  }
}

export function with_frames(builder, frames) {
  let _record = builder;
  return new Builder(frames, _record.text, _record.colour);
}

export function with_colour(builder, colour) {
  let _record = builder;
  return new Builder(_record.frames, _record.text, colour);
}

export function set_text(spinner, text) {
  return $repeatedly.update_state(
    spinner.repeater,
    (state) => {
      let _record = state;
      return new State(text, _record.frames, _record.colour);
    },
  );
}

const clear_line_code = "\u{001b}[2K";

const go_to_start_code = "\r";

const show_cursor = "\u{001b}[?25h";

export function stop(spinner) {
  $repeatedly.stop(spinner.repeater);
  return $io.print((clear_line_code + go_to_start_code) + show_cursor);
}

const hide_cursor = "\u{001b}[?25l";

export function start(builder) {
  let frames = $deque.from_list(builder.frames);
  let init = new State(builder.text, frames, builder.colour);
  let repeater = ((_capture) => { return $repeatedly.call(80, init, _capture); })(
    (_use0, _) => {
      let text = _use0.text;
      let colour = _use0.colour;
      let $ = $deque.pop_front(frames);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "lustre_dev_tools/vendor/spinner",
          238,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let frame = $[0][0];
      let frames$1 = $[0][1];
      let frames$2 = $deque.push_back(frames$1, frame);
      $io.print(
        ((((hide_cursor + clear_line_code) + go_to_start_code) + colour(frame)) + " ") + text,
      );
      return new State(text, frames$2, colour);
    },
  );
  return new Spinner(repeater, builder.frames);
}

export const snake_frames = /* @__PURE__ */ toList([
  "⠋",
  "⠙",
  "⠹",
  "⠸",
  "⠼",
  "⠴",
  "⠦",
  "⠧",
  "⠇",
  "⠏",
]);

export function new$(text) {
  return new Builder(snake_frames, text, $ansi.magenta);
}
