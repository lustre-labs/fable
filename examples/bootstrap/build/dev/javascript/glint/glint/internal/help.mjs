import * as $ansi from "../../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $colour from "../../../gleam_community_colour/gleam_community/colour.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType, isEqual } from "../../gleam.mjs";
import * as $utils from "../../glint/internal/utils.mjs";

export class MinArgs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class EqArgs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Config extends $CustomType {
  constructor(name, usage_colour, flags_colour, subcommands_colour, as_module, description, indent_width, max_output_width, min_first_column_width, column_gap, flag_prefix, flag_delimiter) {
    super();
    this.name = name;
    this.usage_colour = usage_colour;
    this.flags_colour = flags_colour;
    this.subcommands_colour = subcommands_colour;
    this.as_module = as_module;
    this.description = description;
    this.indent_width = indent_width;
    this.max_output_width = max_output_width;
    this.min_first_column_width = min_first_column_width;
    this.column_gap = column_gap;
    this.flag_prefix = flag_prefix;
    this.flag_delimiter = flag_delimiter;
  }
}

export class Metadata extends $CustomType {
  constructor(name, description) {
    super();
    this.name = name;
    this.description = description;
  }
}

export class Flag extends $CustomType {
  constructor(meta, type_) {
    super();
    this.meta = meta;
    this.type_ = type_;
  }
}

export class Command extends $CustomType {
  constructor(meta, flags, subcommands, unnamed_args, named_args) {
    super();
    this.meta = meta;
    this.flags = flags;
    this.subcommands = subcommands;
    this.unnamed_args = unnamed_args;
    this.named_args = named_args;
  }
}

function heading_style(heading, colour) {
  let _pipe = heading;
  let _pipe$1 = $ansi.bold(_pipe);
  let _pipe$2 = $ansi.underline(_pipe$1);
  let _pipe$3 = $ansi.italic(_pipe$2);
  return $ansi.hex(_pipe$3, $colour.to_rgb_hex(colour));
}

function args_count_to_usage_string(count) {
  if (count instanceof EqArgs && count[0] === 0) {
    return "";
  } else if (count instanceof EqArgs && count[0] === 1) {
    return "[ 1 argument ]";
  } else if (count instanceof EqArgs) {
    let n = count[0];
    return ("[ " + $int.to_string(n)) + " arguments ]";
  } else {
    let n = count[0];
    return ("[ " + $int.to_string(n)) + " or more arguments ]";
  }
}

function flag_help_to_string(help, config) {
  return (config.flag_prefix + help.meta.name) + (() => {
    let $ = help.type_;
    if ($ === "") {
      return "";
    } else {
      return ((config.flag_delimiter + "<") + help.type_) + ">";
    }
  })();
}

function flags_help_to_usage_strings(help, config) {
  let _pipe = help;
  let _pipe$1 = $list.map(
    _pipe,
    (_capture) => { return flag_help_to_string(_capture, config); },
  );
  return $list.sort(_pipe$1, $string.compare);
}

function flags_help_to_usage_string(config, help) {
  return $bool.guard(
    isEqual(help, toList([])),
    "",
    () => {
      let _block;
      let _pipe = help;
      let _pipe$1 = flags_help_to_usage_strings(_pipe, config);
      _block = $string.join(_pipe$1, " ");
      let content = _block;
      return ("[ " + content) + " ]";
    },
  );
}

function format_content(left, right, left_length, config) {
  let left_formatted = $string.pad_end(left, left_length, " ");
  let _block;
  let _pipe = config.max_output_width;
  let _pipe$1 = $int.subtract(_pipe, left_length + config.indent_width);
  let _pipe$2 = $int.max(_pipe$1, config.min_first_column_width);
  _block = ((_capture) => { return $utils.wordwrap(right, _capture); })(_pipe$2);
  let lines = _block;
  let right_formatted = $string.join(
    lines,
    "\n" + $string.repeat(" ", config.indent_width + left_length),
  );
  let _block$1;
  if (lines.hasLength(0)) {
    _block$1 = false;
  } else if (lines.hasLength(1)) {
    _block$1 = false;
  } else {
    _block$1 = true;
  }
  let wrapped = _block$1;
  return [
    (("\n" + $string.repeat(" ", config.indent_width)) + left_formatted) + right_formatted,
    wrapped,
  ];
}

function to_spaced_indented_string(data, f, left_length, config) {
  let left_length$1 = left_length + config.column_gap;
  let $ = $list.fold(
    data,
    [toList([]), false],
    (acc, data) => {
      let $1 = f(data);
      let left = $1[0];
      let right = $1[1];
      let $2 = format_content(left, right, left_length$1, config);
      let line = $2[0];
      let wrapped = $2[1];
      return [listPrepend(line, acc[0]), wrapped || acc[1]];
    },
  );
  let content = $[0];
  let wrapped = $[1];
  let _block;
  if (wrapped) {
    _block = "\n";
  } else {
    _block = "";
  }
  let joiner = _block;
  let _pipe = content;
  let _pipe$1 = $list.sort(_pipe, $string.compare);
  return $string.join(_pipe$1, joiner);
}

export const help_flag = /* @__PURE__ */ new Flag(
  /* @__PURE__ */ new Metadata("help", "Print help information"),
  "",
);

const flags_heading = "FLAGS:";

function flags_help_to_string(help, config) {
  return $bool.guard(
    isEqual(help, toList([])),
    "",
    () => {
      let _block;
      let _pipe = help;
      let _pipe$1 = $list.map(
        _pipe,
        (_capture) => { return flag_help_to_string(_capture, config); },
      );
      let _pipe$2 = $utils.max_string_length(_pipe$1);
      _block = $int.max(_pipe$2, config.min_first_column_width);
      let longest_flag_length = _block;
      let _block$1;
      let $ = config.flags_colour;
      if ($ instanceof None) {
        _block$1 = flags_heading;
      } else {
        let pretty = $[0];
        _block$1 = heading_style(flags_heading, pretty);
      }
      let heading = _block$1;
      let content = to_spaced_indented_string(
        listPrepend(help_flag, help),
        (help) => {
          return [flag_help_to_string(help, config), help.meta.description];
        },
        longest_flag_length,
        config,
      );
      return heading + content;
    },
  );
}

const subcommands_heading = "SUBCOMMANDS:";

function subcommands_help_to_string(help, config) {
  return $bool.guard(
    isEqual(help, toList([])),
    "",
    () => {
      let _block;
      let _pipe = help;
      let _pipe$1 = $list.map(_pipe, (h) => { return h.name; });
      let _pipe$2 = $utils.max_string_length(_pipe$1);
      _block = $int.max(_pipe$2, config.min_first_column_width);
      let longest_subcommand_length = _block;
      let _block$1;
      let $ = config.subcommands_colour;
      if ($ instanceof None) {
        _block$1 = subcommands_heading;
      } else {
        let pretty = $[0];
        _block$1 = heading_style(subcommands_heading, pretty);
      }
      let heading = _block$1;
      let content = to_spaced_indented_string(
        help,
        (help) => { return [help.name, help.description]; },
        longest_subcommand_length,
        config,
      );
      return heading + content;
    },
  );
}

const usage_heading = "USAGE:";

function command_help_to_usage_string(help, config) {
  let _block;
  let $ = config.name;
  if ($ instanceof Some && (config.as_module)) {
    let name = $[0];
    _block = "gleam run -m " + name;
  } else if ($ instanceof Some) {
    let name = $[0];
    _block = name;
  } else {
    _block = "gleam run";
  }
  let app_name = _block;
  let flags = flags_help_to_usage_string(config, help.flags);
  let _block$1;
  let $1 = (() => {
    let _pipe = $list.map(help.subcommands, (sc) => { return sc.name; });
    let _pipe$1 = $list.sort(_pipe, $string.compare);
    return $string.join(_pipe$1, " | ");
  })();
  if ($1 === "") {
    _block$1 = "";
  } else {
    let subcommands = $1;
    _block$1 = ("( " + subcommands) + " )";
  }
  let subcommands = _block$1;
  let _block$2;
  let _pipe = help.named_args;
  let _pipe$1 = $list.map(_pipe, (s) => { return ("<" + s) + ">"; });
  _block$2 = $string.join(_pipe$1, " ");
  let named_args = _block$2;
  let _block$3;
  let _pipe$2 = $option.map(help.unnamed_args, args_count_to_usage_string);
  _block$3 = $option.unwrap(_pipe$2, "[ ARGS ]");
  let unnamed_args = _block$3;
  let max_usage_width = config.max_output_width - config.indent_width;
  let _block$4;
  let _pipe$3 = toList([
    app_name,
    help.meta.name,
    subcommands,
    named_args,
    unnamed_args,
    flags,
  ]);
  let _pipe$4 = $list.filter(_pipe$3, (s) => { return s !== ""; });
  let _pipe$5 = $string.join(_pipe$4, " ");
  let _pipe$6 = $utils.wordwrap(_pipe$5, max_usage_width);
  _block$4 = $string.join(
    _pipe$6,
    "\n" + $string.repeat(" ", config.indent_width * 2),
  );
  let content = _block$4;
  return (((() => {
    let $2 = config.usage_colour;
    if ($2 instanceof None) {
      return usage_heading;
    } else {
      let pretty = $2[0];
      return heading_style(usage_heading, pretty);
    }
  })() + "\n") + $string.repeat(" ", config.indent_width)) + content;
}

export function command_help_to_string(help, config) {
  let _block;
  let $ = help.meta.name;
  if ($ === "") {
    _block = "";
  } else {
    let s = $;
    _block = "Command: " + s;
  }
  let command = _block;
  let _block$1;
  let _pipe = help.meta.description;
  let _pipe$1 = $utils.wordwrap(_pipe, config.max_output_width);
  _block$1 = $string.join(_pipe$1, "\n");
  let command_description = _block$1;
  let _pipe$2 = toList([
    (() => {
      let _pipe$2 = config.description;
      return $option.unwrap(_pipe$2, "");
    })(),
    command,
    command_description,
    command_help_to_usage_string(help, config),
    flags_help_to_string(help.flags, config),
    subcommands_help_to_string(help.subcommands, config),
  ]);
  let _pipe$3 = $list.filter(_pipe$2, (s) => { return s !== ""; });
  return $string.join(_pipe$3, "\n\n");
}
