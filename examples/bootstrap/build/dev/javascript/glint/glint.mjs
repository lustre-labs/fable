import * as $colour from "../gleam_community_colour/gleam_community/colour.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $snag from "../snag/snag.mjs";
import * as $gleam from "./gleam.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "./gleam.mjs";
import * as $constraint from "./glint/constraint.mjs";
import * as $help from "./glint/internal/help.mjs";
import { exit } from "node:process";

class Config extends $CustomType {
  constructor(pretty_help, name, as_module, description, exit, indent_width, max_output_width, min_first_column_width, column_gap) {
    super();
    this.pretty_help = pretty_help;
    this.name = name;
    this.as_module = as_module;
    this.description = description;
    this.exit = exit;
    this.indent_width = indent_width;
    this.max_output_width = max_output_width;
    this.min_first_column_width = min_first_column_width;
    this.column_gap = column_gap;
  }
}

export class PrettyHelp extends $CustomType {
  constructor(usage, flags, subcommands) {
    super();
    this.usage = usage;
    this.flags = flags;
    this.subcommands = subcommands;
  }
}

class Glint extends $CustomType {
  constructor(config, cmd) {
    super();
    this.config = config;
    this.cmd = cmd;
  }
}

export class EqArgs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MinArgs extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Command extends $CustomType {
  constructor(do$, flags, description, unnamed_args, named_args) {
    super();
    this.do = do$;
    this.flags = flags;
    this.description = description;
    this.unnamed_args = unnamed_args;
    this.named_args = named_args;
  }
}

class InternalCommand extends $CustomType {
  constructor(do$, flags, unnamed_args, named_args) {
    super();
    this.do = do$;
    this.flags = flags;
    this.unnamed_args = unnamed_args;
    this.named_args = named_args;
  }
}

class NamedArgs extends $CustomType {
  constructor(internal) {
    super();
    this.internal = internal;
  }
}

class CommandNode extends $CustomType {
  constructor(contents, subcommands, group_flags, description) {
    super();
    this.contents = contents;
    this.subcommands = subcommands;
    this.group_flags = group_flags;
    this.description = description;
  }
}

export class Out extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Help extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class B extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class I extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class LI extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class F extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class LF extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class S extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class LS extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Flag extends $CustomType {
  constructor(name, desc, parser, value, getter, default$) {
    super();
    this.name = name;
    this.desc = desc;
    this.parser = parser;
    this.value = value;
    this.getter = getter;
    this.default = default$;
  }
}

class FlagInternals extends $CustomType {
  constructor(value, parser) {
    super();
    this.value = value;
    this.parser = parser;
  }
}

class FlagEntry extends $CustomType {
  constructor(value, description) {
    super();
    this.value = value;
    this.description = description;
  }
}

class Flags extends $CustomType {
  constructor(internal) {
    super();
    this.internal = internal;
  }
}

export function pretty_help(glint, pretty) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        new Some(pretty),
        _record$1.name,
        _record$1.as_module,
        _record$1.description,
        _record$1.exit,
        _record$1.indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function with_name(glint, name) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        new Some(name),
        _record$1.as_module,
        _record$1.description,
        _record$1.exit,
        _record$1.indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function without_exit(glint) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        _record$1.as_module,
        _record$1.description,
        false,
        _record$1.indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function as_module(glint) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        true,
        _record$1.description,
        _record$1.exit,
        _record$1.indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function with_indent_width(glint, indent_width) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        _record$1.as_module,
        _record$1.description,
        _record$1.exit,
        indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function with_max_output_width(glint, max_output_width) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        _record$1.as_module,
        _record$1.description,
        _record$1.exit,
        _record$1.indent_width,
        max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function with_min_first_column_width(glint, min_first_column_width) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        _record$1.as_module,
        _record$1.description,
        _record$1.exit,
        _record$1.indent_width,
        _record$1.max_output_width,
        min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function with_column_gap(glint, column_gap) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        _record$1.as_module,
        _record$1.description,
        _record$1.exit,
        _record$1.indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        column_gap,
      );
    })(),
    _record.cmd,
  );
}

export function global_help(glint, description) {
  let _record = glint;
  return new Glint(
    (() => {
      let _record$1 = glint.config;
      return new Config(
        _record$1.pretty_help,
        _record$1.name,
        _record$1.as_module,
        new Some(description),
        _record$1.exit,
        _record$1.indent_width,
        _record$1.max_output_width,
        _record$1.min_first_column_width,
        _record$1.column_gap,
      );
    })(),
    _record.cmd,
  );
}

function sanitize_path(path) {
  let _pipe = path;
  let _pipe$1 = $list.map(_pipe, $string.trim);
  return $list.filter(_pipe$1, (s) => { return s !== ""; });
}

export function map_command(command, fun) {
  return new Command(
    (named_args, args, flags) => {
      return fun(command.do(named_args, args, flags));
    },
    command.flags,
    command.description,
    command.unnamed_args,
    command.named_args,
  );
}

export function command_help(desc, f) {
  let _record = f();
  return new Command(
    _record.do,
    _record.flags,
    desc,
    _record.unnamed_args,
    _record.named_args,
  );
}

export function unnamed_args(count, f) {
  let _record = f();
  return new Command(
    _record.do,
    _record.flags,
    _record.description,
    new Some(count),
    _record.named_args,
  );
}

export function named_arg(name, f) {
  let cmd = f(
    (named_args) => {
      let $ = $dict.get(named_args.internal, name);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "glint",
          394,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let arg = $[0];
      return arg;
    },
  );
  let _record = cmd;
  return new Command(
    _record.do,
    _record.flags,
    _record.description,
    _record.unnamed_args,
    listPrepend(name, cmd.named_args),
  );
}

function args_compare(expected, actual) {
  return $result.map_error(
    (() => {
      if (expected instanceof EqArgs && (actual === expected[0])) {
        let expected$1 = expected[0];
        return new Ok(undefined);
      } else if (expected instanceof MinArgs && (actual >= expected[0])) {
        let expected$1 = expected[0];
        return new Ok(undefined);
      } else if (expected instanceof EqArgs) {
        let expected$1 = expected[0];
        return new Error($int.to_string(expected$1));
      } else {
        let expected$1 = expected[0];
        return new Error("at least " + $int.to_string(expected$1));
      }
    })(),
    (err) => {
      return $snag.new$(
        (("expected: " + err) + " argument(s), provided: ") + $int.to_string(
          actual,
        ),
      );
    },
  );
}

export function default_pretty_help() {
  let $ = $colour.from_rgb255(182, 255, 234);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "glint",
      643,
      "default_pretty_help",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let usage_colour = $[0];
  let $1 = $colour.from_rgb255(255, 175, 243);
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "glint",
      644,
      "default_pretty_help",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let flags_colour = $1[0];
  let $2 = $colour.from_rgb255(252, 226, 174);
  if (!$2.isOk()) {
    throw makeError(
      "let_assert",
      "glint",
      645,
      "default_pretty_help",
      "Pattern match failed, no pattern matched the value.",
      { value: $2 }
    )
  }
  let subcommands_colour = $2[0];
  return new PrettyHelp(usage_colour, flags_colour, subcommands_colour);
}

function flag_type_info(flag) {
  let $ = flag.value;
  if ($ instanceof I) {
    return "INT";
  } else if ($ instanceof B) {
    return "BOOL";
  } else if ($ instanceof F) {
    return "FLOAT";
  } else if ($ instanceof LF) {
    return "FLOAT_LIST";
  } else if ($ instanceof LI) {
    return "INT_LIST";
  } else if ($ instanceof LS) {
    return "STRING_LIST";
  } else {
    return "STRING";
  }
}

function build_subcommands_help(subcommands) {
  return $dict.fold(
    subcommands,
    toList([]),
    (acc, name, node) => {
      return listPrepend(new $help.Metadata(name, node.description), acc);
    },
  );
}

function new_builder(name, valuer, getter, p) {
  return new Flag(name, "", p, valuer, getter, new None());
}

function build_flag(fb) {
  return new FlagEntry(
    fb.value(new FlagInternals(fb.default, fb.parser)),
    fb.desc,
  );
}

function attempt(val, f) {
  return $result.try$(val, (a) => { return $result.replace(f(a), a); });
}

function wrap_with_constraint(p, constraint) {
  return (input) => { return attempt(p(input), constraint); };
}

export function flag_constraint(builder, constraint) {
  let _record = builder;
  return new Flag(
    _record.name,
    _record.desc,
    wrap_with_constraint(builder.parser, constraint),
    _record.value,
    _record.getter,
    _record.default,
  );
}

export function flag_help(flag, description) {
  let _record = flag;
  return new Flag(
    _record.name,
    description,
    _record.parser,
    _record.value,
    _record.getter,
    _record.default,
  );
}

export function flag_default(flag, default$) {
  let _record = flag;
  return new Flag(
    _record.name,
    _record.desc,
    _record.parser,
    _record.value,
    _record.getter,
    new Some(default$),
  );
}

function insert(flags, name, flag) {
  return new Flags($dict.insert(flags.internal, name, flag));
}

export function flag(flag, f) {
  let cmd = f((_capture) => { return flag.getter(_capture, flag.name); });
  let _record = cmd;
  return new Command(
    _record.do,
    insert(cmd.flags, flag.name, build_flag(flag)),
    _record.description,
    _record.unnamed_args,
    _record.named_args,
  );
}

function merge(a, b) {
  return new Flags($dict.merge(a.internal, b.internal));
}

function fold(flags, acc, f) {
  return $dict.fold(flags.internal, acc, f);
}

function build_flags_help(flags) {
  return fold(
    flags,
    toList([]),
    (acc, name, flag) => {
      return listPrepend(
        new $help.Flag(
          new $help.Metadata(name, flag.description),
          flag_type_info(flag),
        ),
        acc,
      );
    },
  );
}

function build_command_help(name, node) {
  let _block;
  let _pipe = node.contents;
  let _pipe$1 = $option.map(
    _pipe,
    (cmd) => {
      return [
        node.description,
        build_flags_help(merge(node.group_flags, cmd.flags)),
        cmd.unnamed_args,
        cmd.named_args,
      ];
    },
  );
  _block = $option.unwrap(
    _pipe$1,
    [node.description, toList([]), new None(), toList([])],
  );
  let $ = _block;
  let description = $[0];
  let flags = $[1];
  let unnamed_args$1 = $[2];
  let named_args = $[3];
  return new $help.Command(
    new $help.Metadata(name, description),
    flags,
    build_subcommands_help(node.subcommands),
    $option.map(
      unnamed_args$1,
      (args) => {
        if (args instanceof EqArgs) {
          let n = args[0];
          return new $help.EqArgs(n);
        } else {
          let n = args[0];
          return new $help.MinArgs(n);
        }
      },
    ),
    named_args,
  );
}

function new_flags() {
  return new Flags($dict.new$());
}

function empty_command() {
  return new CommandNode(new None(), $dict.new$(), new_flags(), "");
}

export function command(runner) {
  return new Command(runner, new_flags(), "", new None(), toList([]));
}

function access_type_error(flag_type) {
  return $snag.error("cannot access flag as " + flag_type);
}

function flag_not_provided_error() {
  return $snag.error("no value provided");
}

function construct_value(input, internal, constructor) {
  return $result.map(
    internal.parser(input),
    (val) => {
      return constructor(
        (() => {
          let _record = internal;
          return new FlagInternals(new Some(val), _record.parser);
        })(),
      );
    },
  );
}

function compute_flag(input, current) {
  let _block;
  if (current instanceof I) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new I(var0); },
      );
    };
  } else if (current instanceof LI) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new LI(var0); },
      );
    };
  } else if (current instanceof F) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new F(var0); },
      );
    };
  } else if (current instanceof LF) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new LF(var0); },
      );
    };
  } else if (current instanceof S) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new S(var0); },
      );
    };
  } else if (current instanceof LS) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new LS(var0); },
      );
    };
  } else {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new B(var0); },
      );
    };
  }
  let _pipe = input;
  let _pipe$1 = _block(_pipe);
  return $snag.context(_pipe$1, "failed to compute value for flag");
}

function layer_invalid_flag(err, flag) {
  return $snag.layer(err, ("invalid flag '" + flag) + "'");
}

function no_value_flag_err(flag_input) {
  let _pipe = (("flag '" + flag_input) + "' has no assigned value");
  let _pipe$1 = $snag.new$(_pipe);
  return layer_invalid_flag(_pipe$1, flag_input);
}

function undefined_flag_err(key) {
  let _pipe = "flag provided but not defined";
  let _pipe$1 = $snag.new$(_pipe);
  return layer_invalid_flag(_pipe$1, key);
}

function cannot_parse(value, kind) {
  let _pipe = ((("cannot parse value '" + value) + "' as ") + kind);
  return $snag.new$(_pipe);
}

function get(flags, name) {
  let _pipe = $dict.get(flags.internal, name);
  return $result.replace_error(_pipe, undefined_flag_err(name));
}

function update_flag_value(flags, data) {
  let key = data[0];
  let input = data[1];
  return $result.try$(
    get(flags, key),
    (contents) => {
      return $result.map(
        (() => {
          let _pipe = compute_flag(input, contents.value);
          return $result.map_error(
            _pipe,
            (_capture) => { return layer_invalid_flag(_capture, key); },
          );
        })(),
        (value) => {
          return insert(
            flags,
            key,
            (() => {
              let _record = contents;
              return new FlagEntry(value, _record.description);
            })(),
          );
        },
      );
    },
  );
}

function attempt_toggle_flag(flags, key) {
  return $result.try$(
    get(flags, key),
    (contents) => {
      let $ = contents.value;
      if ($ instanceof B &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        let internal = $[0];
        let _block;
        let _record = internal;
        _block = new FlagInternals(new Some(true), _record.parser);
        let _pipe = _block;
        let _pipe$1 = new B(_pipe);
        let _pipe$2 = ((val) => {
          let _record$1 = contents;
          return new FlagEntry(val, _record$1.description);
        })(_pipe$1);
        let _pipe$3 = $dict.insert(flags.internal, key, _pipe$2);
        let _pipe$4 = new Flags(_pipe$3);
        return new Ok(_pipe$4);
      } else if ($ instanceof B &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let internal = $[0];
        let val = $[0].value[0];
        let _block;
        let _record = internal;
        _block = new FlagInternals(new Some(!val), _record.parser);
        let _pipe = _block;
        let _pipe$1 = new B(_pipe);
        let _pipe$2 = ((val) => {
          let _record$1 = contents;
          return new FlagEntry(val, _record$1.description);
        })(_pipe$1);
        let _pipe$3 = $dict.insert(flags.internal, key, _pipe$2);
        let _pipe$4 = new Flags(_pipe$3);
        return new Ok(_pipe$4);
      } else {
        return new Error(no_value_flag_err(key));
      }
    },
  );
}

function get_value(flags, key, kind) {
  let _pipe = get(flags, key);
  let _pipe$1 = $result.try$(_pipe, kind);
  return $snag.context(
    _pipe$1,
    ("failed to retrieve value for flag '" + key) + "'",
  );
}

export function get_flag(flags, flag) {
  return flag.getter(flags, flag.name);
}

function get_int_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof I &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof I &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("int");
      }
    },
  );
}

export function int_flag(name) {
  return new_builder(
    name,
    (var0) => { return new I(var0); },
    get_int_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $int.parse(_pipe);
      return $result.replace_error(_pipe$1, cannot_parse(input, "int"));
    },
  );
}

function get_ints_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof LI &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof LI &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("int list");
      }
    },
  );
}

export function ints_flag(name) {
  return new_builder(
    name,
    (var0) => { return new LI(var0); },
    get_ints_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $string.split(_pipe, ",");
      let _pipe$2 = $list.try_map(_pipe$1, $int.parse);
      return $result.replace_error(_pipe$2, cannot_parse(input, "int list"));
    },
  );
}

function get_bool_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof B &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof B &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("bool");
      }
    },
  );
}

export function bool_flag(name) {
  return new_builder(
    name,
    (var0) => { return new B(var0); },
    get_bool_flag,
    (input) => {
      let $ = $string.lowercase(input);
      if ($ === "true") {
        return new Ok(true);
      } else if ($ === "t") {
        return new Ok(true);
      } else if ($ === "false") {
        return new Ok(false);
      } else if ($ === "f") {
        return new Ok(false);
      } else {
        return new Error(cannot_parse(input, "bool"));
      }
    },
  );
}

function get_string_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof S &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof S &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("string");
      }
    },
  );
}

export function string_flag(name) {
  return new_builder(
    name,
    (var0) => { return new S(var0); },
    get_string_flag,
    (s) => { return new Ok(s); },
  );
}

function get_strings_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof LS &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof LS &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("string list");
      }
    },
  );
}

export function strings_flag(name) {
  return new_builder(
    name,
    (var0) => { return new LS(var0); },
    get_strings_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $string.split(_pipe, ",");
      return new Ok(_pipe$1);
    },
  );
}

function get_floats(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof F &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof F &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("float");
      }
    },
  );
}

export function float_flag(name) {
  return new_builder(
    name,
    (var0) => { return new F(var0); },
    get_floats,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $float.parse(_pipe);
      return $result.replace_error(_pipe$1, cannot_parse(input, "float"));
    },
  );
}

function get_floats_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof LF &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof Some) {
        let val = $[0].value[0];
        return new Ok(val);
      } else if ($ instanceof LF &&
      $[0] instanceof FlagInternals &&
      $[0].value instanceof None) {
        return flag_not_provided_error();
      } else {
        return access_type_error("float list");
      }
    },
  );
}

export function floats_flag(name) {
  return new_builder(
    name,
    (var0) => { return new LF(var0); },
    get_floats_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $string.split(_pipe, ",");
      let _pipe$2 = $list.try_map(_pipe$1, $float.parse);
      return $result.replace_error(_pipe$2, cannot_parse(input, "float list"));
    },
  );
}

function do_update_at(node, path, f) {
  if (path.hasLength(0)) {
    return f(node);
  } else {
    let next = path.head;
    let rest = path.tail;
    let _record = node;
    return new CommandNode(
      _record.contents,
      $dict.upsert(
        node.subcommands,
        next,
        (found) => {
          let _pipe = found;
          let _pipe$1 = $option.lazy_unwrap(_pipe, empty_command);
          return do_update_at(_pipe$1, rest, f);
        },
      ),
      _record.group_flags,
      _record.description,
    );
  }
}

function update_at(glint, path, f) {
  let _record = glint;
  return new Glint(
    _record.config,
    do_update_at(glint.cmd, sanitize_path(path), f),
  );
}

export function path_help(glint, path, description) {
  return update_at(
    glint,
    path,
    (node) => {
      let _record = node;
      return new CommandNode(
        _record.contents,
        _record.subcommands,
        _record.group_flags,
        description,
      );
    },
  );
}

export function add(glint, path, command) {
  return update_at(
    glint,
    path,
    (node) => {
      let _record = node;
      return new CommandNode(
        new Some(
          new InternalCommand(
            command.do,
            command.flags,
            command.unnamed_args,
            command.named_args,
          ),
        ),
        _record.subcommands,
        _record.group_flags,
        command.description,
      );
    },
  );
}

export function group_flag(glint, path, flag) {
  return update_at(
    glint,
    path,
    (node) => {
      let _record = node;
      return new CommandNode(
        _record.contents,
        _record.subcommands,
        insert(node.group_flags, flag.name, build_flag(flag)),
        _record.description,
      );
    },
  );
}

const default_config = /* @__PURE__ */ new Config(
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  false,
  /* @__PURE__ */ new None(),
  true,
  4,
  80,
  20,
  2,
);

export function new$() {
  return new Glint(default_config, empty_command());
}

const flag_prefix = "--";

const flag_delimiter = "=";

function build_help_config(config) {
  return new $help.Config(
    config.name,
    $option.map(config.pretty_help, (p) => { return p.usage; }),
    $option.map(config.pretty_help, (p) => { return p.flags; }),
    $option.map(config.pretty_help, (p) => { return p.subcommands; }),
    config.as_module,
    config.description,
    config.indent_width,
    config.max_output_width,
    config.min_first_column_width,
    config.column_gap,
    flag_prefix,
    flag_delimiter,
  );
}

function cmd_help(path, cmd, config) {
  let _pipe = path;
  let _pipe$1 = $list.reverse(_pipe);
  let _pipe$2 = $string.join(_pipe$1, " ");
  let _pipe$3 = build_command_help(_pipe$2, cmd);
  return $help.command_help_to_string(_pipe$3, build_help_config(config));
}

function update_flags(flags, flag_input) {
  let flag_input$1 = $string.drop_start(flag_input, $string.length(flag_prefix));
  let $ = $string.split_once(flag_input$1, flag_delimiter);
  if ($.isOk()) {
    let data = $[0];
    return update_flag_value(flags, data);
  } else {
    return attempt_toggle_flag(flags, flag_input$1);
  }
}

function execute_root(path, config, cmd, args, flag_inputs) {
  let _pipe = $result.try$(
    $option.to_result(cmd.contents, $snag.new$("command not found")),
    (contents) => {
      return $result.try$(
        $list.try_fold(
          flag_inputs,
          merge(cmd.group_flags, contents.flags),
          update_flags,
        ),
        (new_flags) => {
          return $result.try$(
            (() => {
              let named = $list.zip(contents.named_args, args);
              let $ = $list.length(named) === $list.length(contents.named_args);
              if ($) {
                return new Ok($dict.from_list(named));
              } else {
                return $snag.error(
                  "unmatched named arguments: " + (() => {
                    let _pipe = contents.named_args;
                    let _pipe$1 = $list.drop(_pipe, $list.length(named));
                    let _pipe$2 = $list.map(
                      _pipe$1,
                      (s) => { return ("'" + s) + "'"; },
                    );
                    return $string.join(_pipe$2, ", ");
                  })(),
                );
              }
            })(),
            (named_args) => {
              let args$1 = $list.drop(args, $dict.size(named_args));
              return $result.map(
                (() => {
                  let $ = contents.unnamed_args;
                  if ($ instanceof Some) {
                    let count = $[0];
                    let _pipe = count;
                    let _pipe$1 = args_compare(_pipe, $list.length(args$1));
                    return $snag.context(
                      _pipe$1,
                      "invalid number of arguments provided",
                    );
                  } else {
                    return new Ok(undefined);
                  }
                })(),
                (_) => {
                  return contents.do(
                    new NamedArgs(named_args),
                    args$1,
                    new_flags,
                  );
                },
              );
            },
          );
        },
      );
    },
  );
  return $result.map_error(
    _pipe,
    (err) => {
      return ((() => {
        let _pipe$1 = err;
        let _pipe$2 = $snag.layer(_pipe$1, "failed to run command");
        return $snag.pretty_print(_pipe$2);
      })() + "\nSee the following help text, available via the '--help' flag.\n\n") + cmd_help(
        path,
        cmd,
        config,
      );
    },
  );
}

function do_execute(
  loop$cmd,
  loop$config,
  loop$args,
  loop$flags,
  loop$help,
  loop$command_path
) {
  while (true) {
    let cmd = loop$cmd;
    let config = loop$config;
    let args = loop$args;
    let flags = loop$flags;
    let help = loop$help;
    let command_path = loop$command_path;
    if (args.hasLength(0) && (help)) {
      return new Ok(new Help(cmd_help(command_path, cmd, config)));
    } else if (args.hasLength(0)) {
      let _pipe = execute_root(command_path, config, cmd, toList([]), flags);
      return $result.map(_pipe, (var0) => { return new Out(var0); });
    } else {
      let arg = args.head;
      let rest = args.tail;
      let $ = $dict.get(cmd.subcommands, arg);
      if ($.isOk()) {
        let sub_command = $[0];
        let _block;
        let _record = sub_command;
        _block = new CommandNode(
          _record.contents,
          _record.subcommands,
          merge(cmd.group_flags, sub_command.group_flags),
          _record.description,
        );
        let _pipe = _block;
        loop$cmd = _pipe;
        loop$config = config;
        loop$args = rest;
        loop$flags = flags;
        loop$help = help;
        loop$command_path = listPrepend(arg, command_path);
      } else if (help) {
        return new Ok(new Help(cmd_help(command_path, cmd, config)));
      } else {
        let _pipe = execute_root(command_path, config, cmd, args, flags);
        return $result.map(_pipe, (var0) => { return new Out(var0); });
      }
    }
  }
}

export function execute(glint, args) {
  let help_flag = flag_prefix + $help.help_flag.meta.name;
  let _block;
  let $1 = $list.partition(args, (s) => { return s === help_flag; });
  if ($1[0].hasLength(0)) {
    let args$1 = $1[1];
    _block = [false, args$1];
  } else {
    let args$1 = $1[1];
    _block = [true, args$1];
  }
  let $ = _block;
  let help = $[0];
  let args$1 = $[1];
  let $2 = $list.partition(
    args$1,
    (_capture) => { return $string.starts_with(_capture, flag_prefix); },
  );
  let flags = $2[0];
  let args$2 = $2[1];
  return do_execute(glint.cmd, glint.config, args$2, flags, help, toList([]));
}

export function run_and_handle(glint, args, handle) {
  let $ = execute(glint, args);
  if (!$.isOk()) {
    let s = $[0];
    $io.println(s);
    let $1 = glint.config.exit;
    if ($1) {
      return exit(1);
    } else {
      return undefined;
    }
  } else if ($.isOk() && $[0] instanceof Help) {
    let s = $[0][0];
    return $io.println(s);
  } else {
    let out = $[0][0];
    handle(out);
    return undefined;
  }
}

export function run(glint, args) {
  return run_and_handle(glint, args, (_) => { return undefined; });
}
