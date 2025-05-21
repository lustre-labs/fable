import * as $stdlib$dict from "../../../gleam_stdlib/dict.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import * as $component from "../../../lustre/lustre/component.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import * as $event from "../../../lustre/lustre/event.mjs";
import {
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  bitArraySlice,
  bitArraySliceToInt,
  BitArray as $BitArray,
  List as $List,
  UtfCodepoint as $UtfCodepoint,
} from "../../gleam.mjs";
import * as $book from "../../lustre_fable/book.mjs";
import { Book } from "../../lustre_fable/book.mjs";
import * as $story from "../../lustre_fable/story.mjs";
import { StoryBuilder, StoryConfig } from "../../lustre_fable/story.mjs";
import * as $value from "../../lustre_fable/value.mjs";
import { PrimitiveBool, PrimitiveString } from "../../lustre_fable/value.mjs";

export function book(title) {
  return new Book(title, toList([]), toList([]), toList([]));
}

export function chapter(book, title, stories) {
  let _record = book;
  return new Book(
    _record.title,
    _record.stylesheets,
    _record.external_stylesheets,
    listPrepend([title, stories], book.chapters),
  );
}

export function stylesheet(book, stylesheet) {
  let _record = book;
  return new Book(
    _record.title,
    listPrepend(stylesheet, book.stylesheets),
    _record.external_stylesheets,
    _record.chapters,
  );
}

export function external_stylesheet(book, href) {
  let _record = book;
  return new Book(
    _record.title,
    _record.stylesheets,
    listPrepend(href, book.external_stylesheets),
    _record.chapters,
  );
}

export function start(book) {
  let $ = $book.start(book);
  
  return undefined;
}

export function story(title, builder) {
  let _record = builder().run(0);
  return new StoryConfig(title, _record.inputs, _record.options, _record.view);
}

export function scene(view) {
  return new StoryBuilder(
    (_) => {
      return new StoryConfig(
        "",
        toList([]),
        toList([$component.adopt_styles(false)]),
        view,
      );
    },
  );
}

function control(wrap, read, view, next) {
  return new StoryBuilder(
    (key) => {
      let state = (model) => {
        let _pipe = model.lookup;
        let _pipe$1 = $dict.get(_pipe, key);
        let _pipe$2 = $result.unwrap(_pipe$1, new PrimitiveString(""));
        return read(_pipe$2);
      };
      let set_state = (value) => {
        let _pipe = value;
        let _pipe$1 = wrap(_pipe);
        return ((_capture) => {
          return new $story.UserEditedValue(key, _capture);
        })(_pipe$1);
      };
      let input$1 = (model) => { return view(state(model), set_state); };
      let option = $component.on_property_change(
        $int.to_string(key),
        (() => {
          let _pipe = $value.decoder();
          let _pipe$1 = $decode.map(
            _pipe,
            (_capture) => {
              return new $story.ComponentUpdatedValue(key, _capture);
            },
          );
          return $decode.map(
            _pipe$1,
            (msg) => { return echo(msg, "src/lustre/dev/fable.gleam", 185); },
          );
        })(),
      );
      let $ = next(state, set_state).run(key + 1);
      let title = $.title;
      let inputs = $.inputs;
      let options = $.options;
      let view$1 = $.view;
      return new StoryConfig(
        title,
        listPrepend(input$1, inputs),
        listPrepend(option, options),
        view$1,
      );
    },
  );
}

export function input(label, next) {
  return ((_capture) => {
    return control(
      (var0) => { return new PrimitiveString(var0); },
      $value.as_string,
      _capture,
      next,
    );
  })(
    (value, set_value) => {
      return $html.label(
        toList([]),
        toList([
          $html.p(toList([]), toList([$html.text(label)])),
          $html.input(
            toList([$attribute.value(value), $event.on_input(set_value)]),
          ),
        ]),
      );
    },
  );
}

export function checkbox(label, next) {
  return ((_capture) => {
    return control(
      (var0) => { return new PrimitiveBool(var0); },
      $value.as_bool,
      _capture,
      next,
    );
  })(
    (value, set_value) => {
      return $html.label(
        toList([]),
        toList([
          $html.p(toList([]), toList([$html.text(label)])),
          $html.input(
            toList([
              $attribute.checked(value),
              $attribute.type_("checkbox"),
              $event.on_check(set_value),
            ]),
          ),
        ]),
      );
    },
  );
}

export function select(label, options, next) {
  return ((_capture) => {
    return control(
      (var0) => { return new PrimitiveString(var0); },
      $value.as_string,
      _capture,
      next,
    );
  })(
    (value, set_value) => {
      let options$1 = $list.map(
        options,
        (option) => {
          return $html.option(
            toList([
              $attribute.value(option[0]),
              $attribute.selected(value === option[0]),
            ]),
            option[1],
          );
        },
      );
      return $html.label(
        toList([]),
        toList([
          $html.p(toList([]), toList([$html.text(label)])),
          $html.select(
            toList([$event.on_change(set_value)]),
            listPrepend(
              $html.option(
                toList([$attribute.value(""), $attribute.selected(value === "")]),
                "",
              ),
              options$1,
            ),
          ),
        ]),
      );
    },
  );
}

function echo(value, file, line) {
  const grey = "\u001b[90m";
  const reset_color = "\u001b[39m";
  const file_line = `${file}:${line}`;
  const string_value = echo$inspect(value);

  if (globalThis.process?.stderr?.write) {
    // If we're in Node.js, use `stderr`
    const string = `${grey}${file_line}${reset_color}\n${string_value}\n`;
    process.stderr.write(string);
  } else if (globalThis.Deno) {
    // If we're in Deno, use `stderr`
    const string = `${grey}${file_line}${reset_color}\n${string_value}\n`;
    globalThis.Deno.stderr.writeSync(new TextEncoder().encode(string));
  } else {
    // Otherwise, use `console.log`
    // The browser's console.log doesn't support ansi escape codes
    const string = `${file_line}\n${string_value}`;
    globalThis.console.log(string);
  }

  return value;
}

function echo$inspectString(str) {
  let new_str = '"';
  for (let i = 0; i < str.length; i++) {
    let char = str[i];
    if (char == "\n") new_str += "\\n";
    else if (char == "\r") new_str += "\\r";
    else if (char == "\t") new_str += "\\t";
    else if (char == "\f") new_str += "\\f";
    else if (char == "\\") new_str += "\\\\";
    else if (char == '"') new_str += '\\"';
    else if (char < " " || (char > "~" && char < "\u{00A0}")) {
      new_str +=
        "\\u{" +
        char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") +
        "}";
    } else {
      new_str += char;
    }
  }
  new_str += '"';
  return new_str;
}

function echo$inspectDict(map) {
  let body = "dict.from_list([";
  let first = true;

  let key_value_pairs = [];
  map.forEach((value, key) => {
    key_value_pairs.push([key, value]);
  });
  key_value_pairs.sort();
  key_value_pairs.forEach(([key, value]) => {
    if (!first) body = body + ", ";
    body = body + "#(" + echo$inspect(key) + ", " + echo$inspect(value) + ")";
    first = false;
  });
  return body + "])";
}

function echo$inspectCustomType(record) {
  const props = globalThis.Object.keys(record)
    .map((label) => {
      const value = echo$inspect(record[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    })
    .join(", ");
  return props
    ? `${record.constructor.name}(${props})`
    : record.constructor.name;
}

function echo$inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${echo$inspect(k)}: ${echo$inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body}})`;
}

function echo$inspect(v) {
  const t = typeof v;
  if (v === true) return "True";
  if (v === false) return "False";
  if (v === null) return "//js(null)";
  if (v === undefined) return "Nil";
  if (t === "string") return echo$inspectString(v);
  if (t === "bigint" || t === "number") return v.toString();
  if (globalThis.Array.isArray(v))
    return `#(${v.map(echo$inspect).join(", ")})`;
  if (v instanceof $List)
    return `[${v.toArray().map(echo$inspect).join(", ")}]`;
  if (v instanceof $UtfCodepoint)
    return `//utfcodepoint(${String.fromCodePoint(v.value)})`;
  if (v instanceof $BitArray) return echo$inspectBitArray(v);
  if (v instanceof $CustomType) return echo$inspectCustomType(v);
  if (echo$isDict(v)) return echo$inspectDict(v);
  if (v instanceof Set)
    return `//js(Set(${[...v].map(echo$inspect).join(", ")}))`;
  if (v instanceof RegExp) return `//js(${v})`;
  if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    const args = [];
    for (const i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  return echo$inspectObject(v);
}

function echo$inspectBitArray(bitArray) {
  // We take all the aligned bytes of the bit array starting from `bitOffset`
  // up to the end of the section containing all the aligned bytes.
  let endOfAlignedBytes =
    bitArray.bitOffset + 8 * Math.trunc(bitArray.bitSize / 8);
  let alignedBytes = bitArraySlice(
    bitArray,
    bitArray.bitOffset,
    endOfAlignedBytes,
  );

  // Now we need to get the remaining unaligned bits at the end of the bit array.
  // They will start after `endOfAlignedBytes` and end at `bitArray.bitSize`
  let remainingUnalignedBits = bitArray.bitSize % 8;
  if (remainingUnalignedBits > 0) {
    let remainingBits = bitArraySliceToInt(
      bitArray,
      endOfAlignedBytes,
      bitArray.bitSize,
      false,
      false,
    );
    let alignedBytesArray = Array.from(alignedBytes.rawBuffer);
    let suffix = `${remainingBits}:size(${remainingUnalignedBits})`;
    if (alignedBytesArray.length === 0) {
      return `<<${suffix}>>`;
    } else {
      return `<<${Array.from(alignedBytes.rawBuffer).join(", ")}, ${suffix}>>`;
    }
  } else {
    return `<<${Array.from(alignedBytes.rawBuffer).join(", ")}>>`;
  }
}

function echo$isDict(value) {
  try {
    // We can only check if an object is a stdlib Dict if it is one of the
    // project's dependencies.
    // The `Dict` class is the default export of `stdlib/dict.mjs`
    // that we import as `$stdlib$dict`.
    return value instanceof $stdlib$dict.default;
  } catch {
    // If stdlib is not one of the project's dependencies then `$stdlib$dict`
    // will not have been imported and the check will throw an exception meaning
    // we can't check if something is actually a `Dict`.
    return false;
  }
}

