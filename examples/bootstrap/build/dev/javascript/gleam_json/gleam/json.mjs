import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string_tree from "../../gleam_stdlib/gleam/string_tree.mjs";
import { Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";
import {
  decode as decode_string,
  json_to_string as do_to_string,
  json_to_string as to_string_tree,
  identity as do_string,
  identity as do_bool,
  identity as do_int,
  identity as do_float,
  do_null,
  object as do_object,
  array as do_preprocessed_array,
} from "../gleam_json_ffi.mjs";

export { to_string_tree };

export class UnexpectedEndOfInput extends $CustomType {}

export class UnexpectedByte extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class UnexpectedSequence extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class UnableToDecode extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function do_parse(json, decoder) {
  return $result.then$(
    decode_string(json),
    (dynamic_value) => {
      let _pipe = $decode.run(dynamic_value, decoder);
      return $result.map_error(
        _pipe,
        (var0) => { return new UnableToDecode(var0); },
      );
    },
  );
}

export function parse(json, decoder) {
  return do_parse(json, decoder);
}

function decode_to_dynamic(json) {
  let $ = $bit_array.to_string(json);
  if ($.isOk()) {
    let string$1 = $[0];
    return decode_string(string$1);
  } else {
    return new Error(new UnexpectedByte(""));
  }
}

export function parse_bits(json, decoder) {
  return $result.then$(
    decode_to_dynamic(json),
    (dynamic_value) => {
      let _pipe = $decode.run(dynamic_value, decoder);
      return $result.map_error(
        _pipe,
        (var0) => { return new UnableToDecode(var0); },
      );
    },
  );
}

export function to_string(json) {
  return do_to_string(json);
}

export function string(input) {
  return do_string(input);
}

export function bool(input) {
  return do_bool(input);
}

export function int(input) {
  return do_int(input);
}

export function float(input) {
  return do_float(input);
}

export function null$() {
  return do_null();
}

export function nullable(input, inner_type) {
  if (input instanceof Some) {
    let value = input[0];
    return inner_type(value);
  } else {
    return null$();
  }
}

export function object(entries) {
  return do_object(entries);
}

export function preprocessed_array(from) {
  return do_preprocessed_array(from);
}

export function array(entries, inner_type) {
  let _pipe = entries;
  let _pipe$1 = $list.map(_pipe, inner_type);
  return preprocessed_array(_pipe$1);
}

export function dict(dict, keys, values) {
  return object(
    $dict.fold(
      dict,
      toList([]),
      (acc, k, v) => { return listPrepend([keys(k), values(v)], acc); },
    ),
  );
}
