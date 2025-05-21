import { toList, remainderInt, bitArraySlice } from "../gleam.mjs";
import * as $int from "../gleam/int.mjs";
import * as $order from "../gleam/order.mjs";
import * as $string from "../gleam/string.mjs";
import {
  bit_array_from_string as from_string,
  bit_array_bit_size as bit_size,
  bit_array_byte_size as byte_size,
  bit_array_pad_to_bytes as pad_to_bytes,
  bit_array_slice as slice,
  bit_array_concat as concat,
  encode64 as base64_encode,
  decode64,
  base16_encode,
  base16_decode,
  bit_array_inspect as inspect_loop,
  bit_array_to_int_and_size,
  bit_array_starts_with as starts_with,
  bit_array_to_string as to_string,
} from "../gleam_stdlib.mjs";

export {
  base16_decode,
  base16_encode,
  base64_encode,
  bit_size,
  byte_size,
  concat,
  from_string,
  pad_to_bytes,
  slice,
  starts_with,
  to_string,
};

export function append(first, second) {
  return concat(toList([first, second]));
}

export function base64_decode(encoded) {
  let _block;
  let $ = remainderInt(byte_size(from_string(encoded)), 4);
  if ($ === 0) {
    _block = encoded;
  } else {
    let n = $;
    _block = $string.append(encoded, $string.repeat("=", 4 - n));
  }
  let padded = _block;
  return decode64(padded);
}

export function base64_url_encode(input, padding) {
  let _pipe = base64_encode(input, padding);
  let _pipe$1 = $string.replace(_pipe, "+", "-");
  return $string.replace(_pipe$1, "/", "_");
}

export function base64_url_decode(encoded) {
  let _pipe = encoded;
  let _pipe$1 = $string.replace(_pipe, "-", "+");
  let _pipe$2 = $string.replace(_pipe$1, "_", "/");
  return base64_decode(_pipe$2);
}

export function inspect(input) {
  return inspect_loop(input, "<<") + ">>";
}

export function compare(loop$a, loop$b) {
  while (true) {
    let a = loop$a;
    let b = loop$b;
    if (a.bitSize >= 8 && b.bitSize >= 8) {
      let first_byte = a.byteAt(0);
      let first_rest = bitArraySlice(a, 8);
      let second_byte = b.byteAt(0);
      let second_rest = bitArraySlice(b, 8);
      if (first_byte > second_byte) {
        let f = first_byte;
        let s = second_byte;
        return new $order.Gt();
      } else if (first_byte < second_byte) {
        let f = first_byte;
        let s = second_byte;
        return new $order.Lt();
      } else {
        loop$a = first_rest;
        loop$b = second_rest;
      }
    } else if (a.bitSize == 0 && b.bitSize == 0) {
      return new $order.Eq();
    } else if (b.bitSize == 0) {
      return new $order.Gt();
    } else if (a.bitSize == 0) {
      return new $order.Lt();
    } else {
      let first = a;
      let second = b;
      let $ = bit_array_to_int_and_size(first);
      let $1 = bit_array_to_int_and_size(second);
      if ($[0] > $1[0]) {
        let a$1 = $[0];
        let b$1 = $1[0];
        return new $order.Gt();
      } else if ($[0] < $1[0]) {
        let a$1 = $[0];
        let b$1 = $1[0];
        return new $order.Lt();
      } else if ($[1] > $1[1]) {
        let size_a = $[1];
        let size_b = $1[1];
        return new $order.Gt();
      } else if ($[1] < $1[1]) {
        let size_a = $[1];
        let size_b = $1[1];
        return new $order.Lt();
      } else {
        return new $order.Eq();
      }
    }
  }
}

export function is_utf8(bits) {
  return is_utf8_loop(bits);
}

function is_utf8_loop(bits) {
  let $ = to_string(bits);
  if ($.isOk()) {
    return true;
  } else {
    return false;
  }
}
