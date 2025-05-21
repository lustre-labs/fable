import * as $dict from "../gleam/dict.mjs";
import {
  classify_dynamic as classify,
  identity as from,
  identity as bool,
  identity as string,
  identity as float,
  identity as int,
  identity as bit_array,
  identity as list,
  list_to_array as array,
  identity as cast,
} from "../gleam_stdlib.mjs";

export { array, bit_array, bool, classify, float, from, int, list, string };

export function properties(entries) {
  return cast($dict.from_list(entries));
}

export function nil() {
  return cast(undefined);
}
