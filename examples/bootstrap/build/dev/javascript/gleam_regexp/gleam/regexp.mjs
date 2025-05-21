import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import {
  compile as do_compile,
  check as do_check,
  split as do_split,
  scan as do_scan,
  replace,
  match_map,
} from "../gleam_regexp_ffi.mjs";

export { match_map, replace };

export class Match extends $CustomType {
  constructor(content, submatches) {
    super();
    this.content = content;
    this.submatches = submatches;
  }
}

export class CompileError extends $CustomType {
  constructor(error, byte_index) {
    super();
    this.error = error;
    this.byte_index = byte_index;
  }
}

export class Options extends $CustomType {
  constructor(case_insensitive, multi_line) {
    super();
    this.case_insensitive = case_insensitive;
    this.multi_line = multi_line;
  }
}

export function compile(pattern, options) {
  return do_compile(pattern, options);
}

export function from_string(pattern) {
  return compile(pattern, new Options(false, false));
}

export function check(regexp, string) {
  return do_check(regexp, string);
}

export function split(regexp, string) {
  return do_split(regexp, string);
}

export function scan(regexp, string) {
  return do_scan(regexp, string);
}
