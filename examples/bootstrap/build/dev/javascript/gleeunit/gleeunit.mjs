import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";
import { main as do_main } from "./gleeunit_ffi.mjs";

class Utf8 extends $CustomType {}

class GleeunitProgress extends $CustomType {}

class Colored extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Verbose extends $CustomType {}

class NoTty extends $CustomType {}

class Report extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function gleam_to_erlang_module_name(path) {
  let _pipe = path;
  let _pipe$1 = $string.replace(_pipe, ".gleam", "");
  let _pipe$2 = $string.replace(_pipe$1, ".erl", "");
  return $string.replace(_pipe$2, "/", "@");
}

export function main() {
  return do_main();
}
