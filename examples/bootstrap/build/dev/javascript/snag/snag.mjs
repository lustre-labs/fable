import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $gleam from "./gleam.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "./gleam.mjs";

export class Snag extends $CustomType {
  constructor(issue, cause) {
    super();
    this.issue = issue;
    this.cause = cause;
  }
}

export function new$(issue) {
  return new Snag(issue, toList([]));
}

export function error(issue) {
  return new Error(new$(issue));
}

export function layer(snag, issue) {
  return new Snag(issue, listPrepend(snag.issue, snag.cause));
}

export function context(result, issue) {
  if (result.isOk()) {
    return result;
  } else {
    let snag = result[0];
    return new Error(layer(snag, issue));
  }
}

export function map_error(result, describer) {
  if (result.isOk()) {
    let a = result[0];
    return new Ok(a);
  } else {
    let b = result[0];
    let _pipe = describer(b);
    return error(_pipe);
  }
}

function pretty_print_cause(cause) {
  let _pipe = cause;
  let _pipe$1 = $list.index_map(
    _pipe,
    (line, index) => {
      return $string.concat(
        toList(["  ", $int.to_string(index), ": ", line, "\n"]),
      );
    },
  );
  return $string.concat(_pipe$1);
}

export function pretty_print(snag) {
  let output = ("error: " + snag.issue) + "\n";
  let $ = snag.cause;
  if ($.hasLength(0)) {
    return output;
  } else {
    let cause = $;
    return (output + "\ncause:\n") + pretty_print_cause(cause);
  }
}

export function line_print(snag) {
  let _pipe = listPrepend($string.append("error: ", snag.issue), snag.cause);
  return $string.join(_pipe, " <- ");
}
