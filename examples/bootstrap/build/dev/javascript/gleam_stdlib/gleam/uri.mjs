import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";
import * as $int from "../gleam/int.mjs";
import * as $list from "../gleam/list.mjs";
import * as $option from "../gleam/option.mjs";
import { None, Some } from "../gleam/option.mjs";
import * as $string from "../gleam/string.mjs";
import * as $string_tree from "../gleam/string_tree.mjs";
import {
  pop_codeunit,
  string_codeunit_slice as codeunit_slice,
  parse_query,
  percent_encode,
  percent_decode,
} from "../gleam_stdlib.mjs";

export { parse_query, percent_decode, percent_encode };

export class Uri extends $CustomType {
  constructor(scheme, userinfo, host, port, path, query, fragment) {
    super();
    this.scheme = scheme;
    this.userinfo = userinfo;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
    this.fragment = fragment;
  }
}

function is_valid_host_within_brackets_char(char) {
  return (((((48 >= char) && (char <= 57)) || ((65 >= char) && (char <= 90))) || ((97 >= char) && (char <= 122))) || (char === 58)) || (char === 46);
}

function parse_fragment(rest, pieces) {
  return new Ok(
    (() => {
      let _record = pieces;
      return new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        _record.port,
        _record.path,
        _record.query,
        new Some(rest),
      );
    })(),
  );
}

function parse_query_with_question_mark_loop(
  loop$original,
  loop$uri_string,
  loop$pieces,
  loop$size
) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size = loop$size;
    if (uri_string.startsWith("#") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_fragment(rest, pieces);
    } else if (uri_string.startsWith("#")) {
      let rest = uri_string.slice(1);
      let query = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        _record.port,
        _record.path,
        new Some(query),
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_fragment(rest, pieces$1);
    } else if (uri_string === "") {
      return new Ok(
        (() => {
          let _record = pieces;
          return new Uri(
            _record.scheme,
            _record.userinfo,
            _record.host,
            _record.port,
            _record.path,
            new Some(original),
            _record.fragment,
          );
        })(),
      );
    } else {
      let $ = pop_codeunit(uri_string);
      let rest = $[1];
      loop$original = original;
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$size = size + 1;
    }
  }
}

function parse_query_with_question_mark(uri_string, pieces) {
  return parse_query_with_question_mark_loop(uri_string, uri_string, pieces, 0);
}

function parse_path_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size = loop$size;
    if (uri_string.startsWith("?")) {
      let rest = uri_string.slice(1);
      let path = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        _record.port,
        path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_query_with_question_mark(rest, pieces$1);
    } else if (uri_string.startsWith("#")) {
      let rest = uri_string.slice(1);
      let path = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        _record.port,
        path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_fragment(rest, pieces$1);
    } else if (uri_string === "") {
      return new Ok(
        (() => {
          let _record = pieces;
          return new Uri(
            _record.scheme,
            _record.userinfo,
            _record.host,
            _record.port,
            original,
            _record.query,
            _record.fragment,
          );
        })(),
      );
    } else {
      let $ = pop_codeunit(uri_string);
      let rest = $[1];
      loop$original = original;
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$size = size + 1;
    }
  }
}

function parse_path(uri_string, pieces) {
  return parse_path_loop(uri_string, uri_string, pieces, 0);
}

function parse_port_loop(loop$uri_string, loop$pieces, loop$port) {
  while (true) {
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let port = loop$port;
    if (uri_string.startsWith("0")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10;
    } else if (uri_string.startsWith("1")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 1;
    } else if (uri_string.startsWith("2")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 2;
    } else if (uri_string.startsWith("3")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 3;
    } else if (uri_string.startsWith("4")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 4;
    } else if (uri_string.startsWith("5")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 5;
    } else if (uri_string.startsWith("6")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 6;
    } else if (uri_string.startsWith("7")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 7;
    } else if (uri_string.startsWith("8")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 8;
    } else if (uri_string.startsWith("9")) {
      let rest = uri_string.slice(1);
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$port = port * 10 + 9;
    } else if (uri_string.startsWith("?")) {
      let rest = uri_string.slice(1);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        new Some(port),
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_query_with_question_mark(rest, pieces$1);
    } else if (uri_string.startsWith("#")) {
      let rest = uri_string.slice(1);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        new Some(port),
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_fragment(rest, pieces$1);
    } else if (uri_string.startsWith("/")) {
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        _record.host,
        new Some(port),
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_path(uri_string, pieces$1);
    } else if (uri_string === "") {
      return new Ok(
        (() => {
          let _record = pieces;
          return new Uri(
            _record.scheme,
            _record.userinfo,
            _record.host,
            new Some(port),
            _record.path,
            _record.query,
            _record.fragment,
          );
        })(),
      );
    } else {
      return new Error(undefined);
    }
  }
}

function parse_port(uri_string, pieces) {
  if (uri_string.startsWith(":0")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 0);
  } else if (uri_string.startsWith(":1")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 1);
  } else if (uri_string.startsWith(":2")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 2);
  } else if (uri_string.startsWith(":3")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 3);
  } else if (uri_string.startsWith(":4")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 4);
  } else if (uri_string.startsWith(":5")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 5);
  } else if (uri_string.startsWith(":6")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 6);
  } else if (uri_string.startsWith(":7")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 7);
  } else if (uri_string.startsWith(":8")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 8);
  } else if (uri_string.startsWith(":9")) {
    let rest = uri_string.slice(2);
    return parse_port_loop(rest, pieces, 9);
  } else if (uri_string.startsWith(":")) {
    return new Error(undefined);
  } else if (uri_string.startsWith("?")) {
    let rest = uri_string.slice(1);
    return parse_query_with_question_mark(rest, pieces);
  } else if (uri_string.startsWith("#")) {
    let rest = uri_string.slice(1);
    return parse_fragment(rest, pieces);
  } else if (uri_string.startsWith("/")) {
    return parse_path(uri_string, pieces);
  } else if (uri_string === "") {
    return new Ok(pieces);
  } else {
    return new Error(undefined);
  }
}

function parse_host_outside_of_brackets_loop(
  loop$original,
  loop$uri_string,
  loop$pieces,
  loop$size
) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size = loop$size;
    if (uri_string === "") {
      return new Ok(
        (() => {
          let _record = pieces;
          return new Uri(
            _record.scheme,
            _record.userinfo,
            new Some(original),
            _record.port,
            _record.path,
            _record.query,
            _record.fragment,
          );
        })(),
      );
    } else if (uri_string.startsWith(":")) {
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_port(uri_string, pieces$1);
    } else if (uri_string.startsWith("/")) {
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_path(uri_string, pieces$1);
    } else if (uri_string.startsWith("?")) {
      let rest = uri_string.slice(1);
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_query_with_question_mark(rest, pieces$1);
    } else if (uri_string.startsWith("#")) {
      let rest = uri_string.slice(1);
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_fragment(rest, pieces$1);
    } else {
      let $ = pop_codeunit(uri_string);
      let rest = $[1];
      loop$original = original;
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$size = size + 1;
    }
  }
}

function parse_host_within_brackets_loop(
  loop$original,
  loop$uri_string,
  loop$pieces,
  loop$size
) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size = loop$size;
    if (uri_string === "") {
      return new Ok(
        (() => {
          let _record = pieces;
          return new Uri(
            _record.scheme,
            _record.userinfo,
            new Some(uri_string),
            _record.port,
            _record.path,
            _record.query,
            _record.fragment,
          );
        })(),
      );
    } else if (uri_string.startsWith("]") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_port(rest, pieces);
    } else if (uri_string.startsWith("]")) {
      let rest = uri_string.slice(1);
      let host = codeunit_slice(original, 0, size + 1);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_port(rest, pieces$1);
    } else if (uri_string.startsWith("/") && (size === 0)) {
      return parse_path(uri_string, pieces);
    } else if (uri_string.startsWith("/")) {
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_path(uri_string, pieces$1);
    } else if (uri_string.startsWith("?") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_query_with_question_mark(rest, pieces);
    } else if (uri_string.startsWith("?")) {
      let rest = uri_string.slice(1);
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_query_with_question_mark(rest, pieces$1);
    } else if (uri_string.startsWith("#") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_fragment(rest, pieces);
    } else if (uri_string.startsWith("#")) {
      let rest = uri_string.slice(1);
      let host = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        _record.userinfo,
        new Some(host),
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_fragment(rest, pieces$1);
    } else {
      let $ = pop_codeunit(uri_string);
      let char = $[0];
      let rest = $[1];
      let $1 = is_valid_host_within_brackets_char(char);
      if ($1) {
        loop$original = original;
        loop$uri_string = rest;
        loop$pieces = pieces;
        loop$size = size + 1;
      } else {
        return parse_host_outside_of_brackets_loop(
          original,
          original,
          pieces,
          0,
        );
      }
    }
  }
}

function parse_host_within_brackets(uri_string, pieces) {
  return parse_host_within_brackets_loop(uri_string, uri_string, pieces, 0);
}

function parse_host_outside_of_brackets(uri_string, pieces) {
  return parse_host_outside_of_brackets_loop(uri_string, uri_string, pieces, 0);
}

function parse_host(uri_string, pieces) {
  if (uri_string.startsWith("[")) {
    return parse_host_within_brackets(uri_string, pieces);
  } else if (uri_string.startsWith(":")) {
    let _block;
    let _record = pieces;
    _block = new Uri(
      _record.scheme,
      _record.userinfo,
      new Some(""),
      _record.port,
      _record.path,
      _record.query,
      _record.fragment,
    );
    let pieces$1 = _block;
    return parse_port(uri_string, pieces$1);
  } else if (uri_string === "") {
    return new Ok(
      (() => {
        let _record = pieces;
        return new Uri(
          _record.scheme,
          _record.userinfo,
          new Some(""),
          _record.port,
          _record.path,
          _record.query,
          _record.fragment,
        );
      })(),
    );
  } else {
    return parse_host_outside_of_brackets(uri_string, pieces);
  }
}

function parse_userinfo_loop(
  loop$original,
  loop$uri_string,
  loop$pieces,
  loop$size
) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size = loop$size;
    if (uri_string.startsWith("@") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_host(rest, pieces);
    } else if (uri_string.startsWith("@")) {
      let rest = uri_string.slice(1);
      let userinfo = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        _record.scheme,
        new Some(userinfo),
        _record.host,
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_host(rest, pieces$1);
    } else if (uri_string === "") {
      return parse_host(original, pieces);
    } else if (uri_string.startsWith("/")) {
      return parse_host(original, pieces);
    } else if (uri_string.startsWith("?")) {
      return parse_host(original, pieces);
    } else if (uri_string.startsWith("#")) {
      return parse_host(original, pieces);
    } else {
      let $ = pop_codeunit(uri_string);
      let rest = $[1];
      loop$original = original;
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$size = size + 1;
    }
  }
}

function parse_authority_pieces(string, pieces) {
  return parse_userinfo_loop(string, string, pieces, 0);
}

function parse_authority_with_slashes(uri_string, pieces) {
  if (uri_string === "//") {
    return new Ok(
      (() => {
        let _record = pieces;
        return new Uri(
          _record.scheme,
          _record.userinfo,
          new Some(""),
          _record.port,
          _record.path,
          _record.query,
          _record.fragment,
        );
      })(),
    );
  } else if (uri_string.startsWith("//")) {
    let rest = uri_string.slice(2);
    return parse_authority_pieces(rest, pieces);
  } else {
    return parse_path(uri_string, pieces);
  }
}

function parse_scheme_loop(
  loop$original,
  loop$uri_string,
  loop$pieces,
  loop$size
) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size = loop$size;
    if (uri_string.startsWith("/") && (size === 0)) {
      return parse_authority_with_slashes(uri_string, pieces);
    } else if (uri_string.startsWith("/")) {
      let scheme = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        new Some($string.lowercase(scheme)),
        _record.userinfo,
        _record.host,
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_authority_with_slashes(uri_string, pieces$1);
    } else if (uri_string.startsWith("?") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_query_with_question_mark(rest, pieces);
    } else if (uri_string.startsWith("?")) {
      let rest = uri_string.slice(1);
      let scheme = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        new Some($string.lowercase(scheme)),
        _record.userinfo,
        _record.host,
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_query_with_question_mark(rest, pieces$1);
    } else if (uri_string.startsWith("#") && (size === 0)) {
      let rest = uri_string.slice(1);
      return parse_fragment(rest, pieces);
    } else if (uri_string.startsWith("#")) {
      let rest = uri_string.slice(1);
      let scheme = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        new Some($string.lowercase(scheme)),
        _record.userinfo,
        _record.host,
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_fragment(rest, pieces$1);
    } else if (uri_string.startsWith(":") && (size === 0)) {
      return new Error(undefined);
    } else if (uri_string.startsWith(":")) {
      let rest = uri_string.slice(1);
      let scheme = codeunit_slice(original, 0, size);
      let _block;
      let _record = pieces;
      _block = new Uri(
        new Some($string.lowercase(scheme)),
        _record.userinfo,
        _record.host,
        _record.port,
        _record.path,
        _record.query,
        _record.fragment,
      );
      let pieces$1 = _block;
      return parse_authority_with_slashes(rest, pieces$1);
    } else if (uri_string === "") {
      return new Ok(
        (() => {
          let _record = pieces;
          return new Uri(
            _record.scheme,
            _record.userinfo,
            _record.host,
            _record.port,
            original,
            _record.query,
            _record.fragment,
          );
        })(),
      );
    } else {
      let $ = pop_codeunit(uri_string);
      let rest = $[1];
      loop$original = original;
      loop$uri_string = rest;
      loop$pieces = pieces;
      loop$size = size + 1;
    }
  }
}

function query_pair(pair) {
  return $string_tree.from_strings(
    toList([percent_encode(pair[0]), "=", percent_encode(pair[1])]),
  );
}

export function query_to_string(query) {
  let _pipe = query;
  let _pipe$1 = $list.map(_pipe, query_pair);
  let _pipe$2 = $list.intersperse(_pipe$1, $string_tree.from_string("&"));
  let _pipe$3 = $string_tree.concat(_pipe$2);
  return $string_tree.to_string(_pipe$3);
}

function remove_dot_segments_loop(loop$input, loop$accumulator) {
  while (true) {
    let input = loop$input;
    let accumulator = loop$accumulator;
    if (input.hasLength(0)) {
      return $list.reverse(accumulator);
    } else {
      let segment = input.head;
      let rest = input.tail;
      let _block;
      if (segment === "") {
        let accumulator$1 = accumulator;
        _block = accumulator$1;
      } else if (segment === ".") {
        let accumulator$1 = accumulator;
        _block = accumulator$1;
      } else if (segment === ".." && accumulator.hasLength(0)) {
        _block = toList([]);
      } else if (segment === ".." && accumulator.atLeastLength(1)) {
        let accumulator$1 = accumulator.tail;
        _block = accumulator$1;
      } else {
        let segment$1 = segment;
        let accumulator$1 = accumulator;
        _block = listPrepend(segment$1, accumulator$1);
      }
      let accumulator$1 = _block;
      loop$input = rest;
      loop$accumulator = accumulator$1;
    }
  }
}

function remove_dot_segments(input) {
  return remove_dot_segments_loop(input, toList([]));
}

export function path_segments(path) {
  return remove_dot_segments($string.split(path, "/"));
}

export function to_string(uri) {
  let _block;
  let $ = uri.fragment;
  if ($ instanceof Some) {
    let fragment = $[0];
    _block = toList(["#", fragment]);
  } else {
    _block = toList([]);
  }
  let parts = _block;
  let _block$1;
  let $1 = uri.query;
  if ($1 instanceof Some) {
    let query = $1[0];
    _block$1 = listPrepend("?", listPrepend(query, parts));
  } else {
    _block$1 = parts;
  }
  let parts$1 = _block$1;
  let parts$2 = listPrepend(uri.path, parts$1);
  let _block$2;
  let $2 = uri.host;
  let $3 = $string.starts_with(uri.path, "/");
  if ($2 instanceof Some && !$3 && ($2[0] !== "")) {
    let host = $2[0];
    _block$2 = listPrepend("/", parts$2);
  } else {
    _block$2 = parts$2;
  }
  let parts$3 = _block$2;
  let _block$3;
  let $4 = uri.host;
  let $5 = uri.port;
  if ($4 instanceof Some && $5 instanceof Some) {
    let port = $5[0];
    _block$3 = listPrepend(":", listPrepend($int.to_string(port), parts$3));
  } else {
    _block$3 = parts$3;
  }
  let parts$4 = _block$3;
  let _block$4;
  let $6 = uri.scheme;
  let $7 = uri.userinfo;
  let $8 = uri.host;
  if ($6 instanceof Some && $7 instanceof Some && $8 instanceof Some) {
    let s = $6[0];
    let u = $7[0];
    let h = $8[0];
    _block$4 = listPrepend(
      s,
      listPrepend(
        "://",
        listPrepend(u, listPrepend("@", listPrepend(h, parts$4))),
      ),
    );
  } else if ($6 instanceof Some && $7 instanceof None && $8 instanceof Some) {
    let s = $6[0];
    let h = $8[0];
    _block$4 = listPrepend(s, listPrepend("://", listPrepend(h, parts$4)));
  } else if ($6 instanceof Some && $7 instanceof Some && $8 instanceof None) {
    let s = $6[0];
    _block$4 = listPrepend(s, listPrepend(":", parts$4));
  } else if ($6 instanceof Some && $7 instanceof None && $8 instanceof None) {
    let s = $6[0];
    _block$4 = listPrepend(s, listPrepend(":", parts$4));
  } else if ($6 instanceof None && $7 instanceof None && $8 instanceof Some) {
    let h = $8[0];
    _block$4 = listPrepend("//", listPrepend(h, parts$4));
  } else {
    _block$4 = parts$4;
  }
  let parts$5 = _block$4;
  return $string.concat(parts$5);
}

export function origin(uri) {
  let scheme = uri.scheme;
  let host = uri.host;
  let port = uri.port;
  if (host instanceof Some &&
  scheme instanceof Some &&
  scheme[0] === "https" &&
  (isEqual(port, new Some(443)))) {
    let h = host[0];
    return new Ok($string.concat(toList(["https://", h])));
  } else if (host instanceof Some &&
  scheme instanceof Some &&
  scheme[0] === "http" &&
  (isEqual(port, new Some(80)))) {
    let h = host[0];
    return new Ok($string.concat(toList(["http://", h])));
  } else if (host instanceof Some &&
  scheme instanceof Some &&
  ((scheme[0] === "http") || (scheme[0] === "https"))) {
    let h = host[0];
    let s = scheme[0];
    if (port instanceof Some) {
      let p = port[0];
      return new Ok(
        $string.concat(toList([s, "://", h, ":", $int.to_string(p)])),
      );
    } else {
      return new Ok($string.concat(toList([s, "://", h])));
    }
  } else {
    return new Error(undefined);
  }
}

function drop_last(elements) {
  return $list.take(elements, $list.length(elements) - 1);
}

function join_segments(segments) {
  return $string.join(listPrepend("", segments), "/");
}

export function merge(base, relative) {
  if (base instanceof Uri &&
  base.scheme instanceof Some &&
  base.host instanceof Some) {
    if (relative instanceof Uri && relative.host instanceof Some) {
      let _block;
      let _pipe = $string.split(relative.path, "/");
      let _pipe$1 = remove_dot_segments(_pipe);
      _block = join_segments(_pipe$1);
      let path = _block;
      let resolved = new Uri(
        $option.or(relative.scheme, base.scheme),
        new None(),
        relative.host,
        $option.or(relative.port, base.port),
        path,
        relative.query,
        relative.fragment,
      );
      return new Ok(resolved);
    } else {
      let _block;
      let $1 = relative.path;
      if ($1 === "") {
        _block = [base.path, $option.or(relative.query, base.query)];
      } else {
        let _block$1;
        let $2 = $string.starts_with(relative.path, "/");
        if ($2) {
          _block$1 = $string.split(relative.path, "/");
        } else {
          let _pipe = $string.split(base.path, "/");
          let _pipe$1 = drop_last(_pipe);
          _block$1 = $list.append(_pipe$1, $string.split(relative.path, "/"));
        }
        let path_segments$1 = _block$1;
        let _block$2;
        let _pipe = path_segments$1;
        let _pipe$1 = remove_dot_segments(_pipe);
        _block$2 = join_segments(_pipe$1);
        let path = _block$2;
        _block = [path, relative.query];
      }
      let $ = _block;
      let new_path = $[0];
      let new_query = $[1];
      let resolved = new Uri(
        base.scheme,
        new None(),
        base.host,
        base.port,
        new_path,
        new_query,
        relative.fragment,
      );
      return new Ok(resolved);
    }
  } else {
    return new Error(undefined);
  }
}

export const empty = /* @__PURE__ */ new Uri(
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  "",
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
);

export function parse(uri_string) {
  return parse_scheme_loop(uri_string, uri_string, empty, 0);
}
