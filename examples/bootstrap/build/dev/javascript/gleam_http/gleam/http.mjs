import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
  toBitArray,
  bitArraySlice,
} from "../gleam.mjs";

export class Get extends $CustomType {}

export class Post extends $CustomType {}

export class Head extends $CustomType {}

export class Put extends $CustomType {}

export class Delete extends $CustomType {}

export class Trace extends $CustomType {}

export class Connect extends $CustomType {}

export class Options extends $CustomType {}

export class Patch extends $CustomType {}

export class Other extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Http extends $CustomType {}

export class Https extends $CustomType {}

export class MultipartHeaders extends $CustomType {
  constructor(headers, remaining) {
    super();
    this.headers = headers;
    this.remaining = remaining;
  }
}

export class MoreRequiredForHeaders extends $CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
}

export class MultipartBody extends $CustomType {
  constructor(chunk, done, remaining) {
    super();
    this.chunk = chunk;
    this.done = done;
    this.remaining = remaining;
  }
}

export class MoreRequiredForBody extends $CustomType {
  constructor(chunk, continuation) {
    super();
    this.chunk = chunk;
    this.continuation = continuation;
  }
}

export class ContentDisposition extends $CustomType {
  constructor(x0, parameters) {
    super();
    this[0] = x0;
    this.parameters = parameters;
  }
}

function is_valid_tchar(ch) {
  if (ch === 33) {
    return true;
  } else if (ch === 35) {
    return true;
  } else if (ch === 36) {
    return true;
  } else if (ch === 37) {
    return true;
  } else if (ch === 38) {
    return true;
  } else if (ch === 39) {
    return true;
  } else if (ch === 42) {
    return true;
  } else if (ch === 43) {
    return true;
  } else if (ch === 45) {
    return true;
  } else if (ch === 46) {
    return true;
  } else if (ch === 94) {
    return true;
  } else if (ch === 95) {
    return true;
  } else if (ch === 96) {
    return true;
  } else if (ch === 124) {
    return true;
  } else if (ch === 126) {
    return true;
  } else if ((ch >= 0x30) && (ch <= 0x39)) {
    let ch$1 = ch;
    return true;
  } else if (((ch >= 0x41) && (ch <= 0x5A)) || ((ch >= 0x61) && (ch <= 0x7A))) {
    let ch$1 = ch;
    return true;
  } else {
    return false;
  }
}

function do_is_valid_token(loop$bytes, loop$acc) {
  while (true) {
    let bytes = loop$bytes;
    let acc = loop$acc;
    if ((bytes.bitSize >= 8 && (bytes.bitSize - 8) % 8 === 0) && acc) {
      let char = bytes.byteAt(0);
      let rest = bitArraySlice(bytes, 8);
      loop$bytes = rest;
      loop$acc = is_valid_tchar(char);
    } else {
      return acc;
    }
  }
}

function is_valid_token(s) {
  let _pipe = $bit_array.from_string(s);
  return do_is_valid_token(_pipe, true);
}

export function parse_method(s) {
  if (s === "CONNECT") {
    return new Ok(new Connect());
  } else if (s === "DELETE") {
    return new Ok(new Delete());
  } else if (s === "GET") {
    return new Ok(new Get());
  } else if (s === "HEAD") {
    return new Ok(new Head());
  } else if (s === "OPTIONS") {
    return new Ok(new Options());
  } else if (s === "PATCH") {
    return new Ok(new Patch());
  } else if (s === "POST") {
    return new Ok(new Post());
  } else if (s === "PUT") {
    return new Ok(new Put());
  } else if (s === "TRACE") {
    return new Ok(new Trace());
  } else {
    let s$1 = s;
    let $ = is_valid_token(s$1);
    if ($) {
      return new Ok(new Other(s$1));
    } else {
      return new Error(undefined);
    }
  }
}

export function method_to_string(method) {
  if (method instanceof Connect) {
    return "CONNECT";
  } else if (method instanceof Delete) {
    return "DELETE";
  } else if (method instanceof Get) {
    return "GET";
  } else if (method instanceof Head) {
    return "HEAD";
  } else if (method instanceof Options) {
    return "OPTIONS";
  } else if (method instanceof Patch) {
    return "PATCH";
  } else if (method instanceof Post) {
    return "POST";
  } else if (method instanceof Put) {
    return "PUT";
  } else if (method instanceof Trace) {
    return "TRACE";
  } else {
    let s = method[0];
    return s;
  }
}

export function scheme_to_string(scheme) {
  if (scheme instanceof Http) {
    return "http";
  } else {
    return "https";
  }
}

export function scheme_from_string(scheme) {
  let $ = $string.lowercase(scheme);
  if ($ === "http") {
    return new Ok(new Http());
  } else if ($ === "https") {
    return new Ok(new Https());
  } else {
    return new Error(undefined);
  }
}

function skip_whitespace(loop$data) {
  while (true) {
    let data = loop$data;
    if (data.byteAt(0) === 32 &&
    (data.bitSize >= 8 && (data.bitSize - 8) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 8);
      loop$data = data$1;
    } else if (data.byteAt(0) === 9 &&
    (data.bitSize >= 8 && (data.bitSize - 8) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 8);
      loop$data = data$1;
    } else {
      return data;
    }
  }
}

function more_please_headers(continuation, existing) {
  return new Ok(
    new MoreRequiredForHeaders(
      (more) => {
        return $bool.guard(
          isEqual(more, toBitArray([])),
          new Error(undefined),
          () => { return continuation(toBitArray([existing, more])); },
        );
      },
    ),
  );
}

function parse_rfc_2045_parameter_quoted_value(
  loop$header,
  loop$name,
  loop$value
) {
  while (true) {
    let header = loop$header;
    let name = loop$name;
    let value = loop$value;
    let $ = $string.pop_grapheme(header);
    if (!$.isOk() && !$[0]) {
      return new Error(undefined);
    } else if ($.isOk() && $[0][0] === "\"") {
      let rest = $[0][1];
      return new Ok([[name, value], rest]);
    } else if ($.isOk() && $[0][0] === "\\") {
      let rest = $[0][1];
      return $result.try$(
        $string.pop_grapheme(rest),
        (_use0) => {
          let grapheme = _use0[0];
          let rest$1 = _use0[1];
          return parse_rfc_2045_parameter_quoted_value(
            rest$1,
            name,
            value + grapheme,
          );
        },
      );
    } else {
      let grapheme = $[0][0];
      let rest = $[0][1];
      loop$header = rest;
      loop$name = name;
      loop$value = value + grapheme;
    }
  }
}

function parse_rfc_2045_parameter_unquoted_value(
  loop$header,
  loop$name,
  loop$value
) {
  while (true) {
    let header = loop$header;
    let name = loop$name;
    let value = loop$value;
    let $ = $string.pop_grapheme(header);
    if (!$.isOk() && !$[0]) {
      return [[name, value], header];
    } else if ($.isOk() && $[0][0] === ";") {
      let rest = $[0][1];
      return [[name, value], rest];
    } else if ($.isOk() && $[0][0] === " ") {
      let rest = $[0][1];
      return [[name, value], rest];
    } else if ($.isOk() && $[0][0] === "\t") {
      let rest = $[0][1];
      return [[name, value], rest];
    } else {
      let grapheme = $[0][0];
      let rest = $[0][1];
      loop$header = rest;
      loop$name = name;
      loop$value = value + grapheme;
    }
  }
}

function parse_rfc_2045_parameter_value(header, name) {
  let $ = $string.pop_grapheme(header);
  if (!$.isOk() && !$[0]) {
    return new Error(undefined);
  } else if ($.isOk() && $[0][0] === "\"") {
    let rest = $[0][1];
    return parse_rfc_2045_parameter_quoted_value(rest, name, "");
  } else {
    let grapheme = $[0][0];
    let rest = $[0][1];
    return new Ok(parse_rfc_2045_parameter_unquoted_value(rest, name, grapheme));
  }
}

function parse_rfc_2045_parameter(header, name) {
  return $result.try$(
    $string.pop_grapheme(header),
    (_use0) => {
      let grapheme = _use0[0];
      let rest = _use0[1];
      if (grapheme === "=") {
        return parse_rfc_2045_parameter_value(rest, name);
      } else {
        return parse_rfc_2045_parameter(
          rest,
          name + $string.lowercase(grapheme),
        );
      }
    },
  );
}

function parse_rfc_2045_parameters(loop$header, loop$parameters) {
  while (true) {
    let header = loop$header;
    let parameters = loop$parameters;
    let $ = $string.pop_grapheme(header);
    if (!$.isOk() && !$[0]) {
      return new Ok($list.reverse(parameters));
    } else if ($.isOk() && $[0][0] === ";") {
      let rest = $[0][1];
      loop$header = rest;
      loop$parameters = parameters;
    } else if ($.isOk() && $[0][0] === " ") {
      let rest = $[0][1];
      loop$header = rest;
      loop$parameters = parameters;
    } else if ($.isOk() && $[0][0] === "\t") {
      let rest = $[0][1];
      loop$header = rest;
      loop$parameters = parameters;
    } else {
      let grapheme = $[0][0];
      let rest = $[0][1];
      let acc = $string.lowercase(grapheme);
      return $result.try$(
        parse_rfc_2045_parameter(rest, acc),
        (_use0) => {
          let parameter = _use0[0];
          let rest$1 = _use0[1];
          return parse_rfc_2045_parameters(
            rest$1,
            listPrepend(parameter, parameters),
          );
        },
      );
    }
  }
}

function parse_content_disposition_type(loop$header, loop$name) {
  while (true) {
    let header = loop$header;
    let name = loop$name;
    let $ = $string.pop_grapheme(header);
    if (!$.isOk() && !$[0]) {
      return new Ok(new ContentDisposition(name, toList([])));
    } else if ($.isOk() && $[0][0] === " ") {
      let rest = $[0][1];
      let result = parse_rfc_2045_parameters(rest, toList([]));
      return $result.map(
        result,
        (parameters) => { return new ContentDisposition(name, parameters); },
      );
    } else if ($.isOk() && $[0][0] === "\t") {
      let rest = $[0][1];
      let result = parse_rfc_2045_parameters(rest, toList([]));
      return $result.map(
        result,
        (parameters) => { return new ContentDisposition(name, parameters); },
      );
    } else if ($.isOk() && $[0][0] === ";") {
      let rest = $[0][1];
      let result = parse_rfc_2045_parameters(rest, toList([]));
      return $result.map(
        result,
        (parameters) => { return new ContentDisposition(name, parameters); },
      );
    } else {
      let grapheme = $[0][0];
      let rest = $[0][1];
      loop$header = rest;
      loop$name = name + $string.lowercase(grapheme);
    }
  }
}

export function parse_content_disposition(header) {
  return parse_content_disposition_type(header, "");
}

function more_please_body(continuation, chunk, existing) {
  let _pipe = (more) => {
    return $bool.guard(
      isEqual(more, toBitArray([])),
      new Error(undefined),
      () => { return continuation(toBitArray([existing, more])); },
    );
  };
  let _pipe$1 = ((_capture) => {
    return new MoreRequiredForBody(chunk, _capture);
  })(_pipe);
  return new Ok(_pipe$1);
}

function parse_body_loop(loop$data, loop$boundary, loop$body) {
  while (true) {
    let data = loop$data;
    let boundary = loop$boundary;
    let body = loop$body;
    let dsize = $bit_array.byte_size(data);
    let bsize = $bit_array.byte_size(boundary);
    let required = 6 + bsize;
    if (dsize < required) {
      return more_please_body(
        (_capture) => {
          return parse_body_loop(_capture, boundary, toBitArray([]));
        },
        body,
        data,
      );
    } else if (data.byteAt(0) === 13 &&
    data.byteAt(1) === 10 &&
    (data.bitSize >= 16 && (data.bitSize - 16) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 16);
      let desired = toBitArray([45, 45, boundary]);
      let size = $bit_array.byte_size(desired);
      let dsize$1 = $bit_array.byte_size(data$1);
      let prefix = $bit_array.slice(data$1, 0, size);
      let rest = $bit_array.slice(data$1, size, dsize$1 - size);
      let $ = isEqual(prefix, new Ok(desired));
      if ($ &&
      rest.isOk() &&
      rest[0].byteAt(0) === 13 &&
      rest[0].byteAt(1) === 10 &&
      (rest[0].bitSize >= 16 && (rest[0].bitSize - 16) % 8 === 0)) {
        return new Ok(new MultipartBody(body, false, data$1));
      } else if ($ &&
      rest.isOk() &&
      rest[0].byteAt(0) === 45 &&
      rest[0].byteAt(1) === 45 &&
      (rest[0].bitSize >= 16 && (rest[0].bitSize - 16) % 8 === 0)) {
        let data$2 = bitArraySlice(rest[0], 16);
        return new Ok(new MultipartBody(body, true, data$2));
      } else if (!$) {
        loop$data = data$1;
        loop$boundary = boundary;
        loop$body = toBitArray([body, 13, 10]);
      } else {
        return new Error(undefined);
      }
    } else if ((data.bitSize >= 8 && (data.bitSize - 8) % 8 === 0)) {
      let char = data.byteAt(0);
      let data$1 = bitArraySlice(data, 8);
      loop$data = data$1;
      loop$boundary = boundary;
      loop$body = toBitArray([body, char]);
    } else {
      throw makeError(
        "panic",
        "gleam/http",
        296,
        "parse_body_loop",
        "unreachable",
        {}
      )
    }
  }
}

function parse_body_with_bit_array(data, boundary) {
  let bsize = $bit_array.byte_size(boundary);
  let prefix = $bit_array.slice(data, 0, 2 + bsize);
  let $ = isEqual(prefix, new Ok(toBitArray([45, 45, boundary])));
  if ($) {
    return new Ok(new MultipartBody(toBitArray([]), false, data));
  } else {
    return parse_body_loop(data, boundary, toBitArray([]));
  }
}

export function parse_multipart_body(data, boundary) {
  let _pipe = boundary;
  let _pipe$1 = $bit_array.from_string(_pipe);
  return ((_capture) => { return parse_body_with_bit_array(data, _capture); })(
    _pipe$1,
  );
}

function parse_header_value(loop$data, loop$headers, loop$name, loop$value) {
  while (true) {
    let data = loop$data;
    let headers = loop$headers;
    let name = loop$name;
    let value = loop$value;
    let size = $bit_array.byte_size(data);
    if (size < 4) {
      let _pipe = (data) => {
        let _pipe = data;
        let _pipe$1 = skip_whitespace(_pipe);
        return parse_header_value(_pipe$1, headers, name, value);
      };
      return more_please_headers(_pipe, data);
    } else if (data.byteAt(0) === 13 &&
    data.byteAt(1) === 10 &&
    data.byteAt(2) === 13 &&
    data.byteAt(3) === 10 &&
    (data.bitSize >= 32 && (data.bitSize - 32) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 32);
      return $result.try$(
        $bit_array.to_string(name),
        (name) => {
          return $result.map(
            $bit_array.to_string(value),
            (value) => {
              let headers$1 = $list.reverse(
                listPrepend([$string.lowercase(name), value], headers),
              );
              return new MultipartHeaders(headers$1, data$1);
            },
          );
        },
      );
    } else if (data.byteAt(0) === 13 &&
    data.byteAt(1) === 10 &&
    data.byteAt(2) === 32 &&
    (data.bitSize >= 24 && (data.bitSize - 24) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 24);
      loop$data = data$1;
      loop$headers = headers;
      loop$name = name;
      loop$value = value;
    } else if (data.byteAt(0) === 13 &&
    data.byteAt(1) === 10 &&
    data.byteAt(2) === 9 &&
    (data.bitSize >= 24 && (data.bitSize - 24) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 24);
      loop$data = data$1;
      loop$headers = headers;
      loop$name = name;
      loop$value = value;
    } else if (data.byteAt(0) === 13 &&
    data.byteAt(1) === 10 &&
    (data.bitSize >= 16 && (data.bitSize - 16) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 16);
      return $result.try$(
        $bit_array.to_string(name),
        (name) => {
          return $result.try$(
            $bit_array.to_string(value),
            (value) => {
              let headers$1 = listPrepend(
                [$string.lowercase(name), value],
                headers,
              );
              return parse_header_name(data$1, headers$1, toBitArray([]));
            },
          );
        },
      );
    } else if ((data.bitSize >= 8 && (data.bitSize - 8) % 8 === 0)) {
      let char = data.byteAt(0);
      let rest = bitArraySlice(data, 8);
      let value$1 = toBitArray([value, char]);
      loop$data = rest;
      loop$headers = headers;
      loop$name = name;
      loop$value = value$1;
    } else {
      return new Error(undefined);
    }
  }
}

function parse_header_name(loop$data, loop$headers, loop$name) {
  while (true) {
    let data = loop$data;
    let headers = loop$headers;
    let name = loop$name;
    let $ = skip_whitespace(data);
    if ($.byteAt(0) === 58 && ($.bitSize >= 8 && ($.bitSize - 8) % 8 === 0)) {
      let data$1 = bitArraySlice($, 8);
      let _pipe = data$1;
      let _pipe$1 = skip_whitespace(_pipe);
      return parse_header_value(_pipe$1, headers, name, toBitArray([]));
    } else if (($.bitSize >= 8 && ($.bitSize - 8) % 8 === 0)) {
      let char = $.byteAt(0);
      let data$1 = bitArraySlice($, 8);
      loop$data = data$1;
      loop$headers = headers;
      loop$name = toBitArray([name, char]);
    } else {
      return more_please_headers(
        (_capture) => { return parse_header_name(_capture, headers, name); },
        data,
      );
    }
  }
}

function do_parse_headers(data) {
  if (data.byteAt(0) === 13 &&
  data.byteAt(1) === 10 &&
  data.byteAt(2) === 13 &&
  data.byteAt(3) === 10 &&
  (data.bitSize >= 32 && (data.bitSize - 32) % 8 === 0)) {
    let data$1 = bitArraySlice(data, 32);
    return new Ok(new MultipartHeaders(toList([]), data$1));
  } else if (data.byteAt(0) === 13 &&
  data.byteAt(1) === 10 &&
  (data.bitSize >= 16 && (data.bitSize - 16) % 8 === 0)) {
    let data$1 = bitArraySlice(data, 16);
    return parse_header_name(data$1, toList([]), toBitArray([]));
  } else if (data.byteAt(0) === 13 && data.bitSize == 8) {
    return more_please_headers(do_parse_headers, data);
  } else if (data.bitSize == 0) {
    return more_please_headers(do_parse_headers, data);
  } else {
    return new Error(undefined);
  }
}

function parse_headers_after_prelude(data, boundary) {
  let dsize = $bit_array.byte_size(data);
  let bsize = $bit_array.byte_size(boundary);
  let required_size = bsize + 4;
  return $bool.guard(
    dsize < required_size,
    more_please_headers(
      (_capture) => { return parse_headers_after_prelude(_capture, boundary); },
      data,
    ),
    () => {
      return $result.try$(
        $bit_array.slice(data, 0, required_size - 2),
        (prefix) => {
          return $result.try$(
            $bit_array.slice(data, 2 + bsize, 2),
            (second) => {
              let desired = toBitArray([45, 45, boundary]);
              return $bool.guard(
                !isEqual(prefix, desired),
                new Error(undefined),
                () => {
                  let $ = isEqual(second, toBitArray([45, 45]));
                  if ($) {
                    let rest_size = dsize - required_size;
                    return $result.map(
                      $bit_array.slice(data, required_size, rest_size),
                      (data) => {
                        return new MultipartHeaders(toList([]), data);
                      },
                    );
                  } else {
                    let start = required_size - 2;
                    let rest_size = (dsize - required_size) + 2;
                    return $result.try$(
                      $bit_array.slice(data, start, rest_size),
                      (data) => { return do_parse_headers(data); },
                    );
                  }
                },
              );
            },
          );
        },
      );
    },
  );
}

function skip_preamble(loop$data, loop$boundary) {
  while (true) {
    let data = loop$data;
    let boundary = loop$boundary;
    let data_size = $bit_array.byte_size(data);
    let boundary_size = $bit_array.byte_size(boundary);
    let required = boundary_size + 4;
    if (data_size < required) {
      return more_please_headers(
        (_capture) => { return skip_preamble(_capture, boundary); },
        data,
      );
    } else if (data.byteAt(0) === 13 &&
    data.byteAt(1) === 10 &&
    data.byteAt(2) === 45 &&
    data.byteAt(3) === 45 &&
    (data.bitSize >= 32 && (data.bitSize - 32) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 32);
      let $ = $bit_array.slice(data$1, 0, boundary_size);
      if ($.isOk() && (isEqual($[0], boundary))) {
        let prefix = $[0];
        let start = boundary_size;
        let length = $bit_array.byte_size(data$1) - boundary_size;
        return $result.try$(
          $bit_array.slice(data$1, start, length),
          (rest) => { return do_parse_headers(rest); },
        );
      } else if ($.isOk()) {
        loop$data = data$1;
        loop$boundary = boundary;
      } else {
        return new Error(undefined);
      }
    } else if ((data.bitSize >= 8 && (data.bitSize - 8) % 8 === 0)) {
      let data$1 = bitArraySlice(data, 8);
      loop$data = data$1;
      loop$boundary = boundary;
    } else {
      throw makeError(
        "panic",
        "gleam/http",
        372,
        "skip_preamble",
        "unreachable",
        {}
      )
    }
  }
}

export function parse_multipart_headers(data, boundary) {
  let boundary$1 = $bit_array.from_string(boundary);
  let prefix = toBitArray([45, 45, boundary$1]);
  let $ = isEqual(
    $bit_array.slice(data, 0, $bit_array.byte_size(prefix)),
    new Ok(prefix)
  );
  if ($) {
    return parse_headers_after_prelude(data, boundary$1);
  } else {
    return skip_preamble(data, boundary$1);
  }
}
