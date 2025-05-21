import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../../../gleam_stdlib/gleam/uri.mjs";
import { Uri } from "../../../gleam_stdlib/gleam/uri.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";
import { Get } from "../../gleam/http.mjs";
import * as $cookie from "../../gleam/http/cookie.mjs";

export class Request extends $CustomType {
  constructor(method, headers, body, scheme, host, port, path, query) {
    super();
    this.method = method;
    this.headers = headers;
    this.body = body;
    this.scheme = scheme;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
  }
}

export function to_uri(request) {
  return new Uri(
    new $option.Some($http.scheme_to_string(request.scheme)),
    new $option.None(),
    new $option.Some(request.host),
    request.port,
    request.path,
    request.query,
    new $option.None(),
  );
}

export function from_uri(uri) {
  return $result.then$(
    (() => {
      let _pipe = uri.scheme;
      let _pipe$1 = $option.unwrap(_pipe, "");
      return $http.scheme_from_string(_pipe$1);
    })(),
    (scheme) => {
      return $result.then$(
        (() => {
          let _pipe = uri.host;
          return $option.to_result(_pipe, undefined);
        })(),
        (host) => {
          let req = new Request(
            new Get(),
            toList([]),
            "",
            scheme,
            host,
            uri.port,
            uri.path,
            uri.query,
          );
          return new Ok(req);
        },
      );
    },
  );
}

export function get_header(request, key) {
  return $list.key_find(request.headers, $string.lowercase(key));
}

export function set_header(request, key, value) {
  let headers = $list.key_set(request.headers, $string.lowercase(key), value);
  let _record = request;
  return new Request(
    _record.method,
    headers,
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    _record.path,
    _record.query,
  );
}

export function prepend_header(request, key, value) {
  let headers = listPrepend([$string.lowercase(key), value], request.headers);
  let _record = request;
  return new Request(
    _record.method,
    headers,
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    _record.path,
    _record.query,
  );
}

export function set_body(req, body) {
  let method = req.method;
  let headers = req.headers;
  let scheme = req.scheme;
  let host = req.host;
  let port = req.port;
  let path = req.path;
  let query = req.query;
  return new Request(method, headers, body, scheme, host, port, path, query);
}

export function map(request, transform) {
  let _pipe = request.body;
  let _pipe$1 = transform(_pipe);
  return ((_capture) => { return set_body(request, _capture); })(_pipe$1);
}

export function path_segments(request) {
  let _pipe = request.path;
  return $uri.path_segments(_pipe);
}

export function get_query(request) {
  let $ = request.query;
  if ($ instanceof $option.Some) {
    let query_string = $[0];
    return $uri.parse_query(query_string);
  } else {
    return new Ok(toList([]));
  }
}

export function set_query(req, query) {
  let pair = (t) => {
    return ($uri.percent_encode(t[0]) + "=") + $uri.percent_encode(t[1]);
  };
  let _block;
  let _pipe = query;
  let _pipe$1 = $list.map(_pipe, pair);
  let _pipe$2 = $list.intersperse(_pipe$1, "&");
  let _pipe$3 = $string.concat(_pipe$2);
  _block = new $option.Some(_pipe$3);
  let query$1 = _block;
  let _record = req;
  return new Request(
    _record.method,
    _record.headers,
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    _record.path,
    query$1,
  );
}

export function set_method(req, method) {
  let _record = req;
  return new Request(
    method,
    _record.headers,
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    _record.path,
    _record.query,
  );
}

export function new$() {
  return new Request(
    new Get(),
    toList([]),
    "",
    new $http.Https(),
    "localhost",
    new $option.None(),
    "",
    new $option.None(),
  );
}

export function to(url) {
  let _pipe = url;
  let _pipe$1 = $uri.parse(_pipe);
  return $result.then$(_pipe$1, from_uri);
}

export function set_scheme(req, scheme) {
  let _record = req;
  return new Request(
    _record.method,
    _record.headers,
    _record.body,
    scheme,
    _record.host,
    _record.port,
    _record.path,
    _record.query,
  );
}

export function set_host(req, host) {
  let _record = req;
  return new Request(
    _record.method,
    _record.headers,
    _record.body,
    _record.scheme,
    host,
    _record.port,
    _record.path,
    _record.query,
  );
}

export function set_port(req, port) {
  let _record = req;
  return new Request(
    _record.method,
    _record.headers,
    _record.body,
    _record.scheme,
    _record.host,
    new $option.Some(port),
    _record.path,
    _record.query,
  );
}

export function set_path(req, path) {
  let _record = req;
  return new Request(
    _record.method,
    _record.headers,
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    path,
    _record.query,
  );
}

export function set_cookie(req, name, value) {
  let new_cookie_string = $string.join(toList([name, value]), "=");
  let _block;
  let $1 = $list.key_pop(req.headers, "cookie");
  if ($1.isOk()) {
    let cookies_string = $1[0][0];
    let headers = $1[0][1];
    let cookies_string$1 = $string.join(
      toList([cookies_string, new_cookie_string]),
      "; ",
    );
    _block = [cookies_string$1, headers];
  } else {
    _block = [new_cookie_string, req.headers];
  }
  let $ = _block;
  let cookies_string = $[0];
  let headers = $[1];
  let _record = req;
  return new Request(
    _record.method,
    listPrepend(["cookie", cookies_string], headers),
    _record.body,
    _record.scheme,
    _record.host,
    _record.port,
    _record.path,
    _record.query,
  );
}

export function get_cookies(req) {
  let headers = req.headers;
  let _pipe = headers;
  let _pipe$1 = $list.filter_map(
    _pipe,
    (header) => {
      let name = header[0];
      let value = header[1];
      if (name === "cookie") {
        return new Ok($cookie.parse(value));
      } else {
        return new Error(undefined);
      }
    },
  );
  return $list.flatten(_pipe$1);
}

export function remove_cookie(req, name) {
  let $ = $list.key_pop(req.headers, "cookie");
  if ($.isOk()) {
    let cookies_string = $[0][0];
    let headers = $[0][1];
    let _block;
    let _pipe = $string.split(cookies_string, ";");
    let _pipe$1 = $list.filter(
      _pipe,
      (str) => {
        let _pipe$1 = $string.trim(str);
        let _pipe$2 = $string.split_once(_pipe$1, "=");
        let _pipe$3 = $result.map(_pipe$2, (tup) => { return tup[0] !== name; });
        return $result.unwrap(_pipe$3, true);
      },
    );
    _block = $string.join(_pipe$1, ";");
    let new_cookies_string = _block;
    let _record = req;
    return new Request(
      _record.method,
      listPrepend(["cookie", new_cookies_string], headers),
      _record.body,
      _record.scheme,
      _record.host,
      _record.port,
      _record.path,
      _record.query,
    );
  } else {
    return req;
  }
}
