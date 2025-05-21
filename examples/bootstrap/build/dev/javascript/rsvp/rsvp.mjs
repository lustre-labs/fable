import * as $fetch from "../gleam_fetch/gleam/fetch.mjs";
import * as $http from "../gleam_http/gleam/http.mjs";
import * as $request from "../gleam_http/gleam/http/request.mjs";
import * as $response from "../gleam_http/gleam/http/response.mjs";
import * as $promise from "../gleam_javascript/gleam/javascript/promise.mjs";
import * as $json from "../gleam_json/gleam/json.mjs";
import * as $decode from "../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import * as $lustre_simulate from "../lustre/lustre/dev/simulate.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import { Ok, Error, CustomType as $CustomType } from "./gleam.mjs";
import { from_relative_url as parse_relative_uri } from "./rsvp.ffi.mjs";

export { parse_relative_uri };

export class BadBody extends $CustomType {}

export class BadUrl extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class HttpError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class JsonError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NetworkError extends $CustomType {}

export class UnhandledResponse extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Handler extends $CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
}

export function expect_ok_response(handler) {
  return new Handler(
    (result) => {
      return handler(
        $result.try$(
          result,
          (response) => {
            let $ = response.status;
            if (($ >= 200) && ($ < 300)) {
              let code = $;
              return new Ok(response);
            } else if (($ >= 400) && ($ < 600)) {
              let code = $;
              return new Error(new HttpError(response));
            } else {
              return new Error(new UnhandledResponse(response));
            }
          },
        ),
      );
    },
  );
}

function expect_json_response(handler) {
  return expect_ok_response(
    (result) => {
      return handler(
        $result.try$(
          result,
          (response) => {
            let $ = $response.get_header(response, "content-type");
            if ($.isOk() && $[0] === "application/json") {
              return new Ok(response);
            } else if ($.isOk() && $[0].startsWith("application/json;")) {
              return new Ok(response);
            } else {
              return new Error(new UnhandledResponse(response));
            }
          },
        ),
      );
    },
  );
}

function expect_text_response(handler) {
  return expect_ok_response(
    (result) => {
      return handler(
        $result.try$(
          result,
          (response) => {
            let $ = $response.get_header(response, "content-type");
            if ($.isOk() && $[0].startsWith("text/")) {
              return new Ok(response);
            } else {
              return new Error(new UnhandledResponse(response));
            }
          },
        ),
      );
    },
  );
}

export function expect_text(handler) {
  return expect_text_response(
    (result) => {
      let _pipe = result;
      let _pipe$1 = $result.map(_pipe, (response) => { return response.body; });
      return handler(_pipe$1);
    },
  );
}

export function expect_any_response(handler) {
  return new Handler(handler);
}

function do_send(request, handler) {
  return $effect.from(
    (dispatch) => {
      let _pipe = $fetch.send(request);
      let _pipe$1 = $promise.try_await(_pipe, $fetch.read_text_body);
      let _pipe$2 = $promise.map(
        _pipe$1,
        (_capture) => {
          return $result.map_error(
            _capture,
            (error) => {
              if (error instanceof $fetch.NetworkError) {
                return new NetworkError();
              } else if (error instanceof $fetch.UnableToReadBody) {
                return new BadBody();
              } else {
                return new BadBody();
              }
            },
          );
        },
      );
      let _pipe$3 = $promise.map(_pipe$2, handler.run);
      $promise.tap(_pipe$3, dispatch)
      return undefined;
    },
  );
}

export function send(request, handler) {
  return do_send(request, handler);
}

export function simulate(simulation, response, handler) {
  return $lustre_simulate.message(simulation, handler.run(new Ok(response)));
}

function reject(err, handler) {
  return $effect.from(
    (dispatch) => {
      let _pipe = new Error(err);
      let _pipe$1 = handler.run(_pipe);
      return dispatch(_pipe$1);
    },
  );
}

function decode_json_body(response, decoder) {
  let _pipe = response.body;
  let _pipe$1 = $json.parse(_pipe, decoder);
  return $result.map_error(_pipe$1, (var0) => { return new JsonError(var0); });
}

export function expect_json(decoder, handler) {
  return expect_json_response(
    (result) => {
      let _pipe = result;
      let _pipe$1 = $result.then$(
        _pipe,
        (_capture) => { return decode_json_body(_capture, decoder); },
      );
      return handler(_pipe$1);
    },
  );
}

function to_uri(uri_string) {
  let _block;
  if (uri_string.startsWith("./")) {
    _block = parse_relative_uri(uri_string);
  } else if (uri_string.startsWith("/")) {
    _block = parse_relative_uri(uri_string);
  } else {
    _block = $uri.parse(uri_string);
  }
  let _pipe = _block;
  return $result.replace_error(_pipe, new BadUrl(uri_string));
}

export function get(url, handler) {
  let $ = to_uri(url);
  if ($.isOk()) {
    let uri = $[0];
    let _pipe = $request.from_uri(uri);
    let _pipe$1 = $result.map(
      _pipe,
      (_capture) => { return send(_capture, handler); },
    );
    let _pipe$2 = $result.map_error(
      _pipe$1,
      (_) => { return reject(new BadUrl(url), handler); },
    );
    return $result.unwrap_both(_pipe$2);
  } else {
    let err = $[0];
    return reject(err, handler);
  }
}

export function post(url, body, handler) {
  let $ = to_uri(url);
  if ($.isOk()) {
    let uri = $[0];
    let _pipe = $request.from_uri(uri);
    let _pipe$1 = $result.map(
      _pipe,
      (request) => {
        let _pipe$1 = request;
        let _pipe$2 = $request.set_method(_pipe$1, new $http.Post());
        let _pipe$3 = $request.set_header(
          _pipe$2,
          "content-type",
          "application/json",
        );
        let _pipe$4 = $request.set_body(_pipe$3, $json.to_string(body));
        return send(_pipe$4, handler);
      },
    );
    let _pipe$2 = $result.map_error(
      _pipe$1,
      (_) => { return reject(new BadUrl(url), handler); },
    );
    return $result.unwrap_both(_pipe$2);
  } else {
    let err = $[0];
    return reject(err, handler);
  }
}
