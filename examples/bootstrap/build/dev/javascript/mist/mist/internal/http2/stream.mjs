import * as $erlang from "../../../../gleam_erlang/gleam/erlang.mjs";
import * as $process from "../../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $ghttp from "../../../../gleam_http/gleam/http.mjs";
import * as $request from "../../../../gleam_http/gleam/http/request.mjs";
import { Request } from "../../../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../../../gleam_http/gleam/http/response.mjs";
import * as $actor from "../../../../gleam_otp/gleam/otp/actor.mjs";
import * as $function from "../../../../gleam_stdlib/gleam/function.mjs";
import * as $int from "../../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $pair from "../../../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../../../../gleam_stdlib/gleam/uri.mjs";
import { Ok, CustomType as $CustomType } from "../../../gleam.mjs";
import * as $http from "../../../mist/internal/http.mjs";
import { Connection, Stream } from "../../../mist/internal/http.mjs";
import * as $flow_control from "../../../mist/internal/http2/flow_control.mjs";
import * as $frame from "../../../mist/internal/http2/frame.mjs";

export class Ready extends $CustomType {}

export class Data extends $CustomType {
  constructor(bits, end) {
    super();
    this.bits = bits;
    this.end = end;
  }
}

export class Done extends $CustomType {}

export class Open extends $CustomType {}

export class RemoteClosed extends $CustomType {}

export class LocalClosed extends $CustomType {}

export class Closed extends $CustomType {}

export class State extends $CustomType {
  constructor(id, state, subject, receive_window_size, send_window_size, pending_content_length) {
    super();
    this.id = id;
    this.state = state;
    this.subject = subject;
    this.receive_window_size = receive_window_size;
    this.send_window_size = send_window_size;
    this.pending_content_length = pending_content_length;
  }
}

export class InternalState extends $CustomType {
  constructor(data_selector, data_subject, end, pending_response, to_remove) {
    super();
    this.data_selector = data_selector;
    this.data_subject = data_subject;
    this.end = end;
    this.pending_response = pending_response;
    this.to_remove = to_remove;
  }
}

export function make_request(loop$headers, loop$req) {
  while (true) {
    let headers = loop$headers;
    let req = loop$req;
    if (headers.hasLength(0)) {
      return new Ok(req);
    } else if (headers.atLeastLength(1) && headers.head[0] === "method") {
      let method = headers.head[1];
      let rest = headers.tail;
      let _pipe = method;
      let _pipe$1 = $ghttp.parse_method(_pipe);
      let _pipe$2 = $result.replace_error(_pipe$1, undefined);
      let _pipe$3 = $result.map(
        _pipe$2,
        (_capture) => { return $request.set_method(req, _capture); },
      );
      return $result.then$(
        _pipe$3,
        (_capture) => { return make_request(rest, _capture); },
      );
    } else if (headers.atLeastLength(1) && headers.head[0] === "scheme") {
      let scheme = headers.head[1];
      let rest = headers.tail;
      let _pipe = scheme;
      let _pipe$1 = $ghttp.scheme_from_string(_pipe);
      let _pipe$2 = $result.replace_error(_pipe$1, undefined);
      let _pipe$3 = $result.map(
        _pipe$2,
        (_capture) => { return $request.set_scheme(req, _capture); },
      );
      return $result.then$(
        _pipe$3,
        (_capture) => { return make_request(rest, _capture); },
      );
    } else if (headers.atLeastLength(1) && headers.head[0] === "authority") {
      let rest = headers.tail;
      loop$headers = rest;
      loop$req = req;
    } else if (headers.atLeastLength(1) && headers.head[0] === "path") {
      let path = headers.head[1];
      let rest = headers.tail;
      let _pipe = path;
      let _pipe$1 = $string.split_once(_pipe, "?");
      let _pipe$2 = $result.map(
        _pipe$1,
        (split) => {
          return $pair.map_second(
            split,
            (query) => {
              let _pipe$2 = query;
              let _pipe$3 = $uri.parse_query(_pipe$2);
              let _pipe$4 = $result.map(
                _pipe$3,
                (var0) => { return new Some(var0); },
              );
              return $result.unwrap(_pipe$4, new None());
            },
          );
        },
      );
      let _pipe$3 = $result.unwrap(_pipe$2, [path, new None()]);
      return ((tup) => { let _block;
        let $ = tup[1];
        if ($ instanceof Some) {
          let query = $[0];
          let _pipe$4 = req;
          let _pipe$5 = $request.set_path(_pipe$4, tup[0]);
          _block = $request.set_query(_pipe$5, query);
        } else {
          _block = $request.set_path(req, tup[0]);
        }
        let _pipe$4 = _block;
        return ((_capture) => { return make_request(rest, _capture); })(_pipe$4); })(
        _pipe$3,
      );
    } else {
      let key = headers.head[0];
      let value = headers.head[1];
      let rest = headers.tail;
      let _pipe = req;
      let _pipe$1 = $request.set_header(_pipe, key, value);
      return ((_capture) => { return make_request(rest, _capture); })(_pipe$1);
    }
  }
}

export function receive_data(state, size) {
  let $ = $flow_control.compute_receive_window(state.receive_window_size, size);
  let new_window_size = $[0];
  let increment = $[1];
  let _block;
  let _record = state;
  _block = new State(
    _record.id,
    _record.state,
    _record.subject,
    new_window_size,
    _record.send_window_size,
    $option.map(state.pending_content_length, (val) => { return val - size; }),
  );
  let new_state = _block;
  return [new_state, increment];
}
