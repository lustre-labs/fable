import * as $erlang from "../../../../gleam_erlang/gleam/erlang.mjs";
import * as $process from "../../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $response from "../../../../gleam_http/gleam/http/response.mjs";
import * as $bit_array from "../../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dict from "../../../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $logging from "../../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../../gleam.mjs";
import * as $buffer from "../../../mist/internal/buffer.mjs";
import * as $http from "../../../mist/internal/http.mjs";
import { Connection, Initial } from "../../../mist/internal/http.mjs";
import * as $http2 from "../../../mist/internal/http2.mjs";
import { Http2Settings } from "../../../mist/internal/http2.mjs";
import * as $flow_control from "../../../mist/internal/http2/flow_control.mjs";
import * as $frame from "../../../mist/internal/http2/frame.mjs";
import { Complete, Continued, Settings } from "../../../mist/internal/http2/frame.mjs";
import * as $stream from "../../../mist/internal/http2/stream.mjs";
import { Ready } from "../../../mist/internal/http2/stream.mjs";

export class Send extends $CustomType {
  constructor(identifier, resp) {
    super();
    this.identifier = identifier;
    this.resp = resp;
  }
}

export class PendingSend extends $CustomType {}

export class State extends $CustomType {
  constructor(fragment, frame_buffer, pending_sends, receive_hpack_context, self, send_hpack_context, send_window_size, receive_window_size, settings, streams) {
    super();
    this.fragment = fragment;
    this.frame_buffer = frame_buffer;
    this.pending_sends = pending_sends;
    this.receive_hpack_context = receive_hpack_context;
    this.self = self;
    this.send_hpack_context = send_hpack_context;
    this.send_window_size = send_window_size;
    this.receive_window_size = receive_window_size;
    this.settings = settings;
    this.streams = streams;
  }
}

export function send_hpack_context(state, context) {
  let _record = state;
  return new State(
    _record.fragment,
    _record.frame_buffer,
    _record.pending_sends,
    _record.receive_hpack_context,
    _record.self,
    context,
    _record.send_window_size,
    _record.receive_window_size,
    _record.settings,
    _record.streams,
  );
}

export function receive_hpack_context(state, context) {
  let _record = state;
  return new State(
    _record.fragment,
    _record.frame_buffer,
    _record.pending_sends,
    context,
    _record.self,
    _record.send_hpack_context,
    _record.send_window_size,
    _record.receive_window_size,
    _record.settings,
    _record.streams,
  );
}

export function append_data(state, data) {
  let _record = state;
  return new State(
    _record.fragment,
    $buffer.append(state.frame_buffer, data),
    _record.pending_sends,
    _record.receive_hpack_context,
    _record.self,
    _record.send_hpack_context,
    _record.send_window_size,
    _record.receive_window_size,
    _record.settings,
    _record.streams,
  );
}
