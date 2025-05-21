import * as $erlang from "../../../gleam_erlang/gleam/erlang.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import * as $bytes_tree from "../../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $socket from "../../../glisten/glisten/socket.mjs";
import * as $transport from "../../../glisten/glisten/transport.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $http from "../../mist/internal/http.mjs";
import * as $frame from "../../mist/internal/http2/frame.mjs";
import { Complete, Data, Header } from "../../mist/internal/http2/frame.mjs";

export class Http2Settings extends $CustomType {
  constructor(header_table_size, server_push, max_concurrent_streams, initial_window_size, max_frame_size, max_header_list_size) {
    super();
    this.header_table_size = header_table_size;
    this.server_push = server_push;
    this.max_concurrent_streams = max_concurrent_streams;
    this.initial_window_size = initial_window_size;
    this.max_frame_size = max_frame_size;
    this.max_header_list_size = max_header_list_size;
  }
}

export class Compression extends $CustomType {}

export class BadHeaderPacket extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function default_settings() {
  return new Http2Settings(
    4096,
    new $frame.Disabled(),
    100,
    65_535,
    16_384,
    new None(),
  );
}

export function update_settings(current, settings) {
  return $list.fold(
    settings,
    current,
    (settings, setting) => {
      if (setting instanceof $frame.HeaderTableSize) {
        let size = setting[0];
        let _record = settings;
        return new Http2Settings(
          size,
          _record.server_push,
          _record.max_concurrent_streams,
          _record.initial_window_size,
          _record.max_frame_size,
          _record.max_header_list_size,
        );
      } else if (setting instanceof $frame.ServerPush) {
        let push = setting[0];
        let _record = settings;
        return new Http2Settings(
          _record.header_table_size,
          push,
          _record.max_concurrent_streams,
          _record.initial_window_size,
          _record.max_frame_size,
          _record.max_header_list_size,
        );
      } else if (setting instanceof $frame.MaxConcurrentStreams) {
        let max = setting[0];
        let _record = settings;
        return new Http2Settings(
          _record.header_table_size,
          _record.server_push,
          max,
          _record.initial_window_size,
          _record.max_frame_size,
          _record.max_header_list_size,
        );
      } else if (setting instanceof $frame.InitialWindowSize) {
        let size = setting[0];
        let _record = settings;
        return new Http2Settings(
          _record.header_table_size,
          _record.server_push,
          _record.max_concurrent_streams,
          size,
          _record.max_frame_size,
          _record.max_header_list_size,
        );
      } else if (setting instanceof $frame.MaxFrameSize) {
        let size = setting[0];
        let _record = settings;
        return new Http2Settings(
          _record.header_table_size,
          _record.server_push,
          _record.max_concurrent_streams,
          _record.initial_window_size,
          size,
          _record.max_header_list_size,
        );
      } else {
        let size = setting[0];
        let _record = settings;
        return new Http2Settings(
          _record.header_table_size,
          _record.server_push,
          _record.max_concurrent_streams,
          _record.initial_window_size,
          _record.max_frame_size,
          new Some(size),
        );
      }
    },
  );
}
