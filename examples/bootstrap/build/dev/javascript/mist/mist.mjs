import * as $erlang from "../gleam_erlang/gleam/erlang.mjs";
import { rescue } from "../gleam_erlang/gleam/erlang.mjs";
import * as $process from "../gleam_erlang/gleam/erlang/process.mjs";
import * as $gleam_http from "../gleam_http/gleam/http.mjs";
import { Http, Https } from "../gleam_http/gleam/http.mjs";
import * as $request from "../gleam_http/gleam/http/request.mjs";
import * as $response from "../gleam_http/gleam/http/response.mjs";
import * as $actor from "../gleam_otp/gleam/otp/actor.mjs";
import * as $supervisor from "../gleam_otp/gleam/otp/supervisor.mjs";
import * as $bit_array from "../gleam_stdlib/gleam/bit_array.mjs";
import * as $bytes_tree from "../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $function from "../gleam_stdlib/gleam/function.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../gleam_stdlib/gleam/string_tree.mjs";
import * as $yielder from "../gleam_yielder/gleam/yielder.mjs";
import * as $glisten from "../glisten/glisten.mjs";
import * as $transport from "../glisten/glisten/transport.mjs";
import * as $gramps_websocket from "../gramps/gramps/websocket.mjs";
import { BinaryFrame, Data, TextFrame } from "../gramps/gramps/websocket.mjs";
import * as $logging from "../logging/logging.mjs";
import { Ok, Error, CustomType as $CustomType } from "./gleam.mjs";
import * as $buffer from "./mist/internal/buffer.mjs";
import { Buffer } from "./mist/internal/buffer.mjs";
import * as $encoder from "./mist/internal/encoder.mjs";
import * as $file from "./mist/internal/file.mjs";
import * as $handler from "./mist/internal/handler.mjs";
import * as $http from "./mist/internal/http.mjs";
import {
  Bytes as InternalBytes,
  Chunked as InternalChunked,
  File as InternalFile,
  ServerSentEvents as InternalServerSentEvents,
  Websocket as InternalWebsocket,
} from "./mist/internal/http.mjs";
import * as $websocket from "./mist/internal/websocket.mjs";
import { Internal, User } from "./mist/internal/websocket.mjs";

export class IpV4 extends $CustomType {
  constructor(x0, x1, x2, x3) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
  }
}

export class IpV6 extends $CustomType {
  constructor(x0, x1, x2, x3, x4, x5, x6, x7) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
    this[4] = x4;
    this[5] = x5;
    this[6] = x6;
    this[7] = x7;
  }
}

export class ConnectionInfo extends $CustomType {
  constructor(port, ip_address) {
    super();
    this.port = port;
    this.ip_address = ip_address;
  }
}

export class Websocket extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Bytes extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Chunked extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class File extends $CustomType {
  constructor(descriptor, offset, length) {
    super();
    this.descriptor = descriptor;
    this.offset = offset;
    this.length = length;
  }
}

export class ServerSentEvents extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class IsDir extends $CustomType {}

export class NoAccess extends $CustomType {}

export class NoEntry extends $CustomType {}

export class UnknownFileError extends $CustomType {}

export class ExcessBody extends $CustomType {}

export class MalformedBody extends $CustomType {}

export class Chunk extends $CustomType {
  constructor(data, consume) {
    super();
    this.data = data;
    this.consume = consume;
  }
}

export class Done extends $CustomType {}

class ChunkState extends $CustomType {
  constructor(data_buffer, chunk_buffer, done) {
    super();
    this.data_buffer = data_buffer;
    this.chunk_buffer = chunk_buffer;
    this.done = done;
  }
}

class Builder extends $CustomType {
  constructor(port, handler, after_start, interface$, ipv6_support) {
    super();
    this.port = port;
    this.handler = handler;
    this.after_start = after_start;
    this.interface = interface$;
    this.ipv6_support = ipv6_support;
  }
}

export class Assigned extends $CustomType {}

export class Provided extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Server extends $CustomType {
  constructor(supervisor, port, ip_address) {
    super();
    this.supervisor = supervisor;
    this.port = port;
    this.ip_address = ip_address;
  }
}

export class NoCertificate extends $CustomType {}

export class NoKey extends $CustomType {}

export class NoKeyOrCertificate extends $CustomType {}

export class GlistenError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class CertificateError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Binary extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Closed extends $CustomType {}

export class Shutdown extends $CustomType {}

export class Custom extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class SSEConnection extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class SSEEvent extends $CustomType {
  constructor(id, event, retry, data) {
    super();
    this.id = id;
    this.event = event;
    this.retry = retry;
    this.data = data;
  }
}

function to_mist_ip_address(ip) {
  if (ip instanceof $glisten.IpV4) {
    let a = ip[0];
    let b = ip[1];
    let c = ip[2];
    let d = ip[3];
    return new IpV4(a, b, c, d);
  } else {
    let a = ip[0];
    let b = ip[1];
    let c = ip[2];
    let d = ip[3];
    let e = ip[4];
    let f = ip[5];
    let g = ip[6];
    let h = ip[7];
    return new IpV6(a, b, c, d, e, f, g, h);
  }
}

function to_glisten_ip_address(ip) {
  if (ip instanceof IpV4) {
    let a = ip[0];
    let b = ip[1];
    let c = ip[2];
    let d = ip[3];
    return new $glisten.IpV4(a, b, c, d);
  } else {
    let a = ip[0];
    let b = ip[1];
    let c = ip[2];
    let d = ip[3];
    let e = ip[4];
    let f = ip[5];
    let g = ip[6];
    let h = ip[7];
    return new $glisten.IpV6(a, b, c, d, e, f, g, h);
  }
}

export function ip_address_to_string(address) {
  return $glisten.ip_address_to_string(to_glisten_ip_address(address));
}

function convert_file_errors(err) {
  if (err instanceof $file.IsDir) {
    return new IsDir();
  } else if (err instanceof $file.NoAccess) {
    return new NoAccess();
  } else if (err instanceof $file.NoEntry) {
    return new NoEntry();
  } else {
    return new UnknownFileError();
  }
}

export function new$(handler) {
  return new Builder(
    4000,
    handler,
    (port, scheme, interface$) => {
      let _block;
      if (interface$ instanceof IpV6) {
        _block = ("[" + ip_address_to_string(interface$)) + "]";
      } else {
        _block = ip_address_to_string(interface$);
      }
      let address = _block;
      let message = (((("Listening on " + $gleam_http.scheme_to_string(scheme)) + "://") + address) + ":") + $int.to_string(
        port,
      );
      return $io.println(message);
    },
    "localhost",
    false,
  );
}

export function port(builder, port) {
  let _record = builder;
  return new Builder(
    port,
    _record.handler,
    _record.after_start,
    _record.interface,
    _record.ipv6_support,
  );
}

export function after_start(builder, after_start) {
  let _record = builder;
  return new Builder(
    _record.port,
    _record.handler,
    after_start,
    _record.interface,
    _record.ipv6_support,
  );
}

export function bind(builder, interface$) {
  let _record = builder;
  return new Builder(
    _record.port,
    _record.handler,
    _record.after_start,
    interface$,
    _record.ipv6_support,
  );
}

export function with_ipv6(builder) {
  let _record = builder;
  return new Builder(
    _record.port,
    _record.handler,
    _record.after_start,
    _record.interface,
    true,
  );
}

function convert_body_types(resp) {
  let _block;
  let $ = resp.body;
  if ($ instanceof Websocket) {
    let selector = $[0];
    _block = new InternalWebsocket(selector);
  } else if ($ instanceof Bytes) {
    let data = $[0];
    _block = new InternalBytes(data);
  } else if ($ instanceof File) {
    let descriptor = $.descriptor;
    let offset = $.offset;
    let length = $.length;
    _block = new InternalFile(descriptor, offset, length);
  } else if ($ instanceof Chunked) {
    let iter = $[0];
    _block = new InternalChunked(iter);
  } else {
    let selector = $[0];
    _block = new InternalServerSentEvents(selector);
  }
  let new_body = _block;
  return $response.set_body(resp, new_body);
}

export function get_supervisor(server) {
  return server.supervisor;
}

export function get_port(server) {
  return server.port;
}

function internal_to_public_ws_message(msg) {
  if (msg instanceof Internal &&
  msg[0] instanceof Data &&
  msg[0][0] instanceof TextFrame) {
    let data = msg[0][0].payload;
    let _pipe = data;
    let _pipe$1 = $bit_array.to_string(_pipe);
    return $result.map(_pipe$1, (var0) => { return new Text(var0); });
  } else if (msg instanceof Internal &&
  msg[0] instanceof Data &&
  msg[0][0] instanceof BinaryFrame) {
    let data = msg[0][0].payload;
    return new Ok(new Binary(data));
  } else if (msg instanceof User) {
    let msg$1 = msg[0];
    return new Ok(new Custom(msg$1));
  } else {
    return new Error(undefined);
  }
}

export function event(data) {
  return new SSEEvent(new None(), new None(), new None(), data);
}

export function event_id(event, id) {
  let _record = event;
  return new SSEEvent(new Some(id), _record.event, _record.retry, _record.data);
}

export function event_name(event, name) {
  let _record = event;
  return new SSEEvent(_record.id, new Some(name), _record.retry, _record.data);
}

export function event_retry(event, retry) {
  let _record = event;
  return new SSEEvent(_record.id, _record.event, new Some(retry), _record.data);
}
