import * as $charlist from "../gleam_erlang/gleam/erlang/charlist.mjs";
import * as $process from "../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../gleam_otp/gleam/otp/actor.mjs";
import * as $supervisor from "../gleam_otp/gleam/otp/supervisor.mjs";
import * as $bytes_tree from "../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, CustomType as $CustomType } from "./gleam.mjs";
import * as $acceptor from "./glisten/internal/acceptor.mjs";
import { Pool } from "./glisten/internal/acceptor.mjs";
import * as $handler from "./glisten/internal/handler.mjs";
import * as $listener from "./glisten/internal/listener.mjs";
import * as $socket from "./glisten/socket.mjs";
import * as $options from "./glisten/socket/options.mjs";
import { Certfile, Keyfile } from "./glisten/socket/options.mjs";
import * as $transport from "./glisten/transport.mjs";

export class ListenerClosed extends $CustomType {}

export class ListenerTimeout extends $CustomType {}

export class AcceptorTimeout extends $CustomType {}

export class AcceptorFailed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class AcceptorCrashed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class SystemError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Packet extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class User extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

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

class Server extends $CustomType {
  constructor(listener, supervisor, transport) {
    super();
    this.listener = listener;
    this.supervisor = supervisor;
    this.transport = transport;
  }
}

export class ConnectionInfo extends $CustomType {
  constructor(port, ip_address) {
    super();
    this.port = port;
    this.ip_address = ip_address;
  }
}

export class Connection extends $CustomType {
  constructor(socket, transport, subject) {
    super();
    this.socket = socket;
    this.transport = transport;
    this.subject = subject;
  }
}

class Handler extends $CustomType {
  constructor(interface$, on_init, loop, on_close, pool_size, http2_support, ipv6_support) {
    super();
    this.interface = interface$;
    this.on_init = on_init;
    this.loop = loop;
    this.on_close = on_close;
    this.pool_size = pool_size;
    this.http2_support = http2_support;
    this.ipv6_support = ipv6_support;
  }
}

export function get_supervisor(server) {
  return server.supervisor;
}

export function convert_ip_address(ip) {
  if (ip instanceof $options.IpV4) {
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

function join_ipv6_fields(fields) {
  let _pipe = $list.map(fields, $int.to_base16);
  return $string.join(_pipe, ":");
}

function ipv6_zeros(
  loop$fields,
  loop$pos,
  loop$len,
  loop$max_start,
  loop$max_len
) {
  while (true) {
    let fields = loop$fields;
    let pos = loop$pos;
    let len = loop$len;
    let max_start = loop$max_start;
    let max_len = loop$max_len;
    if (fields.hasLength(0) && (max_len > 1)) {
      return new Ok([max_start, max_start + max_len]);
    } else if (fields.hasLength(0)) {
      return new Error(undefined);
    } else if (fields.atLeastLength(1) && (fields.head === 0)) {
      let x = fields.head;
      let xs = fields.tail;
      let len$1 = len + 1;
      let $ = len$1 > max_len;
      if ($) {
        loop$fields = xs;
        loop$pos = pos + 1;
        loop$len = len$1;
        loop$max_start = (pos + 1) - len$1;
        loop$max_len = len$1;
      } else {
        loop$fields = xs;
        loop$pos = pos + 1;
        loop$len = len$1;
        loop$max_start = max_start;
        loop$max_len = max_len;
      }
    } else {
      let xs = fields.tail;
      loop$fields = xs;
      loop$pos = pos + 1;
      loop$len = 0;
      loop$max_start = max_start;
      loop$max_len = max_len;
    }
  }
}

export function ip_address_to_string(address) {
  if (address instanceof IpV4) {
    let a = address[0];
    let b = address[1];
    let c = address[2];
    let d = address[3];
    let _pipe = toList([a, b, c, d]);
    let _pipe$1 = $list.map(_pipe, $int.to_string);
    return $string.join(_pipe$1, ".");
  } else {
    let a = address[0];
    let b = address[1];
    let c = address[2];
    let d = address[3];
    let e = address[4];
    let f = address[5];
    let g = address[6];
    let h = address[7];
    let fields = toList([a, b, c, d, e, f, g, h]);
    let _block;
    let $ = ipv6_zeros(fields, 0, 0, 0, 0);
    if (!$.isOk()) {
      _block = join_ipv6_fields(fields);
    } else {
      let start = $[0][0];
      let end = $[0][1];
      _block = (join_ipv6_fields($list.take(fields, start)) + "::") + join_ipv6_fields(
        $list.drop(fields, end),
      );
    }
    let _pipe = _block;
    return $string.lowercase(_pipe);
  }
}

function convert_on_init(on_init) {
  return (conn) => {
    let connection = new Connection(conn.socket, conn.transport, conn.sender);
    return on_init(connection);
  };
}

export function handler(on_init, loop) {
  return new Handler(
    new $options.Loopback(),
    on_init,
    loop,
    new None(),
    10,
    false,
    false,
  );
}

export function with_close(handler, on_close) {
  let _record = handler;
  return new Handler(
    _record.interface,
    _record.on_init,
    _record.loop,
    new Some(on_close),
    _record.pool_size,
    _record.http2_support,
    _record.ipv6_support,
  );
}

export function with_pool_size(handler, size) {
  let _record = handler;
  return new Handler(
    _record.interface,
    _record.on_init,
    _record.loop,
    _record.on_close,
    size,
    _record.http2_support,
    _record.ipv6_support,
  );
}

export function with_http2(handler) {
  let _record = handler;
  return new Handler(
    _record.interface,
    _record.on_init,
    _record.loop,
    _record.on_close,
    _record.pool_size,
    true,
    _record.ipv6_support,
  );
}

export function with_ipv6(handler) {
  let _record = handler;
  return new Handler(
    _record.interface,
    _record.on_init,
    _record.loop,
    _record.on_close,
    _record.pool_size,
    _record.http2_support,
    true,
  );
}
