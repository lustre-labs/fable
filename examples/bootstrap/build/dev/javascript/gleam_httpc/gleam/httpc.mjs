import * as $charlist from "../../gleam_erlang/gleam/erlang/charlist.mjs";
import * as $http from "../../gleam_http/gleam/http.mjs";
import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../gleam_http/gleam/http/response.mjs";
import { Response } from "../../gleam_http/gleam/http/response.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $uri from "../../gleam_stdlib/gleam/uri.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class InvalidUtf8Response extends $CustomType {}

export class FailedToConnect extends $CustomType {
  constructor(ip4, ip6) {
    super();
    this.ip4 = ip4;
    this.ip6 = ip6;
  }
}

export class Posix extends $CustomType {
  constructor(code) {
    super();
    this.code = code;
  }
}

export class TlsAlert extends $CustomType {
  constructor(code, detail) {
    super();
    this.code = code;
    this.detail = detail;
  }
}

class Ssl extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Autoredirect extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Binary extends $CustomType {}

class BodyFormat extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class SocketOpts extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Ipfamily extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Inet6fb4 extends $CustomType {}

class Verify extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class VerifyNone extends $CustomType {}

class Builder extends $CustomType {
  constructor(verify_tls, follow_redirects) {
    super();
    this.verify_tls = verify_tls;
    this.follow_redirects = follow_redirects;
  }
}

export function configure() {
  return new Builder(true, false);
}

export function verify_tls(config, which) {
  let _record = config;
  return new Builder(which, _record.follow_redirects);
}

export function follow_redirects(config, which) {
  let _record = config;
  return new Builder(_record.verify_tls, which);
}
