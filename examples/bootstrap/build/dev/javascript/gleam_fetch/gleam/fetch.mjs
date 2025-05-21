import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../gleam_http/gleam/http/response.mjs";
import * as $promise from "../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import { Ok, CustomType as $CustomType } from "../gleam.mjs";
import * as $form_data from "../gleam/fetch/form_data.mjs";
import {
  raw_send,
  to_fetch_request,
  form_data_to_fetch_request,
  bitarray_request_to_fetch_request,
  from_fetch_response,
  read_bytes_body,
  read_text_body,
  read_json_body,
} from "../gleam_fetch_ffi.mjs";

export {
  bitarray_request_to_fetch_request,
  form_data_to_fetch_request,
  from_fetch_response,
  raw_send,
  read_bytes_body,
  read_json_body,
  read_text_body,
  to_fetch_request,
};

export class NetworkError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class UnableToReadBody extends $CustomType {}

export class InvalidJsonBody extends $CustomType {}

export function send(request) {
  let _pipe = request;
  let _pipe$1 = to_fetch_request(_pipe);
  let _pipe$2 = raw_send(_pipe$1);
  return $promise.try_await(
    _pipe$2,
    (resp) => { return $promise.resolve(new Ok(from_fetch_response(resp))); },
  );
}

export function send_form_data(request) {
  let _pipe = request;
  let _pipe$1 = form_data_to_fetch_request(_pipe);
  let _pipe$2 = raw_send(_pipe$1);
  return $promise.try_await(
    _pipe$2,
    (resp) => { return $promise.resolve(new Ok(from_fetch_response(resp))); },
  );
}

export function send_bits(request) {
  let _pipe = request;
  let _pipe$1 = bitarray_request_to_fetch_request(_pipe);
  let _pipe$2 = raw_send(_pipe$1);
  return $promise.try_await(
    _pipe$2,
    (resp) => { return $promise.resolve(new Ok(from_fetch_response(resp))); },
  );
}
