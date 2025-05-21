import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import { Error } from "../../gleam.mjs";
import * as $array from "../../gleam/javascript/array.mjs";
import {
  newPromise as new$,
  start_promise as start,
  resolve,
  rescue,
  then_await as await$,
  map_promise as map,
  all_promises as await_array,
  all_promises as do_await_list,
  race_promises as race_list,
  race_promises as race_array,
  wait,
} from "../../gleam_javascript_ffi.mjs";

export {
  await$,
  await_array,
  map,
  new$,
  race_array,
  race_list,
  rescue,
  resolve,
  start,
  wait,
};

export function tap(promise, callback) {
  let _pipe = promise;
  return map(
    _pipe,
    (a) => {
      callback(a);
      return a;
    },
  );
}

export function map_try(promise, callback) {
  let _pipe = promise;
  return map(
    _pipe,
    (result) => {
      if (result.isOk()) {
        let a = result[0];
        return callback(a);
      } else {
        let e = result[0];
        return new Error(e);
      }
    },
  );
}

export function try_await(promise, callback) {
  let _pipe = promise;
  return await$(
    _pipe,
    (result) => {
      if (result.isOk()) {
        let a = result[0];
        return callback(a);
      } else {
        let e = result[0];
        return resolve(new Error(e));
      }
    },
  );
}

export function await_list(xs) {
  let _pipe = xs;
  let _pipe$1 = do_await_list(_pipe);
  return map(_pipe$1, $array.to_list);
}
