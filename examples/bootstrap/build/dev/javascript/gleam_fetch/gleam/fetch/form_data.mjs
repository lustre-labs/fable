import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import {
  newFormData as new$,
  appendFormData as append,
  appendBitsFormData as append_bits,
  setFormData as set,
  setBitsFormData as set_bits,
  deleteFormData as delete$,
  getFormData as get,
  getBitsFormData as get_bits,
  hasFormData as contains,
  keysFormData as keys,
} from "../../gleam_fetch_ffi.mjs";

export {
  append,
  append_bits,
  contains,
  delete$,
  get,
  get_bits,
  keys,
  new$,
  set,
  set_bits,
};
