import { empty as new$, get, insert, remove as delete$, size } from "./mutable_map.ffi.mjs";

export { delete$, get, insert, new$, size };

export function is_empty(map) {
  return size(map) === 0;
}
