import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { do_escape } from "../../houdini.ffi.mjs";

export function escape(text) {
  return do_escape(text);
}
