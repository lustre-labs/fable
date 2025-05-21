import * as $escape from "./houdini/internal/escape_generic.mjs";

export function escape(string) {
  return $escape.escape(string);
}
