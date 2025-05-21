import { load as do$ } from "./argv_ffi.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";

export class Argv extends $CustomType {
  constructor(runtime, program, arguments$) {
    super();
    this.runtime = runtime;
    this.program = program;
    this.arguments = arguments$;
  }
}

export function load() {
  let $ = do$();
  let runtime = $[0];
  let program = $[1];
  let arguments$ = $[2];
  return new Argv(runtime, program, arguments$);
}
