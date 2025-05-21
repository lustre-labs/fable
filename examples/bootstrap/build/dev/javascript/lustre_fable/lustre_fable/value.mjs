import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

export class PrimitiveString extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class PrimitiveInt extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class PrimitiveFloat extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class PrimitiveBool extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function as_string(value) {
  if (value instanceof PrimitiveString) {
    let string = value[0];
    return string;
  } else {
    return "";
  }
}

export function as_int(value) {
  if (value instanceof PrimitiveInt) {
    let int = value[0];
    return int;
  } else {
    return 0;
  }
}

export function as_float(value) {
  if (value instanceof PrimitiveFloat) {
    let float = value[0];
    return float;
  } else {
    return 0.0;
  }
}

export function as_bool(value) {
  if (value instanceof PrimitiveBool) {
    let bool = value[0];
    return bool;
  } else {
    return false;
  }
}

export function to_json(value) {
  if (value instanceof PrimitiveString) {
    let string = value[0];
    return $json.string(string);
  } else if (value instanceof PrimitiveInt) {
    let int = value[0];
    return $json.int(int);
  } else if (value instanceof PrimitiveFloat) {
    let float = value[0];
    return $json.float(float);
  } else {
    let bool = value[0];
    return $json.bool(bool);
  }
}

export function decoder() {
  return $decode.one_of(
    (() => {
      let _pipe = $decode.string;
      return $decode.map(_pipe, (var0) => { return new PrimitiveString(var0); });
    })(),
    toList([
      (() => {
        let _pipe = $decode.int;
        return $decode.map(_pipe, (var0) => { return new PrimitiveInt(var0); });
      })(),
      (() => {
        let _pipe = $decode.float;
        return $decode.map(
          _pipe,
          (var0) => { return new PrimitiveFloat(var0); },
        );
      })(),
      (() => {
        let _pipe = $decode.bool;
        return $decode.map(_pipe, (var0) => { return new PrimitiveBool(var0); });
      })(),
    ]),
  );
}
