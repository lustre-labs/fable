//

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

// TYPES -----------------------------------------------------------------------

///
///
pub type Value {
  PrimitiveString(String)
  PrimitiveInt(Int)
  PrimitiveFloat(Float)
  PrimitiveBool(Bool)
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn as_string(value: Value) -> String {
  case value {
    PrimitiveString(string) -> string
    _ -> ""
  }
}

///
///
pub fn as_int(value: Value) -> Int {
  case value {
    PrimitiveInt(int) -> int
    _ -> 0
  }
}

///
///
pub fn as_float(value: Value) -> Float {
  case value {
    PrimitiveFloat(float) -> float
    _ -> 0.0
  }
}

///
///
pub fn as_bool(value: Value) -> Bool {
  case value {
    PrimitiveBool(bool) -> bool
    _ -> False
  }
}

// SERIALISATION ---------------------------------------------------------------

///
///
pub fn to_json(value: Value) -> Json {
  case value {
    PrimitiveString(string) -> json.string(string)
    PrimitiveInt(int) -> json.int(int)
    PrimitiveFloat(float) -> json.float(float)
    PrimitiveBool(bool) -> json.bool(bool)
  }
}

///
///
pub fn decoder() -> Decoder(Value) {
  decode.one_of(decode.string |> decode.map(PrimitiveString), or: [
    decode.int |> decode.map(PrimitiveInt),
    decode.float |> decode.map(PrimitiveFloat),
    decode.bool |> decode.map(PrimitiveBool),
  ])
}
