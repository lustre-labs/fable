import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import lustre/effect.{type Effect}

const storage_key = "fable:color-scheme"

pub type ColourScheme {
  Light
  Dark
}

pub type Preference {
  System
  Selected(ColourScheme)
}

pub fn from_dark(is_dark: Bool) -> ColourScheme {
  case is_dark {
    True -> Dark
    False -> Light
  }
}

pub fn resolve(
  preference: Preference,
  system_scheme: ColourScheme,
) -> ColourScheme {
  case preference {
    System -> system_scheme
    Selected(scheme) -> scheme
  }
}

/// Only a user interaction can clear an override that matches the system.
pub fn select(scheme: ColourScheme, system_scheme: ColourScheme) -> Preference {
  case scheme == system_scheme {
    True -> System
    False -> Selected(scheme)
  }
}

pub fn apply(preference: Preference) -> Effect(message) {
  use _, root <- effect.before_paint
  apply_scheme(root, preference_to_string(preference))
}

pub fn persist(preference: Preference) -> Effect(message) {
  use _ <- effect.from
  case preference {
    System -> set_item(storage_key, "")
    Selected(_) -> set_item(storage_key, preference_to_string(preference))
  }
}

fn preference_to_string(preference: Preference) -> String {
  case preference {
    System -> "light dark"
    Selected(Light) -> "light"
    Selected(Dark) -> "dark"
  }
}

pub fn load(on_preference: fn(Preference) -> message) -> Effect(message) {
  use dispatch <- effect.from

  let preference = case decode.run(get_item(storage_key), decode.string) {
    Ok("light") -> Selected(Light)
    Ok("dark") -> Selected(Dark)
    _ -> System
  }
  dispatch(on_preference(preference))
}

pub fn subscribe(
  on_system_change: fn(ColourScheme) -> message,
) -> Effect(message) {
  use dispatch <- effect.from
  use dark <- on_prefers_dark
  dispatch(on_system_change(from_dark(dark)))
}

@external(javascript, "./theme.ffi.mjs", "onPrefersDark")
fn on_prefers_dark(callback: fn(Bool) -> Nil) -> Nil

@external(javascript, "./theme.ffi.mjs", "getItem")
fn get_item(key: String) -> Dynamic

@external(javascript, "./theme.ffi.mjs", "applyScheme")
fn apply_scheme(root: Dynamic, scheme: String) -> Nil

@external(javascript, "./theme.ffi.mjs", "setItem")
fn set_item(key: String, value: String) -> Nil
