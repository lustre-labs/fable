// IMPORTS ---------------------------------------------------------------------

import lustre/effect.{type Effect}

// EFFECTS ---------------------------------------------------------------------

pub fn focus(selector: String) -> Effect(msg) {
  use _, _ <- effect.after_paint
  do_focus(selector)
}

// UTILS -----------------------------------------------------------------------

@external(javascript, "./document.ffi.mjs", "focus")
pub fn do_focus(_selector: String) -> Nil {
  Nil
}
