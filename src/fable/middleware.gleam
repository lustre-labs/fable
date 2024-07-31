import gleam/function
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import lustre.{type App}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html

///
///
pub opaque type Middleware(to, from) {
  Middleware(run: fn(Translation(to, from)) -> Component(to, from))
}

pub type Component(to, from) {
  Component(
    init: to,
    effects: List(fn(from) -> Effect(fn(from) -> from)),
    controls: List(fn(from) -> Element(fn(from) -> from)),
    render: fn(from, List(Element(fn(from) -> from))) ->
      Element(fn(from) -> from),
  )
}

///
///
pub fn compose(
  middleware: Middleware(model, model),
) -> App(Nil, model, fn(model) -> model) {
  let Component(model, effects, controls, render) = middleware.run(identity())

  let init = fn(_) {
    #(model, effect.batch(list.map(effects, function.apply1(_, model))))
  }

  let update = fn(model, f) {
    let next = f(model)
    let effects = effect.batch(list.map(effects, function.apply1(_, next)))

    #(next, effects)
  }

  let view = fn(model) {
    render(model, list.map(controls, function.apply1(_, model)))
  }

  lustre.application(init, update, view)
}

// STATE -----------------------------------------------------------------------

///
///
pub type State(a, model) {
  State(
    get: fn(model) -> a,
    set: fn(a) -> fn(model) -> model,
    update: fn(fn(a) -> a) -> fn(model) -> model,
  )
}

///
///
pub fn state(
  init: a,
  next: fn(State(a, model)) -> Middleware(rest, model),
) -> Middleware(#(a, rest), model) {
  use rest_to_outer <- Middleware
  let inner_to_outer =
    compose_translations(tuple_second_translation(), rest_to_outer)

  let state = compose_with_state(rest_to_outer, tuple_first_state())
  let Component(rest, eff, controls, render) = next(state).run(inner_to_outer)

  Component(#(init, rest), eff, controls, render)
}

// PREVIOUS --------------------------------------------------------------------

///
///
pub fn previous(
  value: State(a, model),
  next: fn(State(a, model), fn(model) -> Option(a)) -> Middleware(rest, model),
) -> Middleware(#(Option(a), rest), model) {
  use prev <- state(None)
  let set = fn(new) {
    fn(model) { model |> prev.set(Some(value.get(model))) |> value.set(new) }
  }

  let update = fn(f) {
    fn(model) { model |> prev.set(Some(value.get(model))) |> value.update(f) }
  }

  next(State(..value, set: set, update: update), prev.get)
}

// FOLD ------------------------------------------------------------------------

///
///
pub type Fold(a, msg, model) {
  Fold(get: fn(model) -> a, dispatch: fn(msg) -> fn(model) -> model)
}

///
///
pub fn fold(
  init: a,
  update: fn(a, msg) -> a,
  next: fn(Fold(a, msg, model)) -> Middleware(rest, model),
) {
  use value <- state(init)
  let dispatch = fn(action) { value.update(update(_, action)) }

  next(Fold(value.get, dispatch))
}

// EFFECT ----------------------------------------------------------------------

///
///
pub fn effect(
  eff: fn(model) -> Effect(fn(model) -> model),
  next: fn() -> Middleware(rest, model),
) -> Middleware(rest, model) {
  use rest_to_model <- Middleware
  let Component(state, effects, controls, render) = next().run(rest_to_model)

  Component(state, [eff, ..effects], controls, render)
}

// RETURN ----------------------------------------------------------------------

///
///
pub fn return(
  render: fn(model, List(Element(fn(model) -> model))) ->
    Element(fn(model) -> model),
) -> Middleware(Nil, model) {
  use _ <- Middleware

  Component(Nil, [], [], render)
}

// CONTROL ---------------------------------------------------------------------

pub fn control(
  render_control: fn(model) -> Element(fn(model) -> model),
  next: fn() -> Middleware(rest, model),
) -> Middleware(rest, model) {
  use rest_to_model <- Middleware
  let Component(state, effects, controls, render) = next().run(rest_to_model)

  Component(state, effects, [render_control, ..controls], render)
}

// TRANSLATIONS ----------------------------------------------------------------

type Translation(to, from) {
  Translation(get: fn(from) -> to, map: fn(fn(to) -> to) -> fn(from) -> from)
}

fn compose_translations(
  a: Translation(a, b),
  b: Translation(b, c),
) -> Translation(a, c) {
  let get = fn(outer) { outer |> b.get |> a.get }
  let map = fn(f) { b.map(a.map(f)) }

  Translation(get, map)
}

fn identity() -> Translation(a, a) {
  Translation(get: fn(x) { x }, map: fn(f) { fn(x) { f(x) } })
}

fn tuple_second_translation() -> Translation(b, #(a, b)) {
  let get = fn(t: #(a, b)) { t.1 }
  let map = fn(f) { fn(t: #(a, b)) { #(t.0, f(t.1)) } }

  Translation(get, map)
}

fn tuple_first_state() -> State(a, #(a, b)) {
  let get = pair.first
  let set = fn(a) { fn(t: #(a, b)) { #(a, t.1) } }
  let update = fn(f) { fn(t: #(a, b)) { #(f(t.0), t.1) } }

  State(get, set, update)
}

fn compose_with_state(a: Translation(b, c), b: State(a, b)) -> State(a, c) {
  let get = fn(c) { c |> a.get |> b.get }
  let set = fn(new) { fn(c) { c |> a.map(b.set(new)) } }
  let update = fn(f) { fn(c) { c |> a.map(b.update(f)) } }

  State(get, set, update)
}
