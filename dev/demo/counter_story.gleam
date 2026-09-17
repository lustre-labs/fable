// IMPORTS ---------------------------------------------------------------------

import demo/counter
import fable.{type SceneConfig, type Story}
import gleam/function
import lustre/dev/simulate

// STORY SETUP -----------------------------------------------------------------

type Model =
  Int

type Message {
  Increment
  Decrement
  Reset
}

pub fn setup() -> Story {
  let name = "Counter"
  let app =
    simulate.simple(
      init: function.identity,
      update: fn(model, message) {
        case message {
          Increment -> model + 1
          Decrement -> model - 1
          Reset -> 0
        }
      },
      view: counter.view(_, Increment, Decrement, Reset),
    )

  fable.story(name:, app:, scenes: [
    incrementing_scene(),
    reset_scene(),
  ])
}

// SCENES ----------------------------------------------------------------------

fn incrementing_scene() -> SceneConfig(Int, Model, Message) {
  fable.scene("Incrementing", fable.mobile, 0, fn(simulation) {
    simulation
    |> simulate.event(on: counter.increment(), name: "click", data: [])
    |> simulate.event(on: counter.increment(), name: "click", data: [])
    |> simulate.event(on: counter.increment(), name: "click", data: [])
  })
}

fn reset_scene() -> SceneConfig(Int, Model, Message) {
  fable.scene("Reset", fable.mobile, 100, fn(simulation) {
    simulation
    |> simulate.event(on: counter.increment(), name: "click", data: [])
    |> simulate.event(on: counter.increment(), name: "click", data: [])
    |> simulate.event(on: counter.reset(), name: "click", data: [])
    |> simulate.event(on: counter.decrement(), name: "click", data: [])
  })
}
