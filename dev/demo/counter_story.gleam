// IMPORTS ---------------------------------------------------------------------

import demo/counter
import fable.{type Scene, type Story}
import gleam/function
import lustre/dev/query
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
  let template =
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

  fable.story(name:, template:, scenes: [
    incrementing_scene(),
    reset_scene(),
  ])
}

// SCENES ----------------------------------------------------------------------

fn incrementing_scene() -> Scene(Int, Model, Message) {
  fable.scene(name: "Incrementing", init: 0, play: fn(simulation) {
    simulation
    |> simulate.click(on: query.element(matching: query.test_id("incr")))
    |> simulate.click(on: query.element(matching: query.test_id("incr")))
    |> simulate.click(on: query.element(matching: query.test_id("incr")))
  })
}

fn reset_scene() -> Scene(Int, Model, Message) {
  fable.scene(name: "Reset", init: 100, play: fn(simulation) {
    simulation
    |> simulate.click(on: query.element(matching: query.test_id("incr")))
    |> simulate.click(on: query.element(matching: query.test_id("incr")))
    |> simulate.click(on: query.element(matching: query.test_id("reset")))
    |> simulate.click(on: query.element(matching: query.test_id("decr")))
  })
}
