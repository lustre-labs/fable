import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { identity as erase } from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $pair from "../../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $query from "../../lustre/dev/query.mjs";
import * as $effect from "../../lustre/effect.mjs";
import * as $element from "../../lustre/element.mjs";
import * as $events from "../../lustre/vdom/events.mjs";
import * as $path from "../../lustre/vdom/path.mjs";

class App extends $CustomType {
  constructor(init, update, view) {
    super();
    this.init = init;
    this.update = update;
    this.view = view;
  }
}

class Simulation extends $CustomType {
  constructor(update, view, history, model, html) {
    super();
    this.update = update;
    this.view = view;
    this.history = history;
    this.model = model;
    this.html = html;
  }
}

export class Dispatch extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class Event extends $CustomType {
  constructor(target, name, data) {
    super();
    this.target = target;
    this.name = name;
    this.data = data;
  }
}

export class Problem extends $CustomType {
  constructor(name, message) {
    super();
    this.name = name;
    this.message = message;
  }
}

export function simple(init, update, view) {
  return new App(
    (args) => { return [init(args), $effect.none()]; },
    (model, msg) => { return [update(model, msg), $effect.none()]; },
    view,
  );
}

export function application(init, update, view) {
  return new App(init, update, view);
}

export function start(app, args) {
  let $ = app.init(args);
  let model$1 = $[0];
  let html = app.view(model$1);
  return new Simulation(app.update, app.view, toList([]), model$1, html);
}

export function message(simulation, msg) {
  let $ = simulation.update(simulation.model, msg);
  let model$1 = $[0];
  let html = simulation.view(model$1);
  let history$1 = listPrepend(new Dispatch(msg), simulation.history);
  let _record = simulation;
  return new Simulation(_record.update, _record.view, history$1, model$1, html);
}

export function problem(simulation, name, message) {
  let history$1 = listPrepend(new Problem(name, message), simulation.history);
  let _record = simulation;
  return new Simulation(
    _record.update,
    _record.view,
    history$1,
    _record.model,
    _record.html,
  );
}

export function model(simulation) {
  return simulation.model;
}

export function view(simulation) {
  return simulation.html;
}

export function history(simulation) {
  let _pipe = simulation.history;
  return $list.reverse(_pipe);
}

export function event(simulation, query, event, payload) {
  return $result.unwrap_both(
    $result.try$(
      $result.replace_error(
        $query.find_path(simulation.html, query, 0, $path.root),
        problem(
          simulation,
          "EventTargetNotFound",
          "No element matching " + $query.to_readable_string(query),
        ),
      ),
      (_use0) => {
        let path = _use0[1];
        let events = $events.from_node(simulation.html);
        let data = $json.object(payload);
        return $result.try$(
          $result.replace_error(
            $pair.second(
              $events.handle(
                events,
                $path.to_string(path),
                event,
                (() => {
                  let _pipe = data;
                  let _pipe$1 = $json.to_string(_pipe);
                  let _pipe$2 = $json.parse(_pipe$1, $decode.dynamic);
                  return $result.unwrap(_pipe$2, erase(undefined));
                })(),
              ),
            ),
            problem(
              simulation,
              "EventHandlerNotFound",
              (("No " + event) + " handler for element matching ") + $query.to_readable_string(
                query,
              ),
            ),
          ),
          (msg) => {
            let $ = simulation.update(simulation.model, msg);
            let model$1 = $[0];
            let html = simulation.view(model$1);
            let history$1 = listPrepend(
              new Event(query, event, data),
              simulation.history,
            );
            return new Ok(
              (() => {
                let _record = simulation;
                return new Simulation(
                  _record.update,
                  _record.view,
                  history$1,
                  model$1,
                  html,
                );
              })(),
            );
          },
        );
      },
    ),
  );
}

export function click(simulation, query) {
  return event(simulation, query, "click", toList([]));
}

export function input(simulation, query, value) {
  return event(
    simulation,
    query,
    "input",
    toList([["target", $json.object(toList([["value", $json.string(value)]]))]]),
  );
}

export function submit(simulation, query, form_data) {
  return event(
    simulation,
    query,
    "submit",
    toList([
      [
        "detail",
        $json.object(
          toList([
            [
              "formData",
              $json.array(
                form_data,
                (entry) => {
                  return $json.preprocessed_array(
                    toList([$json.string(entry[0]), $json.string(entry[1])]),
                  );
                },
              ),
            ],
          ]),
        ),
      ],
    ]),
  );
}
