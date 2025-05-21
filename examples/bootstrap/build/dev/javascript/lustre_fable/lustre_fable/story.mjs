import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $regexp from "../../gleam_regexp/gleam/regexp.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $lustre from "../../lustre/lustre.mjs";
import * as $attribute from "../../lustre/lustre/attribute.mjs";
import * as $component from "../../lustre/lustre/component.mjs";
import * as $effect from "../../lustre/lustre/effect.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import * as $html from "../../lustre/lustre/element/html.mjs";
import * as $event from "../../lustre/lustre/event.mjs";
import * as $rsvp from "../../rsvp/rsvp.mjs";
import {
  Ok,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../gleam.mjs";
import * as $value from "../lustre_fable/value.mjs";

export class Story extends $CustomType {
  constructor(title, route, component, scene) {
    super();
    this.title = title;
    this.route = route;
    this.component = component;
    this.scene = scene;
  }
}

export class StoryBuilder extends $CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
}

export class StoryConfig extends $CustomType {
  constructor(title, inputs, options, view) {
    super();
    this.title = title;
    this.inputs = inputs;
    this.options = options;
    this.view = view;
  }
}

export class Model extends $CustomType {
  constructor(lookup) {
    super();
    this.lookup = lookup;
  }
}

export class ExternalStylesheetLoaded extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ComponentUpdatedValue extends $CustomType {
  constructor(key, value) {
    super();
    this.key = key;
    this.value = value;
  }
}

export class UserEditedValue extends $CustomType {
  constructor(key, value) {
    super();
    this.key = key;
    this.value = value;
  }
}

class SceneModel extends $CustomType {
  constructor(lookup, stylesheets, pending_stylesheets) {
    super();
    this.lookup = lookup;
    this.stylesheets = stylesheets;
    this.pending_stylesheets = pending_stylesheets;
  }
}

function story_init(_) {
  return new Model($dict.new$());
}

function story_update(model, msg) {
  if (msg instanceof ExternalStylesheetLoaded) {
    return model;
  } else if (msg instanceof ComponentUpdatedValue) {
    let key = msg.key;
    let value = msg.value;
    return new Model($dict.insert(model.lookup, key, value));
  } else {
    let key = msg.key;
    let value = msg.value;
    return new Model($dict.insert(model.lookup, key, value));
  }
}

function story_view(model, scene, inputs) {
  let handle_change = $decode.subfield(
    toList(["detail", "key"]),
    $decode.int,
    (key) => {
      return $decode.subfield(
        toList(["detail", "value"]),
        $value.decoder(),
        (value) => {
          return $decode.success(new ComponentUpdatedValue(key, value));
        },
      );
    },
  );
  let attributes = $dict.fold(
    model.lookup,
    toList([]),
    (attributes, key, value) => {
      return listPrepend(
        $attribute.property($int.to_string(key), $value.to_json(value)),
        attributes,
      );
    },
  );
  return $html.div(
    toList([$attribute.class$("@container h-full")]),
    toList([
      $html.div(
        toList([
          $attribute.class$("h-full grid grid-cols-1 grid-rows-[1fr_300px]"),
          $attribute.class$("@3xl:grid-cols-[1fr_300px] @3xl:grid-rows-1"),
        ]),
        toList([
          $html.main(
            toList([
              $attribute.class$(
                "p-4 grid grid-cols-1 grid-rows-1 place-items-center",
              ),
            ]),
            toList([
              $element.element(
                scene,
                listPrepend($event.on("change", handle_change), attributes),
                toList([]),
              ),
            ]),
          ),
          $html.aside(
            toList([
              $attribute.class$("p-4 border-t"),
              $attribute.class$("@3xl:border-t-0 @3xl:border-l"),
            ]),
            $list.map(inputs, (input) => { return input(model); }),
          ),
        ]),
      ),
    ]),
  );
}

function scene_init(_, stylesheets, external_stylesheets) {
  let model = new SceneModel(
    $dict.new$(),
    stylesheets,
    $list.length(external_stylesheets),
  );
  let _block;
  let _pipe = external_stylesheets;
  let _pipe$1 = $list.map(
    _pipe,
    (_capture) => {
      return $rsvp.get(
        _capture,
        $rsvp.expect_text(
          (var0) => { return new ExternalStylesheetLoaded(var0); },
        ),
      );
    },
  );
  _block = $effect.batch(_pipe$1);
  let effect = _block;
  return [model, effect];
}

function scene_update(model, msg) {
  if (msg instanceof ComponentUpdatedValue) {
    let key = msg.key;
    let value = msg.value;
    return [
      (() => {
        let _record = model;
        return new SceneModel(
          $dict.insert(model.lookup, key, value),
          _record.stylesheets,
          _record.pending_stylesheets,
        );
      })(),
      $effect.none(),
    ];
  } else if (msg instanceof ExternalStylesheetLoaded && msg[0].isOk()) {
    let css = msg[0][0];
    let _block;
    let _record = model;
    _block = new SceneModel(
      _record.lookup,
      listPrepend(css, model.stylesheets),
      model.pending_stylesheets - 1,
    );
    let model$1 = _block;
    return [model$1, $effect.none()];
  } else if (msg instanceof ExternalStylesheetLoaded && !msg[0].isOk()) {
    let _block;
    let _record = model;
    _block = new SceneModel(
      _record.lookup,
      _record.stylesheets,
      model.pending_stylesheets - 1,
    );
    let model$1 = _block;
    return [model$1, $effect.none()];
  } else {
    let key = msg.key;
    let value = msg.value;
    return [
      model,
      $event.emit(
        "change",
        $json.object(
          toList([["key", $json.int(key)], ["value", $value.to_json(value)]]),
        ),
      ),
    ];
  }
}

function scene_view(model, view) {
  return $element.fragment(
    $list.flatten(
      toList([
        $list.map(
          model.stylesheets,
          (css) => {
            return $html.style(
              toList([]),
              $string.replace(css, ":root", ":host"),
            );
          },
        ),
        toList([
          view(new Model(model.lookup)),
          $html.style(
            toList([]),
            "\n          :host {\n            border-collapse: revert;\n            border-spacing: revert;\n            caption-side: revert;\n            color: revert;\n            cursor: revert;\n            direction: revert;\n            empty-cells: revert;\n            font-family: revert;\n            font-size: revert;\n            font-style: revert;\n            font-variant: revert;\n            font-weight: revert;\n            font-size-adjust: revert;\n            font-stretch: revert;\n            font: revert;\n            letter-spacing: revert;\n            line-height: revert;\n            list-style-image: revert;\n            list-style-position: revert;\n            list-style-type: revert;\n            list-style: revert;\n            orphans: revert;\n            quotes: revert;\n            tab-size: revert;\n            text-align: revert;\n            text-align-last: revert;\n            text-decoration-color: revert;\n            text-indent: revert;\n            text-justify: revert;\n            text-shadow: revert;\n            text-transform: revert;\n            visibility: revert;\n            white-space: revert;\n            widows: revert;\n            word-break: revert;\n            word-spacing: revert;\n            word-wrap: revert;\n          }\n          ",
          ),
        ]),
      ]),
    ),
  );
}

function do_base_tag(loop$base, loop$count) {
  while (true) {
    let base = loop$base;
    let count = loop$count;
    let $ = $lustre.is_registered((base + "-") + $int.to_string(count));
    if ($) {
      loop$base = base;
      loop$count = count + 1;
    } else {
      return (base + "-") + $int.to_string(count);
    }
  }
}

function base_tag(title) {
  let $ = $regexp.from_string("[^a-zA-Z0-9]+");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "lustre_fable/story",
      312,
      "base_tag",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let re = $[0];
  let _block;
  let _pipe = title;
  let _pipe$1 = $string.lowercase(_pipe);
  _block = ((_capture) => { return $regexp.replace(re, _capture, "-"); })(
    _pipe$1,
  );
  let safe_component_name = _block;
  let $1 = $lustre.is_registered(safe_component_name);
  if ($1) {
    return do_base_tag(safe_component_name, 1);
  } else {
    return safe_component_name;
  }
}

export function register(config, stylesheets, external_stylesheets) {
  let base = base_tag(config.title);
  let component = base + "-story";
  let scene = base + "-scene";
  return $result.try$(
    $lustre.register(
      $lustre.simple(
        story_init,
        story_update,
        ((_capture) => { return story_view(_capture, scene, config.inputs); }),
      ),
      component,
    ),
    (_) => {
      return $result.try$(
        $lustre.register(
          $lustre.component(
            (_capture) => {
              return scene_init(_capture, stylesheets, external_stylesheets);
            },
            scene_update,
            (_capture) => { return scene_view(_capture, config.view); },
            config.options,
          ),
          scene,
        ),
        (_) => {
          return new Ok(new Story(config.title, base, component, scene));
        },
      );
    },
  );
}
