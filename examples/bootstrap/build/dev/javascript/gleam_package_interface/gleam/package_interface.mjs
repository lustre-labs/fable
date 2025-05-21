import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class Package extends $CustomType {
  constructor(name, version, gleam_version_constraint, modules) {
    super();
    this.name = name;
    this.version = version;
    this.gleam_version_constraint = gleam_version_constraint;
    this.modules = modules;
  }
}

export class Module extends $CustomType {
  constructor(documentation, type_aliases, types, constants, functions) {
    super();
    this.documentation = documentation;
    this.type_aliases = type_aliases;
    this.types = types;
    this.constants = constants;
    this.functions = functions;
  }
}

export class TypeAlias extends $CustomType {
  constructor(documentation, deprecation, parameters, alias) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.parameters = parameters;
    this.alias = alias;
  }
}

export class TypeDefinition extends $CustomType {
  constructor(documentation, deprecation, parameters, constructors) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.parameters = parameters;
    this.constructors = constructors;
  }
}

export class TypeConstructor extends $CustomType {
  constructor(documentation, name, parameters) {
    super();
    this.documentation = documentation;
    this.name = name;
    this.parameters = parameters;
  }
}

export class Parameter extends $CustomType {
  constructor(label, type_) {
    super();
    this.label = label;
    this.type_ = type_;
  }
}

export class Constant extends $CustomType {
  constructor(documentation, deprecation, implementations, type_) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.implementations = implementations;
    this.type_ = type_;
  }
}

export class Function extends $CustomType {
  constructor(documentation, deprecation, implementations, parameters, return$) {
    super();
    this.documentation = documentation;
    this.deprecation = deprecation;
    this.implementations = implementations;
    this.parameters = parameters;
    this.return = return$;
  }
}

export class Deprecation extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}

export class Implementations extends $CustomType {
  constructor(gleam, uses_erlang_externals, uses_javascript_externals, can_run_on_erlang, can_run_on_javascript) {
    super();
    this.gleam = gleam;
    this.uses_erlang_externals = uses_erlang_externals;
    this.uses_javascript_externals = uses_javascript_externals;
    this.can_run_on_erlang = can_run_on_erlang;
    this.can_run_on_javascript = can_run_on_javascript;
  }
}

export class Tuple extends $CustomType {
  constructor(elements) {
    super();
    this.elements = elements;
  }
}

export class Fn extends $CustomType {
  constructor(parameters, return$) {
    super();
    this.parameters = parameters;
    this.return = return$;
  }
}

export class Variable extends $CustomType {
  constructor(id) {
    super();
    this.id = id;
  }
}

export class Named extends $CustomType {
  constructor(name, package$, module, parameters) {
    super();
    this.name = name;
    this.package = package$;
    this.module = module;
    this.parameters = parameters;
  }
}

export function deprecation_decoder() {
  return $decode.field(
    "message",
    $decode.string,
    (message) => { return $decode.success(new Deprecation(message)); },
  );
}

export function implementations_decoder() {
  return $decode.field(
    "gleam",
    $decode.bool,
    (gleam) => {
      return $decode.field(
        "uses-erlang-externals",
        $decode.bool,
        (uses_erlang_externals) => {
          return $decode.field(
            "uses-javascript-externals",
            $decode.bool,
            (uses_javascript_externals) => {
              return $decode.optional_field(
                "can-run-on-erlang",
                new $option.None(),
                $decode.optional($decode.bool),
                (can_run_on_erlang) => {
                  return $decode.optional_field(
                    "can-run-on-javascript",
                    new $option.None(),
                    $decode.optional($decode.bool),
                    (can_run_on_javascript) => {
                      let _block;
                      let _pipe = can_run_on_erlang;
                      _block = $option.unwrap(_pipe, gleam);
                      let can_run_on_erlang$1 = _block;
                      let _block$1;
                      let _pipe$1 = can_run_on_javascript;
                      _block$1 = $option.unwrap(_pipe$1, gleam);
                      let can_run_on_javascript$1 = _block$1;
                      return $decode.success(
                        new Implementations(
                          gleam,
                          uses_erlang_externals,
                          uses_javascript_externals,
                          can_run_on_erlang$1,
                          can_run_on_javascript$1,
                        ),
                      );
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

export function type_decoder() {
  return $decode.field(
    "kind",
    $decode.string,
    (kind) => {
      if (kind === "variable") {
        return $decode.field(
          "id",
          $decode.int,
          (id) => { return $decode.success(new Variable(id)); },
        );
      } else if (kind === "tuple") {
        return $decode.field(
          "elements",
          $decode.list(type_decoder()),
          (elements) => { return $decode.success(new Tuple(elements)); },
        );
      } else if (kind === "named") {
        return $decode.field(
          "name",
          $decode.string,
          (name) => {
            return $decode.field(
              "package",
              $decode.string,
              (package$) => {
                return $decode.field(
                  "module",
                  $decode.string,
                  (module) => {
                    return $decode.field(
                      "parameters",
                      $decode.list(type_decoder()),
                      (parameters) => {
                        return $decode.success(
                          new Named(name, package$, module, parameters),
                        );
                      },
                    );
                  },
                );
              },
            );
          },
        );
      } else if (kind === "fn") {
        return $decode.field(
          "parameters",
          $decode.list(type_decoder()),
          (parameters) => {
            return $decode.field(
              "return",
              type_decoder(),
              (return$) => {
                return $decode.success(new Fn(parameters, return$));
              },
            );
          },
        );
      } else {
        return $decode.failure(
          new Variable(0),
          "String of variable, tuple, named, or fn",
        );
      }
    },
  );
}

export function type_alias_decoder() {
  return $decode.field(
    "documentation",
    $decode.optional($decode.string),
    (documentation) => {
      return $decode.field(
        "deprecation",
        $decode.optional(deprecation_decoder()),
        (deprecation) => {
          return $decode.field(
            "parameters",
            $decode.int,
            (parameters) => {
              return $decode.field(
                "alias",
                type_decoder(),
                (alias) => {
                  let _pipe = new TypeAlias(
                    documentation,
                    deprecation,
                    parameters,
                    alias,
                  );
                  return $decode.success(_pipe);
                },
              );
            },
          );
        },
      );
    },
  );
}

export function constant_decoder() {
  return $decode.field(
    "documentation",
    $decode.optional($decode.string),
    (documentation) => {
      return $decode.field(
        "deprecation",
        $decode.optional(deprecation_decoder()),
        (deprecation) => {
          return $decode.field(
            "implementations",
            implementations_decoder(),
            (implementations) => {
              return $decode.field(
                "type",
                type_decoder(),
                (type_) => {
                  let _pipe = new Constant(
                    documentation,
                    deprecation,
                    implementations,
                    type_,
                  );
                  return $decode.success(_pipe);
                },
              );
            },
          );
        },
      );
    },
  );
}

export function parameter_decoder() {
  return $decode.field(
    "label",
    $decode.optional($decode.string),
    (label) => {
      return $decode.field(
        "type",
        type_decoder(),
        (type_) => { return $decode.success(new Parameter(label, type_)); },
      );
    },
  );
}

export function function_decoder() {
  return $decode.field(
    "documentation",
    $decode.optional($decode.string),
    (documentation) => {
      return $decode.field(
        "deprecation",
        $decode.optional(deprecation_decoder()),
        (deprecation) => {
          return $decode.field(
            "implementations",
            implementations_decoder(),
            (implementations) => {
              return $decode.field(
                "parameters",
                $decode.list(parameter_decoder()),
                (parameters) => {
                  return $decode.field(
                    "return",
                    type_decoder(),
                    (return$) => {
                      let _pipe = new Function(
                        documentation,
                        deprecation,
                        implementations,
                        parameters,
                        return$,
                      );
                      return $decode.success(_pipe);
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

export function constructor_decoder() {
  return $decode.field(
    "documentation",
    $decode.optional($decode.string),
    (documentation) => {
      return $decode.field(
        "name",
        $decode.string,
        (name) => {
          return $decode.field(
            "parameters",
            $decode.list(parameter_decoder()),
            (parameters) => {
              return $decode.success(
                new TypeConstructor(documentation, name, parameters),
              );
            },
          );
        },
      );
    },
  );
}

export function type_definition_decoder() {
  return $decode.field(
    "documentation",
    $decode.optional($decode.string),
    (documentation) => {
      return $decode.field(
        "deprecation",
        $decode.optional(deprecation_decoder()),
        (deprecation) => {
          return $decode.field(
            "parameters",
            $decode.int,
            (parameters) => {
              return $decode.field(
                "constructors",
                $decode.list(constructor_decoder()),
                (constructors) => {
                  let _pipe = new TypeDefinition(
                    documentation,
                    deprecation,
                    parameters,
                    constructors,
                  );
                  return $decode.success(_pipe);
                },
              );
            },
          );
        },
      );
    },
  );
}

export function module_decoder() {
  return $decode.field(
    "documentation",
    $decode.list($decode.string),
    (documentation) => {
      return $decode.field(
        "type-aliases",
        $decode.dict($decode.string, type_alias_decoder()),
        (type_aliases) => {
          return $decode.field(
            "types",
            $decode.dict($decode.string, type_definition_decoder()),
            (types) => {
              return $decode.field(
                "constants",
                $decode.dict($decode.string, constant_decoder()),
                (constants) => {
                  return $decode.field(
                    "functions",
                    $decode.dict($decode.string, function_decoder()),
                    (functions) => {
                      let _pipe = new Module(
                        documentation,
                        type_aliases,
                        types,
                        constants,
                        functions,
                      );
                      return $decode.success(_pipe);
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

export function decoder() {
  return $decode.field(
    "name",
    $decode.string,
    (name) => {
      return $decode.field(
        "version",
        $decode.string,
        (version) => {
          return $decode.field(
            "gleam-version-constraint",
            $decode.optional($decode.string),
            (gleam_version_constraint) => {
              return $decode.field(
                "modules",
                $decode.dict($decode.string, module_decoder()),
                (modules) => {
                  let _pipe = new Package(
                    name,
                    version,
                    gleam_version_constraint,
                    modules,
                  );
                  return $decode.success(_pipe);
                },
              );
            },
          );
        },
      );
    },
  );
}
