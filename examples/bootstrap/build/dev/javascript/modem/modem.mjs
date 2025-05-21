import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import { Uri } from "../gleam_stdlib/gleam/uri.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $query from "../lustre/lustre/dev/query.mjs";
import * as $lustre_simulate from "../lustre/lustre/dev/simulate.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import * as $vattr from "../lustre/lustre/vdom/vattr.mjs";
import { Attribute } from "../lustre/lustre/vdom/vattr.mjs";
import * as $vnode from "../lustre/lustre/vdom/vnode.mjs";
import { Element } from "../lustre/lustre/vdom/vnode.mjs";
import { Ok, Error, CustomType as $CustomType } from "./gleam.mjs";
import {
  do_initial_uri as initial_uri,
  do_init,
  do_init as do_advanced,
  do_push,
  do_replace,
  do_load,
  do_forward,
  do_back,
} from "./modem.ffi.mjs";

export { initial_uri };

export class Options extends $CustomType {
  constructor(handle_internal_links, handle_external_links) {
    super();
    this.handle_internal_links = handle_internal_links;
    this.handle_external_links = handle_external_links;
  }
}

export function init(handler) {
  return $effect.from(
    (dispatch) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return do_init(
            (uri) => {
              let _pipe = uri;
              let _pipe$1 = handler(_pipe);
              return dispatch(_pipe$1);
            },
          );
        },
      );
    },
  );
}

export function advanced(options, handler) {
  return $effect.from(
    (dispatch) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return ((_capture) => { return do_advanced(_capture, options); })(
            (uri) => {
              let _pipe = uri;
              let _pipe$1 = handler(_pipe);
              return dispatch(_pipe$1);
            },
          );
        },
      );
    },
  );
}

export function load(uri) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => { return do_load(uri); },
      );
    },
  );
}

export function forward(steps) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => { return do_forward(steps); },
      );
    },
  );
}

export function back(steps) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => { return do_back(steps); },
      );
    },
  );
}

export function simulate(simulation, query, route, handler) {
  return $result.unwrap_both(
    $result.try$(
      $result.replace_error(
        $uri.parse(route),
        $lustre_simulate.problem(
          simulation,
          "ModemInvalidBaseURL",
          ("`" + route) + "` is not a valid base URL",
        ),
      ),
      (base) => {
        return $result.try$(
          $result.replace_error(
            $uri.origin(base),
            $lustre_simulate.problem(
              simulation,
              "ModemInvalidBaseURL",
              ("`" + route) + "` is not a valid base URL",
            ),
          ),
          (origin) => {
            return $result.try$(
              $result.replace_error(
                $query.find($lustre_simulate.view(simulation), query),
                $lustre_simulate.problem(
                  simulation,
                  "EventTargetNotFound",
                  "No element matching " + $query.to_readable_string(query),
                ),
              ),
              (target) => {
                return $result.try$(
                  (() => {
                    if (target instanceof Element && target.tag === "a") {
                      let attributes = target.attributes;
                      return new Ok(attributes);
                    } else {
                      return new Error(
                        $lustre_simulate.problem(
                          simulation,
                          "ModemInvalidTarget",
                          "Target must be an <a> tag",
                        ),
                      );
                    }
                  })(),
                  (attributes) => {
                    return $result.try$(
                      $result.replace_error(
                        $list.find_map(
                          attributes,
                          (attribute) => {
                            if (attribute instanceof Attribute &&
                            attribute.name === "href") {
                              let value = attribute.value;
                              return new Ok(value);
                            } else {
                              return new Error(undefined);
                            }
                          },
                        ),
                        $lustre_simulate.problem(
                          simulation,
                          "ModemMissingHref",
                          "Target must have an `href` attribute",
                        ),
                      ),
                      (href) => {
                        return $result.try$(
                          $result.replace_error(
                            $uri.parse(href),
                            $lustre_simulate.problem(
                              simulation,
                              "ModemInvalidHref",
                              ("`" + href) + "` is not a valid URL",
                            ),
                          ),
                          (relative) => {
                            return $result.try$(
                              (() => {
                                let $ = $uri.origin(relative);
                                if ($.isOk() && (origin !== $[0])) {
                                  let relative_origin = $[0];
                                  return new Error(
                                    $lustre_simulate.problem(
                                      simulation,
                                      "ModemExternalUrl",
                                      ("`" + href) + "` is an external URL and cannot be simulated",
                                    ),
                                  );
                                } else {
                                  return new Ok(undefined);
                                }
                              })(),
                              (_) => {
                                return $result.try$(
                                  $result.replace_error(
                                    $uri.merge(base, relative),
                                    $lustre_simulate.problem(
                                      simulation,
                                      "ModemInvalidBaseURL",
                                      ("`" + route) + "` is not a valid base URL",
                                    ),
                                  ),
                                  (resolved) => {
                                    return new Ok(
                                      $lustre_simulate.message(
                                        simulation,
                                        handler(resolved),
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
              },
            );
          },
        );
      },
    ),
  );
}

const relative = /* @__PURE__ */ new Uri(
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  "",
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
);

export function push(path, query, fragment) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return do_push(
            (() => {
              let _record = relative;
              return new Uri(
                _record.scheme,
                _record.userinfo,
                _record.host,
                _record.port,
                path,
                query,
                fragment,
              );
            })(),
          );
        },
      );
    },
  );
}

export function replace(path, query, fragment) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return do_replace(
            (() => {
              let _record = relative;
              return new Uri(
                _record.scheme,
                _record.userinfo,
                _record.host,
                _record.port,
                path,
                query,
                fragment,
              );
            })(),
          );
        },
      );
    },
  );
}
