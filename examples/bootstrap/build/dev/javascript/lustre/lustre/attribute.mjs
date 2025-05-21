import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $vattr from "../lustre/vdom/vattr.mjs";

export function attribute(name, value) {
  return $vattr.attribute(name, value);
}

export function property(name, value) {
  return $vattr.property(name, value);
}

function boolean_attribute(name, value) {
  if (value) {
    return attribute(name, "");
  } else {
    return property(name, $json.bool(false));
  }
}

export function accesskey(key) {
  return attribute("accesskey", key);
}

export function autocapitalize(value) {
  return attribute("autocapitalize", value);
}

export function autocorrect(enabled) {
  return boolean_attribute("autocorrect", enabled);
}

export function autofocus(should_autofocus) {
  return boolean_attribute("autofocus", should_autofocus);
}

export function class$(name) {
  return attribute("class", name);
}

export function none() {
  return class$("");
}

function do_classes(loop$names, loop$class) {
  while (true) {
    let names = loop$names;
    let class$ = loop$class;
    if (names.hasLength(0)) {
      return class$;
    } else if (names.atLeastLength(1) && names.head[1]) {
      let name$1 = names.head[0];
      let rest = names.tail;
      return ((class$ + name$1) + " ") + do_classes(rest, class$);
    } else {
      let rest = names.tail;
      loop$names = rest;
      loop$class = class$;
    }
  }
}

export function classes(names) {
  return class$(do_classes(names, ""));
}

export function contenteditable(is_editable) {
  return attribute("contenteditable", is_editable);
}

export function data(key, value) {
  return attribute("data-" + key, value);
}

export function dir(direction) {
  return attribute("dir", direction);
}

export function draggable(is_draggable) {
  return attribute(
    "draggable",
    (() => {
      if (is_draggable) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function enterkeyhint(value) {
  return attribute("enterkeyhint", value);
}

export function hidden(is_hidden) {
  return boolean_attribute("hidden", is_hidden);
}

export function id(value) {
  return attribute("id", value);
}

export function inert(is_inert) {
  return boolean_attribute("inert", is_inert);
}

export function inputmode(value) {
  return attribute("inputmode", value);
}

export function is(value) {
  return attribute("is", value);
}

export function itemid(id) {
  return attribute("itemid", id);
}

export function itemprop(name) {
  return attribute("itemprop", name);
}

export function itemscope(has_scope) {
  return boolean_attribute("itemscope", has_scope);
}

export function itemtype(url) {
  return attribute("itemtype", url);
}

export function lang(language) {
  return attribute("lang", language);
}

export function nonce(value) {
  return attribute("nonce", value);
}

export function popover(value) {
  return attribute("popover", value);
}

export function spellcheck(should_check) {
  return attribute(
    "spellcheck",
    (() => {
      if (should_check) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function style(property, value) {
  if (property === "") {
    return class$("");
  } else if (value === "") {
    return class$("");
  } else {
    return attribute("style", ((property + ":") + value) + ";");
  }
}

function do_styles(loop$properties, loop$styles) {
  while (true) {
    let properties = loop$properties;
    let styles = loop$styles;
    if (properties.hasLength(0)) {
      return styles;
    } else if (properties.atLeastLength(1) && properties.head[0] === "") {
      let rest = properties.tail;
      loop$properties = rest;
      loop$styles = styles;
    } else if (properties.atLeastLength(1) && properties.head[1] === "") {
      let rest = properties.tail;
      loop$properties = rest;
      loop$styles = styles;
    } else {
      let name$1 = properties.head[0];
      let value$1 = properties.head[1];
      let rest = properties.tail;
      loop$properties = rest;
      loop$styles = (((styles + name$1) + ":") + value$1) + ";";
    }
  }
}

export function styles(properties) {
  return attribute("style", do_styles(properties, ""));
}

export function tabindex(index) {
  return attribute("tabindex", $int.to_string(index));
}

export function title(text) {
  return attribute("title", text);
}

export function translate(should_translate) {
  return attribute(
    "translate",
    (() => {
      if (should_translate) {
        return "yes";
      } else {
        return "no";
      }
    })(),
  );
}

export function writingsuggestions(enabled) {
  return attribute(
    "writingsuggestions",
    (() => {
      if (enabled) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function href(url) {
  return attribute("href", url);
}

export function target(value) {
  return attribute("target", value);
}

export function download(filename) {
  return attribute("download", filename);
}

export function ping(urls) {
  return attribute("ping", $string.join(urls, " "));
}

export function rel(value) {
  return attribute("rel", value);
}

export function hreflang(language) {
  return attribute("hreflang", language);
}

export function referrerpolicy(value) {
  return attribute("referrerpolicy", value);
}

export function alt(text) {
  return attribute("alt", text);
}

export function src(url) {
  return attribute("src", url);
}

export function srcset(sources) {
  return attribute("srcset", sources);
}

export function sizes(value) {
  return attribute("sizes", value);
}

export function crossorigin(value) {
  return attribute("crossorigin", value);
}

export function usemap(value) {
  return attribute("usemap", value);
}

export function ismap(is_map) {
  return boolean_attribute("ismap", is_map);
}

export function width(value) {
  return attribute("width", $int.to_string(value));
}

export function height(value) {
  return attribute("height", $int.to_string(value));
}

export function decoding(value) {
  return attribute("decoding", value);
}

export function loading(value) {
  return attribute("loading", value);
}

export function fetchpriority(value) {
  return attribute("fetchpriority", value);
}

export function accept_charset(charsets) {
  return attribute("accept-charset", charsets);
}

export function action(url) {
  return attribute("action", url);
}

export function enctype(encoding_type) {
  return attribute("enctype", encoding_type);
}

export function method(http_method) {
  return attribute("method", http_method);
}

export function novalidate(disable_validation) {
  return boolean_attribute("novalidate", disable_validation);
}

export function accept(values) {
  return attribute("accept", $string.join(values, ","));
}

export function alpha(allowed) {
  return boolean_attribute("alpha", allowed);
}

export function autocomplete(value) {
  return attribute("autocomplete", value);
}

export function checked(is_checked) {
  return boolean_attribute("checked", is_checked);
}

export function colorspace(value) {
  return attribute("colorspace", value);
}

export function dirname(direction) {
  return attribute("dirname", direction);
}

export function disabled(is_disabled) {
  return boolean_attribute("disabled", is_disabled);
}

export function for$(id) {
  return attribute("for", id);
}

export function form(id) {
  return attribute("form", id);
}

export function formaction(url) {
  return attribute("formaction", url);
}

export function formenctype(encoding_type) {
  return attribute("formenctype", encoding_type);
}

export function formmethod(method) {
  return attribute("formmethod", method);
}

export function formnovalidate(no_validate) {
  return boolean_attribute("formnovalidate", no_validate);
}

export function formtarget(target) {
  return attribute("formtarget", target);
}

export function list(id) {
  return attribute("list", id);
}

export function max(value) {
  return attribute("max", value);
}

export function maxlength(length) {
  return attribute("maxlength", $int.to_string(length));
}

export function min(value) {
  return attribute("min", value);
}

export function minlength(length) {
  return attribute("minlength", $int.to_string(length));
}

export function multiple(allow_multiple) {
  return boolean_attribute("multiple", allow_multiple);
}

export function name(element_name) {
  return attribute("name", element_name);
}

export function pattern(regex) {
  return attribute("pattern", regex);
}

export function placeholder(text) {
  return attribute("placeholder", text);
}

export function popovertarget(id) {
  return attribute("popovertarget", id);
}

export function popovertargetaction(action) {
  return attribute("popovertargetaction", action);
}

export function readonly(is_readonly) {
  return boolean_attribute("readonly", is_readonly);
}

export function required(is_required) {
  return boolean_attribute("required", is_required);
}

export function selected(is_selected) {
  return boolean_attribute("selected", is_selected);
}

export function size(value) {
  return attribute("size", value);
}

export function step(value) {
  return attribute("step", value);
}

export function type_(control_type) {
  return attribute("type", control_type);
}

export function value(control_value) {
  return attribute("value", control_value);
}

export function default_value(control_value) {
  return attribute("virtual:defaultValue", control_value);
}

export function http_equiv(value) {
  return attribute("http-equiv", value);
}

export function content(value) {
  return attribute("content", value);
}

export function charset(value) {
  return attribute("charset", value);
}

export function media(query) {
  return attribute("media", query);
}

export function autoplay(auto_play) {
  return boolean_attribute("autoplay", auto_play);
}

export function controls(show_controls) {
  return boolean_attribute("controls", show_controls);
}

export function loop(should_loop) {
  return boolean_attribute("loop", should_loop);
}

export function muted(is_muted) {
  return boolean_attribute("muted", is_muted);
}

export function playsinline(play_inline) {
  return boolean_attribute("playsinline", play_inline);
}

export function poster(url) {
  return attribute("poster", url);
}

export function preload(value) {
  return attribute("preload", value);
}

export function shadowrootmode(mode) {
  return attribute("shadowrootmode", mode);
}

export function shadowrootdelegatesfocus(delegates) {
  return boolean_attribute("shadowrootdelegatesfocus", delegates);
}

export function shadowrootclonable(clonable) {
  return boolean_attribute("shadowrootclonable", clonable);
}

export function shadowrootserializable(serializable) {
  return boolean_attribute("shadowrootserializable", serializable);
}

export function abbr(value) {
  return attribute("abbr", value);
}

export function colspan(value) {
  return attribute("colspan", $int.to_string(value));
}

export function headers(ids) {
  return attribute("headers", $string.join(ids, " "));
}

export function rowspan(value) {
  return attribute(
    "rowspan",
    (() => {
      let _pipe = value;
      let _pipe$1 = $int.max(_pipe, 0);
      let _pipe$2 = $int.min(_pipe$1, 65_534);
      return $int.to_string(_pipe$2);
    })(),
  );
}

export function span(value) {
  return attribute("span", $int.to_string(value));
}

export function scope(value) {
  return attribute("scope", value);
}

export function aria(name, value) {
  return attribute("aria-" + name, value);
}

export function role(name) {
  return attribute("role", name);
}

export function aria_activedescendant(id) {
  return aria("activedescendant", id);
}

export function aria_atomic(value) {
  return aria(
    "atomic",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_autocomplete(value) {
  return aria("autocomplete", value);
}

export function aria_braillelabel(value) {
  return aria("braillelabel", value);
}

export function aria_brailleroledescription(value) {
  return aria("brailleroledescription", value);
}

export function aria_busy(value) {
  return aria(
    "busy",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_checked(value) {
  return aria("checked", value);
}

export function aria_colcount(value) {
  return aria("colcount", $int.to_string(value));
}

export function aria_colindex(value) {
  return aria("colindex", $int.to_string(value));
}

export function aria_colindextext(value) {
  return aria("colindextext", value);
}

export function aria_colspan(value) {
  return aria("colspan", $int.to_string(value));
}

export function aria_controls(value) {
  return aria("controls", value);
}

export function aria_current(value) {
  return aria("current", value);
}

export function aria_describedby(value) {
  return aria("describedby", value);
}

export function aria_description(value) {
  return aria("description", value);
}

export function aria_details(value) {
  return aria("details", value);
}

export function aria_disabled(value) {
  return aria(
    "disabled",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_errormessage(value) {
  return aria("errormessage", value);
}

export function aria_expanded(value) {
  return aria(
    "expanded",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_flowto(value) {
  return aria("flowto", value);
}

export function aria_haspopup(value) {
  return aria("haspopup", value);
}

export function aria_hidden(value) {
  return aria(
    "hidden",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_invalid(value) {
  return aria("invalid", value);
}

export function aria_keyshortcuts(value) {
  return aria("keyshortcuts", value);
}

export function aria_label(value) {
  return aria("label", value);
}

export function aria_labelledby(value) {
  return aria("labelledby", value);
}

export function aria_level(value) {
  return aria("level", $int.to_string(value));
}

export function aria_live(value) {
  return aria("live", value);
}

export function aria_modal(value) {
  return aria(
    "modal",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_multiline(value) {
  return aria(
    "multiline",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_multiselectable(value) {
  return aria(
    "multiselectable",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_orientation(value) {
  return aria("orientation", value);
}

export function aria_owns(value) {
  return aria("owns", value);
}

export function aria_placeholder(value) {
  return aria("placeholder", value);
}

export function aria_posinset(value) {
  return aria("posinset", $int.to_string(value));
}

export function aria_pressed(value) {
  return aria("pressed", value);
}

export function aria_readonly(value) {
  return aria(
    "readonly",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_relevant(value) {
  return aria("relevant", value);
}

export function aria_required(value) {
  return aria(
    "required",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_roledescription(value) {
  return aria("roledescription", value);
}

export function aria_rowcount(value) {
  return aria("rowcount", $int.to_string(value));
}

export function aria_rowindex(value) {
  return aria("rowindex", $int.to_string(value));
}

export function aria_rowindextext(value) {
  return aria("rowindextext", value);
}

export function aria_rowspan(value) {
  return aria("rowspan", $int.to_string(value));
}

export function aria_selected(value) {
  return aria(
    "selected",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

export function aria_setsize(value) {
  return aria("setsize", $int.to_string(value));
}

export function aria_sort(value) {
  return aria("sort", value);
}

export function aria_valuemax(value) {
  return aria("valuemax", value);
}

export function aria_valuemin(value) {
  return aria("valuemin", value);
}

export function aria_valuenow(value) {
  return aria("valuenow", value);
}

export function aria_valuetext(value) {
  return aria("valuetext", value);
}
