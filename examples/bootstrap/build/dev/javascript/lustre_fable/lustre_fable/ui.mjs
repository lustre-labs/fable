import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $attribute from "../../lustre/lustre/attribute.mjs";
import * as $element from "../../lustre/lustre/element.mjs";
import * as $html from "../../lustre/lustre/element/html.mjs";
import { toList } from "../gleam.mjs";
import * as $chapter from "../lustre_fable/chapter.mjs";
import * as $route from "../lustre_fable/route.mjs";

export function breadcrumb(route, chapters) {
  let separator = $html.span(
    toList([$attribute.class$("text-stone-400 user-select-none")]),
    toList([$html.text("/")]),
  );
  let link = (route, label) => {
    return $html.a(toList([$route.href(route)]), toList([$html.text(label)]));
  };
  let current = (label) => {
    return $html.a(
      toList([$attribute.href("#"), $attribute.aria_current("page")]),
      toList([$html.text(label)]),
    );
  };
  return $html.nav(
    toList([$attribute.class$("flex gap-4")]),
    (() => {
      if (route instanceof $route.Index) {
        return toList([]);
      } else if (route instanceof $route.NotFound) {
        return toList([]);
      } else if (route instanceof $route.Chapter) {
        let chapter = route.chapter;
        let $ = $dict.get(chapters, chapter);
        if (!$.isOk()) {
          return toList([]);
        } else {
          let c = $[0];
          return toList([separator, current(c.title)]);
        }
      } else {
        let chapter = route.chapter;
        let story = route.story;
        let c = $dict.get(chapters, chapter);
        let _block;
        let _pipe = c;
        _block = $result.then$(
          _pipe,
          (_capture) => { return $chapter.story(_capture, story); },
        );
        let s = _block;
        if (c.isOk() && s.isOk()) {
          let c$1 = c[0];
          let s$1 = s[0];
          return toList([
            separator,
            link(new $route.Chapter(chapter), c$1.title),
            separator,
            current(s$1.title),
          ]);
        } else if (c.isOk()) {
          let c$1 = c[0];
          return toList([separator, current(c$1.title)]);
        } else {
          return toList([]);
        }
      }
    })(),
  );
}
