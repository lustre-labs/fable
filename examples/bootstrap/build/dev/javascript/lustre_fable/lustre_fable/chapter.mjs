import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $lustre from "../../lustre/lustre.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $story from "../lustre_fable/story.mjs";

export class Chapter extends $CustomType {
  constructor(title, stories) {
    super();
    this.title = title;
    this.stories = stories;
  }
}

export function story(chapter, slug) {
  return $list.find(
    chapter.stories,
    (story) => { return story.route === slug; },
  );
}

export function init(title, stories, stylesheets, external_stylesheets) {
  let _pipe = stories;
  let _pipe$1 = $list.try_map(
    _pipe,
    (_capture) => {
      return $story.register(_capture, stylesheets, external_stylesheets);
    },
  );
  return $result.map(
    _pipe$1,
    (_capture) => { return new Chapter(title, _capture); },
  );
}
