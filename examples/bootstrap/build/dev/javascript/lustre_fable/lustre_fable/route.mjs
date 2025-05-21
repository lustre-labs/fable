import * as $uri from "../../gleam_stdlib/gleam/uri.mjs";
import * as $attribute from "../../lustre/lustre/attribute.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class Index extends $CustomType {}

export class Chapter extends $CustomType {
  constructor(chapter) {
    super();
    this.chapter = chapter;
  }
}

export class Story extends $CustomType {
  constructor(chapter, story) {
    super();
    this.chapter = chapter;
    this.story = story;
  }
}

export class NotFound extends $CustomType {}

export function parse(uri) {
  let $ = $uri.path_segments(uri.path);
  if ($.hasLength(0)) {
    return new Index();
  } else if ($.hasLength(2) && $.head === "chapter") {
    let chapter = $.tail.head;
    return new Chapter(chapter);
  } else if ($.hasLength(4) &&
  $.head === "chapter" &&
  $.tail.tail.head === "story") {
    let chapter = $.tail.head;
    let story = $.tail.tail.tail.head;
    return new Story(chapter, story);
  } else {
    return new NotFound();
  }
}

export function href(route) {
  return $attribute.href(
    (() => {
      if (route instanceof Index) {
        return "/";
      } else if (route instanceof Chapter) {
        let chapter = route.chapter;
        return "/chapter/" + chapter;
      } else if (route instanceof Story) {
        let chapter = route.chapter;
        let story = route.story;
        return (("/chapter/" + chapter) + "/story/") + story;
      } else {
        return "#";
      }
    })(),
  );
}
