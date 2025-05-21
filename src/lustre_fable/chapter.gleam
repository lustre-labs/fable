import gleam/list
import gleam/result
import lustre
import lustre_fable/story.{type Story, type StoryConfig}

pub type Chapter {
  Chapter(title: String, stories: List(Story))
}

///
///
pub fn init(
  title: String,
  stories: List(StoryConfig),
  stylesheets: List(String),
  external_stylesheets: List(String),
) -> Result(Chapter, lustre.Error) {
  stories
  |> list.try_map(story.register(_, stylesheets, external_stylesheets))
  |> result.map(Chapter(title:, stories: _))
}

///
///
pub fn story(chapter: Chapter, slug: String) -> Result(Story, Nil) {
  use story <- list.find(chapter.stories)

  story.route == slug
}
