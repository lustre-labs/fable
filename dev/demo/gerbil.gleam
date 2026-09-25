// IMPORTS ---------------------------------------------------------------------

import fable.{type Story}
import gerbil/axis
import gerbil/chart.{type Chart}
import gleam/float
import gleam/function
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/time/duration
import gleam/time/timestamp
import gleam_community/maths
import lustre/element.{type Element}
import lustre/element/html

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let book =
    fable.book("Gerbil", [
      fable.chapter("plot", [
        points_plot_story(),
        line_plot_story(),
        vertical_bars_story(),
        horizontal_bars_story(),
        vertical_bands_story(),
        horizontal_bands_story(),
      ]),
      fable.chapter("ticks", [
        float_tick_inference_story(),
      ]),
      fable.chapter("labels", [
        date_labels_story(),
      ]),
    ])

  let assert Ok(_) = fable.start(book)
}

// TICK STORIES ----------------------------------------------------------------

type InferredTicksStoryModel {
  PreferredCount(count: Int)
}

fn float_tick_inference_story() -> Story {
  let view = fn(model) {
    let PreferredCount(count:) = model
    let plot =
      chart.points([], {
        use points, i <- int.range(280, 0, [])
        let x = int.to_float(i) /. 10.0
        let point = chart.point(x, maths.cos(x), [])

        [point, ..points]
      })

    chart.new(x: axis.float() |> axis.infer_ticks(count), y: axis.float())
    |> chart.add(plot)
    |> chart_view
  }

  fable.static_story("float tick inference", view, [
    fable.scene("preferred count: 5", PreferredCount(5)),
    fable.scene("preferred count: 10", PreferredCount(10)),
    fable.scene("preferred count: 20", PreferredCount(20)),
  ])
}

type DateLabelsStoryModel {
  SpanningWeek
  SpanningMonths
  SpanningYears
}

fn date_labels_story() -> Story {
  let view = fn(model) {
    let points = case model {
      SpanningWeek -> [0, 1, 2, 3, 4, 5, 6]
      SpanningMonths -> [0, 50, 100, 150, 201, 268, 365]
      SpanningYears -> [0, 365, 503, 601, 811, 1200]
    }

    let plot =
      chart.line([], {
        use point <- list.map(points)
        let #(date, _) =
          timestamp.unix_epoch
          |> timestamp.add(duration.hours(24 * point))
          |> timestamp.to_calendar(duration.hours(2))

        chart.point(
          date,
          maths.tan(int.to_float(point))
            |> float.round
            |> int.absolute_value
            |> int.multiply(point * point),
          [],
        )
      })

    chart.new(x: axis.date(), y: axis.int() |> axis.labels_gutter(60))
    |> chart.add(plot)
    |> chart_view
  }

  fable.static_story("date labels", view, [
    fable.scene("spanning a week", SpanningWeek),
    fable.scene("spanning months", SpanningMonths),
    fable.scene("spanning years", SpanningYears),
  ])
}

// BASIC PLOT STORIES ----------------------------------------------------------

fn points_plot_story() -> Story {
  let view = fn(points) {
    let plot = chart.points([], points)

    chart.new(x: axis.float(), y: axis.float())
    |> chart.add(plot)
    |> chart_view
  }

  fable.static_story("points", view, [
    fable.scene("scene", [
      chart.point(1.0, 1.0, []),
      chart.point(1.2, 1.1, []),
      chart.point(2.0, 0.8, []),
      chart.point(1.3, 0.9, []),
      chart.point(1.35, 0.92, []),
    ]),
  ])
}

type LinePlotStoryMode {
  JustLine
  LineAndPoints
}

fn line_plot_story() -> Story {
  let points = [
    chart.point(1.0, 1.12, []),
    chart.point(1.2, 1.14, []),
    chart.point(1.3, 1.05, []),
    chart.point(1.4, 0.95, []),
    chart.point(1.6, 0.93, []),
    chart.point(2.0, 0.9, []),
  ]

  let view = fn(model) {
    let chart = case model {
      LineAndPoints ->
        chart.new(x: axis.float(), y: axis.float())
        |> chart.add(chart.line([], points))
        |> chart.add(chart.points([], points))

      JustLine ->
        chart.new(x: axis.float(), y: axis.float())
        |> chart.add(chart.line([], points))
    }

    chart_view(chart)
  }

  fable.static_story("line", view, [
    fable.scene("line", JustLine),
    fable.scene("line and points", LineAndPoints),
  ])
}

type BarsStoryMode {
  StartingAtTheSameValue
  StartingAtDifferentValues
}

fn vertical_bars_story() -> Story {
  let view = fn(model) {
    let plot =
      chart.vertical_bars([], case model {
        StartingAtTheSameValue -> [
          chart.bar("gerbil", 0, 20, []),
          chart.bar("mouse", 0, 9, []),
          chart.bar("nutria", 0, 13, []),
          chart.bar("beaver", 0, 14, []),
          chart.bar("capybara", 0, 16, []),
        ]

        StartingAtDifferentValues -> [
          chart.bar("gerbil", 10, 20, []),
          chart.bar("mouse", 0, 9, []),
          chart.bar("nutria", 5, 13, []),
          chart.bar("beaver", 2, 14, []),
          chart.bar("capybara", 7, 16, []),
        ]
      })

    chart.new(
      x: axis.categorical() |> axis.show_labels(function.identity),
      y: axis.int(),
    )
    |> chart.add(plot)
    |> chart_view
  }

  fable.static_story("vertical bars", view, [
    fable.scene("starting at the same value", StartingAtTheSameValue),
    fable.scene("starting at different values", StartingAtDifferentValues),
  ])
}

fn horizontal_bars_story() -> Story {
  let view = fn(model) {
    let plot =
      chart.horizontal_bars([], case model {
        StartingAtTheSameValue -> [
          chart.bar("gerbil", 0, 20, []),
          chart.bar("mouse", 0, 9, []),
          chart.bar("nutria", 0, 13, []),
          chart.bar("beaver", 0, 14, []),
          chart.bar("capybara", 0, 16, []),
        ]

        StartingAtDifferentValues -> [
          chart.bar("gerbil", 10, 20, []),
          chart.bar("mouse", 0, 9, []),
          chart.bar("nutria", 5, 13, []),
          chart.bar("beaver", 2, 14, []),
          chart.bar("capybara", 7, 16, []),
        ]
      })

    chart.new(
      x: axis.int(),
      y: axis.categorical() |> axis.show_labels(function.identity),
    )
    |> chart.add(plot)
    |> chart_view
  }

  fable.static_story("horizontal bars", view, [
    fable.scene("starting at the same value", StartingAtTheSameValue),
    fable.scene("starting at different values", StartingAtDifferentValues),
  ])
}

type BarStoryMode(a) {
  WithStartAndEnd(a, a)
  WithNoStart(a)
  WithNoEnd(a)
}

fn vertical_bands_story() -> Story {
  let view = fn(model) {
    let plot =
      chart.points([], [
        chart.point(0.0, 0.0, []),
        chart.point(1.0, 10.0, []),
        chart.point(2.0, 20.0, []),
        chart.point(3.0, 21.0, []),
        chart.point(4.0, 22.0, []),
        chart.point(5.0, 27.0, []),
        chart.point(6.0, 30.0, []),
        chart.point(7.0, 20.0, []),
      ])

    let bands = case model {
      WithStartAndEnd(start, end) ->
        chart.vertical_band(Some(start), Some(end), [])
      WithNoStart(end) -> chart.vertical_band(None, Some(end), [])
      WithNoEnd(start) -> chart.vertical_band(Some(start), None, [])
    }

    let chart =
      chart.new(x: axis.float(), y: axis.float())
      |> chart.add(plot)
      |> chart.add(bands)

    chart_view(chart)
  }

  fable.static_story("vertical bands", view, [
    fable.scene("with start and end", WithStartAndEnd(1.5, 5.5)),
    fable.scene("with no start", WithNoStart(5.5)),
    fable.scene("with no end", WithNoEnd(1.5)),
  ])
}

fn horizontal_bands_story() -> Story {
  let view = fn(model) {
    let plot =
      chart.points([], [
        chart.point(0.0, 0.0, []),
        chart.point(1.0, 10.0, []),
        chart.point(2.0, 20.0, []),
        chart.point(3.0, 21.0, []),
        chart.point(4.0, 22.0, []),
        chart.point(5.0, 27.0, []),
        chart.point(6.0, 30.0, []),
        chart.point(7.0, 20.0, []),
      ])

    let chart = case model {
      WithStartAndEnd(start, end) ->
        chart.new(x: axis.float(), y: axis.float())
        |> chart.add(plot)
        |> chart.add(chart.horizontal_band(Some(start), Some(end), []))

      WithNoStart(end) ->
        chart.new(x: axis.float(), y: axis.float())
        |> chart.add(plot)
        |> chart.add(chart.horizontal_band(None, Some(end), []))

      WithNoEnd(start) ->
        chart.new(x: axis.float(), y: axis.float())
        |> chart.add(plot)
        |> chart.add(chart.horizontal_band(Some(start), None, []))
    }

    chart_view(chart)
  }
  fable.static_story("horizontal bands", view, [
    fable.scene("with start and end", WithStartAndEnd(15.0, 25.0)),
    fable.scene("with no start", WithNoStart(25.0)),
    fable.scene("with no end", WithNoEnd(15.0)),
  ])
}

// UTILS -----------------------------------------------------------------------

fn chart_view(chart: Chart(a, b, c)) -> Element(c) {
  element.fragment([
    chart.to_svg(chart),
    html.style([], {
      "
      * { 
        font-family: system-ui, sans-serif; 
      }

      .gerbil {
        padding-block-start: 1rem;
        max-width: 80ch;
      }
      "
    }),
  ])
}
