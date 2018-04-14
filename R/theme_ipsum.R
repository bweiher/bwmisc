
#' An attempted port of `theme_ipsum` into `Highcharter`
#'
#'
#' Supplies a new theme inspired by theme_ipsum
#'
#'
#' @export
#' @param ... arguments passed to methods


theme_ipsum <- function(...) {
  theme <- highcharter::hc_theme(
    # ipsum colors
    colors = c(
      "#d18975", "#8fd175", "#3f2d54", "#75b8d1",
      "#2d543d", "#c9d175", "#d1ab75", "#d175b8",
      "#758bd1"
    ),

    chart = list(
      style = list(
        fontFamily = "Arial Narrow",
        color = "#666666"
      )
    ),
    title = list(
      align = "left",
      style = list(
        fontFamily = "Arial Narrow",
        fontWeight = "bold",
        fontSize = "19px"
      )
    ),
    subtitle = list(
      align = "left",
      style = list(
        fontFamily = "Arial Narrow",
        fontSize = "15px"
      )
    ),
    legend = list(
      align = "right",
      verticalAlign = "bottom"
    ),
    xAxis = list(
      gridLineWidth = 1,
      gridLineColor = "#F3F3F3",
      lineColor = "#F3F3F3",
      minorGridLineColor = "#F3F3F3",
      tickColor = "#F3F3F3",
      tickWidth = 1,
      title = list(align = "high")
    ),
    yAxis = list(
      gridLineColor = "#F3F3F3",
      lineColor = "#F3F3F3",
      minorGridLineColor = "#F3F3F3",
      tickColor = "#F3F3F3",
      tickWidth = 1,
      title = list(align = "high")
    ),
    plotOptions = list(
      line = list(
        marker = list(enabled = FALSE)
      ),
      spline = list(
        marker = list(enabled = FALSE)
      ),
      area = list(
        marker = list(enabled = FALSE)
      ),
      areaspline = list(
        marker = list(enabled = FALSE)
      ),
      arearange = list(
        marker = list(enabled = FALSE)
      ),
      bubble = list(
        maxSize = "10%"
      )
    )
  )

  theme <- structure(theme, class = "hc_theme")

  if (length(list(...)) > 0) {
    theme <- highcharter::hc_theme_merge(
      theme,
      highcharter::hc_theme(...)
    )
  }

  theme
}
