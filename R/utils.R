#' @importFrom crayon "magenta"
#' @importFrom crayon "cyan"
#' @importFrom crayon "green"
#' @importFrom crayon "black"
#' @importFrom crayon "yellow"
#' @importFrom crayon "white"
#' @importFrom crayon "italic"
#' @importFrom crayon "red"
#' @importFrom crayon "bold"
#' @importFrom crayon "blue"
#' @importFrom crayon "bgRed"

# TODO sorting methods

utils::globalVariables(c("x2", "p"))


pad_vector <- function(x, max) {
  if (length(x) < max) {
    diff <- max - length(x)
    c(x, rep("", diff))
  } else if (length(x) > max) {
    c(x[1:max - 1L], "...")
  } else {
    x 
  }
}




equalize_chr_length <- function(x, max_chr_length) {
  dplyr::tibble(x) %>%
    dplyr::mutate(
      n = nchar(x),
      pad = 3 + max_chr_length - nchar(x),
      x2 = dplyr::case_when(
        # TODO fix this...
        nchar(x) > max_chr_length ~ paste0(substr(x, 0, max_chr_length), "..."),
        TRUE ~ paste0(x, stringr::str_pad(as.character(""), width = pad, side = "right", pad = " "))
      )
    ) %>%
    dplyr::pull(x2)
}


thm1 <- red$ italic
thm2 <- cyan$ italic
thm3 <- blue$ italic
thm4 <- magenta $italic

thm1_title <- red$ italic$ underline$ bold
thm2_title <- cyan$ italic$ underline$ bold
thm3_title <- blue$ italic$ underline$ bold
thm4_title <- magenta $ italic $underline $bold
wd <- green$ bold$ italic
vertline <- black$ bold

gi <- cyan$ italic$ bold
gb <- green$ bold
# db_remove_rows
rw <- bgRed$ white$ italic # rows or table names
ri <- red$ italic$ bold # the field we call out
mt <- blue # the main theme


cd_helper <- function() {
  glue::glue_col(
    "{gb

Print current working directory with:

    {gi cd('')}

Move with:

    {gi cd('updir')}

Jump down with:

    {gi cd('..')}


Go to your root directory with:

    {gi cd('~')}

}"
  )
}

# ------

.onLoad <- function(libname = find.package("bwmisc"),
                    pkgname = "bwmisc")
  

options(bwmisc.global =  list(
    max_files =  11, 
    max_nchar = 20
  ))


# .onLoad <- function(libname = find.package("highcharter"),
#                     pkgname = "highcharter") {
#
#   options(
#     highcharter.global = list(
#       Date = NULL,
#       VMLRadialGradientURL =
#         "http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png",
#       canvasToolsURL =
#         "http =//code.highcharts.com/list(version)/modules/canvas-tools.js",
#       getTimezoneOffset = NULL,
#       timezoneOffset = 0,
#       useUTC = TRUE
#     )
#   )
#
#   options(
#     highcharter.lang = list(
#       contextButtonTitle = "Chart context menu",
#       decimalPoint = ".",
#       downloadJPEG = "Download JPEG image",
#       downloadPDF = "Download PDF document",
#       downloadPNG = "Download PNG image",
#       downloadSVG = "Download SVG vector image",
#       drillUpText = "Back to {series.name}",
#       invalidDate = NULL,
#       loading = "Loading...",
#       months = c("January", "February", "March", "April",
#                  "May", "June", "July", "August",
#                  "September", "October", "November", "December"),
#       noData = "No data to display",
#       numericSymbols = c("k", "M", "G", "T", "P", "E"),
#       printChart = "Print chart",
#       resetZoom = "Reset zoom",
#       resetZoomTitle = "Reset zoom level 1:1",
#       shortMonths = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
#                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#       thousandsSep = " ",
#       weekdays = c("Sunday", "Monday", "Tuesday", "Wednesday",
#                    "Thursday", "Friday", "Saturday")
#     )
#   )
#
#   options(
#     highcharter.chart = list(
#       title = list(
#         text = NULL
#       ),
#       yAxis = list(
#         title = list(
#           text = NULL
#         )
#       ),
#       credits = list(
#         enabled = FALSE
#       ),
#       exporting = list(
#         enabled = FALSE
#       ),
#       plotOptions = list(
#         series = list(
#           # start disabled series-label.js module https://api.highcharts.com/highcharts/plotOptions.series.label
#           label = list(enabled = FALSE),
#           turboThreshold = 0
#         ),
#         treemap = list(layoutAlgorithm = "squarified")
#       )
#       # accessibility = list(
#       #   enabled = TRUE,
#       #   keyboardNavigation = list(
#       #     enabled = TRUE
#       #     )
#       #   )
#     )
#   )
#
#   options(
#     highcharter.theme = hc_theme(chart = list(backgroundColor = "transparent")),
#     highcharter.verbose = FALSE,
#     highcharter.debug = FALSE,
#     highcharter.download_map_data = TRUE,
#     # gsub("FF$", "", viridisLite::viridis(10, option = "B", begin = 0.2))
#     highcharter.color_palette = c("#420A68", "#66166E", "#8B226A", "#AE305C", "#CF4446",
#                                   "#E8602C", "#F8850F", "#FCAF13", "#F5DC4D", "#FCFFA4")
#   )
# }
