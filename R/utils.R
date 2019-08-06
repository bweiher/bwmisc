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
#' @useDynLib bwmisc
#' @importFrom Rcpp sourceCpp
NULL
# TODO sorting methods

utils::globalVariables(c("x2", "p"))


typeof_safe <- function(x) {
  if (typeof(x) == "double" && class(x) %in% c("POSIXct", "POSIXt", "Date") |
      typeof(x) == "integer" && class(x) == 'factor') {
    "character"
  } else {
    typeof(x)
  }
}


switch_x <- function(x) switch(x,
                               "character" = "STRING",
                               "logical" = "BOOLEAN",
                               "double" = "DOUBLE",
                               "integer" = "INT"
)


switch_to_hive <- function(x) {
  len <- length(x)
  if (len > 1) {
    chr <- character(length = len)
    for (g in seq_along(x)) {
      chr[g] <- switch_x(x[[g]])
    }
    chr
  } else {
    switch_x(x)
  }
}

equalize_chr_vector <- function(chr_vector) {
  stringr::str_pad(string = chr_vector, width = max(nchar(chr_vector)), pad = " ", side = "right")
}


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


# colors and themes from crayon
thm1 <- red$ italic
thm2 <- cyan$ italic
thm3 <- blue$ italic
thm4 <- magenta $italic
bi <- bold$italic$green
thm1_title <- red$ italic$ underline$ bold
thm2_title <- cyan$ italic$ underline$ bold
thm3_title <- blue$ italic$ underline$ bold
thm4_title <- magenta $ italic $underline $bold
wd <- magenta$ bold$ italic $ bgWhite
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

 
