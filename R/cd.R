
#' Navigate directories and print contents
#' @export
#' @param x Your directory or empty string
#' @param ... Other arguments passed to methods

cd <- function(x, ...) {
  if (missing(x)) {
    glue::glue_col(
      "{gb
Print current working directory with:
{gi cd('')}

Move with:
{gi cd('newdir')}

Jump down with:
{gi cd('..')}

Go to your root directory with:
{gi cd('~')}
      }"
    )
  } else if (grepl("\\..", x)) {
    setwd("..")
    dprint(...)
  } else if (grepl("~", x)) {
    setwd("~")
    dprint(...)
  } else if (grepl(pattern = "^[A-Z]{1-4}:", x = x)) {
    setwd(x)
    dprint(...)
  } else {
    setwd(paste(getwd(), "/", x, sep = ""))
    dprint(...)
  }
}
