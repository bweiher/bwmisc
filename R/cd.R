
#' Navigate directories and print contents
#' @export
#' @param x Your directory or empty string
#' @param ... Other arguments passed to methods

cd <- function(x, ...) {
  if (missing(x)) {
    cd_helper()
  } else if (grepl("\\..", x)) { # fine
    setwd("..")
    dprint(...)
  } else if (grepl("~", x)) { # fine
    setwd("~")
    dprint(...)
  } else if (grepl(pattern = "^[A-Z]{1-4}:", x = x)) { # fine, new HD
    setwd(x)
    dprint(...)
  } else {
    newdir <- paste0(getwd(), "/", x, "/") # bugged ?
    setwd(newdir)
    dprint(...)
  }
}
