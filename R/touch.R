#' Create a new file
#'
#' Just a wrapper for file.create or dir.create
#'
#' @export
#' @param x a file to create
touch <- function(x) {
  if (x %in% list.files()) {
    message("File already exists!")
  } else {
    if (stringr::str_detect(x, "\\.")) {
      invisible(file.create(x))
      th <- "File"
    } else {
      dir.create(x)
      th <- "Directory"
    }

    glue::glue_col("
{gi {th} Created:}
{red {x} }
                   ")
  }
}
