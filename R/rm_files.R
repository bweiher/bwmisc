#' Create a new file
#'
#' Just a wrapper for file.create or dir.create
#'
#' @export
#' @param x a file (or vector of them) to delete
rm_files <- function(x) {
  if (stringr::str_detect(x, "\\.")) {
    file.remove(x)
  } else {
    unlink(x)
  }
}
