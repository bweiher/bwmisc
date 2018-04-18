#' Move some files or rename them
#'
#' Just a wrapper for file.rename
#'
#' @export
#' @param x files or dirs that you want to move
#' @param dir_to directory/filename to move files to
move <- function(x, dir_to) {
  if (file.exists(x)) {
    chr_length <- character(length = length(x))
    for (g in seq_along(x)) {
      where <- glue::glue("{dir_to}/{x[g]}")
      invisible(file.rename(from = x, to = where))
    }
  } else {
    message("File didn't exist")
  }
}
