#' Create a new file
#'
#' Just a wrapper for file.create or dir.create
#'
#' @export
#' @param x a file to create
touch <- function(x) {
  if(stringr::str_detect(x, "\\.")) file.create(x)
  else dir.create(x)
}
