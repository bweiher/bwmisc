#' Like from data.table
#'
#'
#'
#'
#' @export
#' @param vector lhs
#' @param pattern pattern

"%like%" <- function (vector, pattern)  {
  if (is.factor(vector)) {
    as.integer(vector) %in% grep(pattern, levels(vector))
  }
  else {
    grepl(pattern, vector)
  }
}