#' Counts rows and calculates proportions
#'
#'
#'
#'
#' @export
#' @param df a dataframe
#' @param col column to count on
#' @param ... other arguments passed on to methods

countp <- function(df, col, ...) {
  col_q <- dplyr::enquo(col)

  df <- dplyr::count(df, !!col_q, ...) %>%
    dplyr::mutate(
      p = 1.0 * n / sum(n)
    )

  if (dplyr::is.grouped_df(df)) { # TODO get smarter
    df
  } else {
    dplyr::arrange(df, dplyr::desc(p))
  }
}
