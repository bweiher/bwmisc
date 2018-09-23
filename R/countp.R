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

  if (dplyr::is.grouped_df(df)) { 
    df %>% 
      dplyr::count() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(n =1.0 * n / sum(n))
  } else {
    df %>% 
      dplyr::count(!!col_q) %>% 
      dplyr::mutate(
        p = 1.0 * n / sum(n)
      )
  }
}
