#' Counts rows and calculates proportions
#'
#'
#'
#'
#' @export
#' @param df a dataframe
#' @param col column to count on
#' @param ... other arguments passed on to methods

countp <- function(df, col=NULL, ...) {
  
  if (is.null(col)) { 
    
    # if is grouped, calc prop by n-groups
    if(dplyr::is.grouped_df(df)){
      df %>% 
        dplyr::count(...) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(n = 1.0 * n / sum(n))
    } else {
      # col is NULL and is not grouped  
      dplyr::count(df,...)
    }
    
  } else {
    # col is NOT NULL
    
    # TODO think about if we need groups?
    col_q <- dplyr::enquo(col)
    df %>% 
      dplyr::count(!!col_q, ...) %>% 
      dplyr::mutate(
        p = 1.0 * n / sum(n)
      )
  }
}

