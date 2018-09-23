#' Create a new file
#'
#' Just a wrapper for file.create or dir.create
#'
#' @export
#' @param x a file (or vector of them) to delete
rm_files <- function(x) {
  if (sum(stringr::str_detect(x, "\\.")) > 0) {
    file.remove(x)
  } else {
    unlink(x,recursive = TRUE)
  }
  
  glue::glue_col("
                   {red Deleted:}
                   {gi {  glue::glue_collapse(x,sep = ', ')}}
                   ")
  
}

