#' Returns a list of files
#'
#'
#' The list is formatted by the object type it is interpreted as
#'
#'
#'
#' @param show_hidden Whether to show hidden files


return_files_in_wd <- function(show_hidden = FALSE) {
  files <- list.files(all.files = show_hidden)

  r_files <- stringr::str_subset(stringr::str_to_lower(files), "\\.r$|\\.rmd") %>% paste(sep = "\n")
  dirs <- files[!stringr::str_detect(files, "\\.")] %>% paste(sep = "\n")
  other_files <- stringr::str_subset(files, "\\.") %>% stringr::str_to_lower() %>% .[!stringr::str_detect(., "\\.r$|\\.rmd$")]

  cols_included <- 3L

  list(cols_included, r_files, dirs, other_files)
}
