#' Returns a list of files
#'
#'
#' The list is formatted by the object type it is interpreted as
#'
#'
#'
#' @param show_hidden Whether to show hidden files
#' @param filter_groups Filter for the groups not the length of them


return_files_in_wd <- function(show_hidden = FALSE, filter_groups = FALSE) {
  files <- list.files(all.files = show_hidden)

  
  other_files_reg <- "\\.r$|\\.rmd$|\\.csv|\\.feather"
  r_files <- stringr::str_subset(stringr::str_to_lower(files), "\\.r$|\\.rmd") %>% paste(sep = "\n")
  dirs <- files[!stringr::str_detect(files, "\\.")] %>% paste(sep = "\n")
  other_files <- stringr::str_subset(files, "\\.") %>% stringr::str_to_lower() %>% .[!stringr::str_detect(.,other_files_reg)]
  data_files <-  stringr::str_to_lower(files) %>%  stringr::str_subset("\\.csv|\\.feather\\.Rdata")

  
  cols_included <- 4L

  if (filter_groups == TRUE) {
    list(r_files, dirs, data_files, other_files)
  } else {
    list(cols_included, r_files, dirs, data_files, other_files)
  }
}
