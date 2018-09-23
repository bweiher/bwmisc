#' Prints the directories of your current wd
#'
#'
#'
#'
#' @export
#' @param max_files The maximum number of files to show per bin
#' @param max_nchar The maximum length of a file name to show
#' @param ... Other arguments passed to methods

dprint <- function(max_files = 11, max_nchar = 20, ...) {
  pwd <- glue::glue_col("
{wd
                        Your current working directory is {base::getwd()}}
                        ")

  files <- return_files_in_wd(...)
  cols_included <- files[[1]]
  files <- files[2:length(files)]

  if (length(files) > 0) {
    file_lengths <- files %>% purrr::map_dbl(~length(.))

    nvec <- double(length = cols_included)
    for (g in seq_along(file_lengths)) {
      if (file_lengths[g] > 0) {
        nvec[[g]] <- g
      }
    }

    # find max no of files within each bucket.
    max_file_length <- files %>%
      purrr::map_dbl(~length(.)) %>%
      max()

    # find whats greater, whats specificied in function arg or present in this dir
    max_file_length <- ifelse(max_file_length > max_files, max_files, max_file_length)


    # if it exceeds max as defined in function args, replace with whats specified
    files <- files %>% purrr::map(~pad_vector(., max = max_file_length))

    # find the maximum length of each element in each vector
    max_chr_length <- files %>%
      purrr::map(~nchar(.)) %>%
      unlist() %>%
      max(., na.rm = TRUE)



    max_chr_length <- ifelse(
      max_chr_length > max_nchar, max_nchar, max_chr_length
    )

    files <- purrr::map(files, ~equalize_chr_length(., max_chr_length = max_chr_length))

    title_list <- list(
      "Scripts" = equalize_chr_length("Scripts", max_chr_length = max_chr_length),
      "Dirs" = equalize_chr_length("Dirs", max_chr_length = max_chr_length),
      "Data" = equalize_chr_length("Data", max_chr_length = max_chr_length),
      "Misc" = equalize_chr_length("Misc", max_chr_length = max_chr_length)
    )


    title_list_chr <- character(length = cols_included)
    file_list_chr <- character(length = cols_included)

    for (g in seq_along(title_list)) {
      if (nvec[g] > 0) {
        title_list_chr[[g]] <- paste0("{thm", g, "_title {title_list[[", g, "]]}}")
        file_list_chr[[g]] <- paste0("{thm", g, paste0(" {"), glue::glue(" files[[{g}]]"), "}}") # ··
      }
    }

    file_list_chr <- file_list_chr[nchar(file_list_chr) != 0]
    title_list_chr <- title_list_chr[nchar(title_list_chr) != 0]

    p1 <- glue::glue_col(paste(title_list_chr, collapse = "   "))
    middle <- rep("|", max_file_length)
    p2 <- glue::glue_col(paste(file_list_chr, collapse = " {vertline {middle}} "))
    files <- files[nvec[nvec > 0]] # filter them out
    print(pwd, p1, p2)
  } else {
    print(pwd)
  }
}
