#' Prints the directories of your current wd
#'
#'
#'
#'
#' @export
#' @param max_files The maximum number of files to show per bin
#' @param max_nchar The maximum length of a file name to show
#' @param show_hidden Whether to show hidden files

dprint <- function(max_files=11, max_nchar=20, show_hidden = FALSE) {

  # TODO check that the max chr length needs to be identical for diff cols
  files <- list.files(all.files = show_hidden)
  pwd <- glue::glue_col("
{wd
                        Your current working directory is {getwd()}}


                        ")

  if (length(files) > 0) {

    # find R scripts, directories, and other file types
    # TODO hidden file types option
    r_files <- stringr::str_subset(stringr::str_to_lower(files), "\\.r$|\\.rmd") %>% paste(sep = "\n")
    dirs <- files[!stringr::str_detect(files, "\\.")] %>% paste(sep = "\n")
    other_files <- stringr::str_subset(files, "\\.") %>% stringr::str_to_lower() %>% .[!stringr::str_detect(., "\\.r$|\\.rmd$")]

    cols_included <- 3L # TODO update if changed above

    # find out which files are actually present inside this wd
    files <- list(r_files, dirs, other_files)
    file_lengths <- files %>% purrr::map_dbl(~ length(.))

    nvec <- double(length = cols_included)
    for (g in seq_along(file_lengths)) {
      if (file_lengths[g] > 0) {
        nvec[[g]] <- g
      }
    }

    # find max no of files within each bucket.
    max_file_length <- files %>%
      purrr::map_dbl(~ length(.)) %>%
      max()

    # find whats greater, whats specificied in function arg or present in this dir
    max_file_length <- ifelse(max_file_length > max_files, max_files, max_file_length)


    # if it exceeds max as defined in function args, replace with whats specified
    files <- files %>% purrr::map(~ pad_vector(., max = max_file_length))

    # find the maximum length of each element in each vector
    max_chr_length <- files %>%
      purrr::map(~ nchar(.)) %>%
      unlist() %>%
      max(., na.rm = TRUE)

    max_chr_length <- ifelse(
      max_chr_length > max_nchar, max_nchar, max_chr_length
    )

    files <- purrr::map(files, ~ equalize_chr_length(., max_chr_length = max_chr_length))

    title_list <- list(
      "title1" = equalize_chr_length("R Scripts", max_chr_length = max_chr_length),
      "title2" = equalize_chr_length("Directories", max_chr_length = max_chr_length),
      "title3" = equalize_chr_length("Other Files", max_chr_length = max_chr_length)
    )


    title_list_chr <- character(length = cols_included)
    file_list_chr <- character(length = cols_included)

    for (g in seq_along(title_list)) {
      if (nvec[g] > 0) {
        name <- names(title_list[g])
        title_list_chr[[g]] <- paste0("{thm", g, "_title {title_list[[", g, "]]}}")
        file_list_chr[[g]] <- paste0("{thm", g, paste0(" {"), glue::glue(" files[[{g}]]"), "}}")
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
