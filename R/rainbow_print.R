
#' Print something in color
#' @export
#' @param x A string to print to console

rprint <- function(x) {
  cols <- c("cyan", "red", "blue", "green", "magenta", "yellow")
  string <- unlist(stringr::str_split(x, ""))

  for (g in seq_along(string)) {
    string[g] <- glue::glue("{sample(x=cols,  size=1)} {string[g]}") %>%
      paste0("{", ., "}")
  }

  string[stringr::str_detect(string, "  ")] <- " "

  glue::glue_collapse(string) %>%
    glue::glue_col() %>%
    print()
}
