#' Generate Hive CTAS statement from R dataframe
#'
#'
#'
#' @export
#' @param dataframe data
#' @param comment the description of each column
#' @param table schema.table name
#' @param partitition quoted partitions to include
#' @return a query to execute in hive
#' @examples
#' generate_hive_ctas(data.frame(
#'   chr = letters[1:5],
#'   lgl = c(TRUE, FALSE, TRUE, FALSE, TRUE),
#'   dbl = runif(5),
#'   int = 15L:19L,
#'   time = Sys.time(),
#'   date = Sys.Date()
#' ))
generate_hive_ctas <- function(dataframe, comment = NA, table = "random", partitition = NULL) {
  if (length(comment) != ncol(dataframe) && !is.na(comment)) stop("comment and colnames must be of equal length")

  if (table == "random") {
    table <- paste0("tmp.", paste0(sample(letters, 12, replace = TRUE), collapse = ""))
  }

  if (sum(is.na(comment)) > 0) {
    comment <- rep("", ncol(dataframe))
  }

  if (any(comment != "")) {
    comment_col <- paste0("'", comment, "'", ",")
    comment_col[length(comment_col)] <- stringr::str_replace(comment_col[length(comment_col)], ",", "")
  } else {
    comment_col <- paste0("'", comment, "'", ",")
    comment_col[length(comment_col)] <- stringr::str_replace(comment_col[length(comment_col)], ",", "")
  }



  # get the type of the columns from R to Hive
  types <- as.character(purrr::map_chr(dataframe, typeof_safe))
  chr <- character(length = length(types))
  hive_datatypes <- switch_to_hive(types)
  names <- names(dataframe)
  names <- equalize_chr_vector(names)

  # using partitions
  if (!is.null(partitition)) {
    part_type <- hive_datatypes[which(names(dataframe) %in% partitition)]
    part_name <- names(dplyr::select(dataframe, partitition))
    y <- paste(part_name, part_type, collapse = ",\n")

    part <- glue::glue(" PARTITION BY ( 
                 {    y}
                 )")
  } else {
    part <- ""
  }

  x <- equalize_chr_vector(paste0(names, "    ", hive_datatypes))
  x <- equalize_chr_vector(paste0(x, "    COMMENT", "    "))
  x <- paste0(x, equalize_chr_vector(comment_col))



  glue::glue(
    "CREATE TABLE IF NOT EXISTS {table} ( 
{ 

glue::glue_collapse(x, sep = '\n')}
) {part}

                                                                     "
  )
}
