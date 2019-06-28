#' Generate Hive CTAS statement from R dataframe
#'
#'
#'
#' @export
#' @param dataframe data
#' @param comment the description of each column
#' @param table schema.table name
#' @param partitition quoted partitions to include
#' @param tbl_properties named list of properties to give
#' @return a create table statement executable in hive
#' @examples
#' hive_ctas(data.frame(
#'   chr = letters[1:5],
#'   lgl = c(TRUE, FALSE, TRUE, FALSE, TRUE),
#'   dbl = runif(5),
#'   int = 15L:19L,
#'   time = Sys.time(),
#'   date = Sys.Date()
#' ),
#' partitition = "date"
#' )
hive_ctas <- function(dataframe, comment = NULL, table = "random", partitition = NULL, tbl_properties = NULL) {
  
  if (length(comment) != ncol(dataframe) && !is.null(comment)) stop("comment and colnames must be of equal length")
  
  if(!is.list(tbl_properties) && !is.null(tbl_properties)) stop("pass arguments as a named list")

  if (table == "random") {
    table <- paste0("tmp.", paste0(sample(letters, 9, replace = TRUE), collapse = ""), "_", stringr::str_replace_all(Sys.Date(), "-", ""))
  }

  if (sum(is.null(comment)) > 0) {
    comment <- rep("", ncol(dataframe))
  }

  comment_col <- paste0("'", comment, "'", ",")
  comment_col[length(comment_col)] <- stringr::str_replace(comment_col[length(comment_col)], ",", "")

  # get the type of the columns from R to Hive
  types <- as.character(purrr::map_chr(dataframe, typeof_safe))
  hive_datatypes <- switch_to_hive(types)
  names <- names(dataframe)
  
  # using partitions
  if (!is.null(partitition)) {
    part_type <- hive_datatypes[which(names(dataframe) %in% partitition)]
    part_type <- glue::glue_col("{italic {part_type}}")
    part_name <- names(dataframe)[names(dataframe) %in% partitition]
    y <- paste(part_name, part_type, collapse = "\n")

    part <- glue::glue_col("{bold PARITITIONED BY }( 
                 {    y}
                 )")
  } else {
    part <- ""
  }

  names <- equalize_chr_vector(names)
  
  if(!is.null(partitition)) names <- names[names != part_name]
  
  
  if(is.null(tbl_properties)){
    props <- glue::glue("
'abb_retention_days' = '3',
'abb_retention_reason' = 'meow'")
  } else {
  
    
    props <- paste(names(tbl_properties),paste0("'",as.character(tbl_properties),"'"), sep = ' = ') %>% 
      paste(collapse = ",\n")
    
  }
  
  props <- glue::glue_col("{bold TBLPROPERTIES } (
{props}
)")

  x <- equalize_chr_vector(paste0(names, "    ", glue::glue_col("{italic {hive_datatypes}}")))
  x <- equalize_chr_vector(paste0(x, "    COMMENT", "    "))
  x <- paste0(x, equalize_chr_vector(glue::glue_col("{italic {comment_col}}")))

  print(glue::glue_col("
{bold CREATE TABLE IF NOT EXISTS} {bi {table}} (
{glue::glue_collapse(x, sep = '\n')}
) {part} {props}
"))
}
