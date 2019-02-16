#' Apply a function to some data, and then write the result
#'
#' Via chunking
#'
#' @export
#' @param dataframe data
#' @param fun function to apply
#' @param dir directory to write data to
#' @param chunks a discrete number of chunks to iterate over

write_chunks <- function(dataframe, fun, dir, chunks){
  for(i in seq_along(chunks)){
    
    x <- chunks[[i]]
    
    output <- fun(dataframe[x,])
    
    name <- paste(sample(letters, 30, replace = TRUE), collapse = '')
    
    utils::write.csv(output, glue::glue("{dir}/{name}.csv"), row.names = FALSE)
    
  }
}



