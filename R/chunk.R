#' Apply a function to some data, and then write the result
#'
#' Via chunking
#'
#' @export
#' @param x vector of something to chunk
#' @param n number of chunks
#' @return chunks of data
#' @examples 
#' chunk(1:10000, 5) # 5 chunks

chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))


