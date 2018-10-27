#' Returns JavaScript formatting date to quarterly values
#'
#'
#'
#'
#' @export
#' @param ds The datetime on the x-axis
#' @param just_quarter Just return the quarter..for X axis label
#' @param additional_js Character string to pass on as JavaScript


hc_quarter <- function(ds, just_quarter = TRUE, additional_js = NULL){
  
  query <- glue::glue("function(){{
    var n = new Date({ds});
    var year = n.getUTCFullYear();
    var month = n.getUTCMonth() + 1;
    var quarter
              
    if(month <= 3){{ 
    quarter = 1
    }} else if (month >  3 & month <= 4){{ 
    quarter = 2
    }} else if (month > 4 & month <= 7){{
    quarter = 3
    }} else if (month > 7){{
    quarter = 4
    }} 
")
  
  quarter <- glue::glue("return year + '.' + quarter")
  
  if(isTRUE(just_quarter)){
    query <- c(query,quarter)
  } else {
    if(is.null(additional_js)) stop("You need to provide an argument for `additional_js`")
    query <- c(query, paste0(quarter, " + '<br>' + "), additional_js) 
  }
  glue::glue_collapse(c(query,glue::glue("}}")), sep = "\n") %>% 
    highcharter::JS()
}

