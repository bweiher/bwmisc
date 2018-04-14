
utils::globalVariables(c("n", "rows", "."))

#' Conditionally remove rows from a database table
#'
#'
#' Allows you to drop specified rows from a database rather than deleting the whole table
#'
#'
#' @export
#' @param db a database connection
#' @param table the table to modify
#' @param column the column to apply filters on
#' @param vals the values to use in the filter

remove_db_rows <- function(db, table, column, vals) {

  # find all the rows affected by this
  query <- "
     select count(1) as rows
     from {table}
     where {`column`} in ({vals*})
    "

  wrap_p <- function(x) {
    paste0("(", x, ")")
  }

  if (is.character(vals)) {
    vals_part <- glue::collapse(paste0("'", vals, "'"), sep = " ") %>%
      wrap_p()
  } else {
    vals_part <- glue::collapse(vals, sep = " , ") %>%
      wrap_p()
  }

  main_part <- stringr::str_split(query, " in ")[[1]][1] %>%
    stringr::str_replace_all("\\`|\\`", "")

  pure_sql <- paste(main_part, vals_part, sep = " in ") # todo this is the problem here

  print_query <- glue::glue_col(
    "

    {mt {pure_sql}}

    "
  )


  # send query to db
  result <- odbc::dbSendQuery(db, glue::glue_sql(query, .con = db))

  # fetch the result set
  fetched_rows <- dplyr::pull(odbc::dbFetch(result), rows)
  odbc::dbClearResult(result)

  if (fetched_rows > 0) {
    print(print_query)


    glue::glue_col("


       {rw ----------------------------------- {rw Warning! }-------------------------------------------------}

       {mt You are about to remove {ri {fetched_rows} rows} from the {ri  {table}} table. Are you sure you wish to proceed?}



         ") %>%
      print()

    Sys.sleep(1.5)



    # ask the user to validate their intent
    response <- rstudioapi::askForPassword(prompt = "Type 'yes' or 'y' to continue (not case sensitive)") %>%
      stringr::str_to_lower()


    # if the user wants to deleete these rows, do it
    if (response %in% c("yes", "y")) {

      # delete query
      query <- "
  delete from {table}
  where {`column`} in ({vals*})
   ;
  " %>%
        glue::glue_sql(.con = db)


      # send query to db
      result <- odbc::dbSendQuery(db, query)
      odbc::dbClearResult(result)


      glue::glue_col("{mt {ri {fetched_rows}} rows have been succesfully dropped from {ri {table}}} ")
    } else { # response is not affirmative
      glue::glue_col("{mt No modifications were made to the table {ri {table}}}")
    }
  } else { # it fetched 0 rows
    glue::glue_col(
      "



           {ri {fetched_rows}} {mt rows will be affected by your query on {ri {table}} so no action has been taken!}



           "
    )
  }
}
