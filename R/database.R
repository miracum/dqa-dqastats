#' @title testTargetDB_ helper function
#'
#' @description Internal function to test and get the database connection of the target data system.
#'
#' @param target_settings A list object containing the database settings.
#' @param headless A boolean (default: FALSE). Indicating, if the function is run only in the console (headless = TRUE) or on a GUI frontend (headless = FALSE).
#'
#' @export
#'
# test db connection
testTargetDB_ <- function(target_settings, headless = FALSE){
  drv <- RPostgres::Postgres()
  db_con <- tryCatch({
    db_con <- RPostgres::dbConnect(
      drv = drv,
      dbname = target_settings$dbname,
      host = target_settings$host,
      port = target_settings$port,
      user = target_settings$user,
      password = target_settings$password
    )
  }, error = function(e){
    if (isFALSE(headless)){
      shiny::showModal(shiny::modalDialog(
        title = "Error occured during testing database connection",
        "An error occured during the test of the database connection. Please check your settings and try again."
      ))
    }
    cat("\nDB connection error\n")
    db_con <- NULL
  }, finally = function(f){
    return(db_con)
  })
}

#' @title testSourceDB_ helper function
#'
#' @description Internal function to test and get the database connection of the source data system.
#'
#' @param source_settings A list object containing the database settings.
#' @inheritParams DQA
#' @inheritParams testTargetDB_
#'
#' @export
#'
testSourceDB_ <- function(source_settings, source_db, headless = FALSE){
  if (source_db == "csv"){

    filelist <- list.files(path=source_settings$dir, pattern = "\\.CSV|\\.csv", full.names = T)
    # iterate over list and check for presence of required filenames: FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV
    check <- sapply(filelist, function(i){
      cat("\n", i, "\n")
      return(grepl("FALL\\.CSV$|FAB\\.CSV$|ICD\\.CSV$|OPS.CSV$", i))
    })

    outflag <- tryCatch({
      # test if there are exact 4 source files
      if (base::sum(check)!=4){
        if (isFALSE(headless)){
          shiny::showModal(shiny::modalDialog(
            title = "Invalid path",
            "The specified directory does not contain the 4 neccessary CSV-files (FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV)."
          ))
        }
        cat("\nThe specified directory does not contain the 4 neccessary CSV-files (FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV).\n")
        outflag <- NULL
      } else {
        outflag <- TRUE
      }
    }, error = function(e){
      if (isFALSE(headless)){
      shiny::showModal(shiny::modalDialog(
        title = "Invalid path",
        "There are no CSV-files in the specified directory."
      ))
      }
      cat("\nThere are no CSV-files in the specified directory.\n")
      outflag <- NULL
    }, finally = function(f){
      return(outflag)
    })

  } else {
    cat("\nThis source_db is not implemented yet\n")
    outflag <- NULL
  }
  return(outflag)
}
