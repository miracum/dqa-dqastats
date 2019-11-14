# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019 Universitätsklinikum Erlangen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @title test_target_db helper function
#'
#' @description Internal function to test and get the database connection of
#'   the target data system.
#'
#' @param target_settings A list object containing the database settings.
#' @param headless A boolean (default: FALSE). Indicating, if the function is
#'   run only in the console (headless = TRUE) or on a GUI frontend
#'   (headless = FALSE).
#'
#' @export
#'
# test db connection
test_target_db <- function(target_settings,
                           headless = FALSE) {

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
    db_con
  }, error = function(e) {
    if (isFALSE(headless)) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Error occured during testing database connection",
          paste0("An error occured during the test of the database ",
                 "connection. Please check your settings and try again.")
        )
      )
    }
    cat("\nDB connection error\n")
    db_con <- NULL
    db_con
  }, finally = function(f) {
    return(db_con)
  })
  return(db_con)
}

#' @title test_csv helper function
#'
#' @description Internal function to test and get the database connection
#'   of the source data system.
#'
#' @param source_settings A list object containing the database settings.
#' @inheritParams create_helper_vars
#' @inheritParams test_target_db
#'
#' @export
#'
test_csv <- function(source_settings,
                           source_db,
                           headless = FALSE) {

  if (source_db == "p21csv") {
    filelist <- list.files(
      path = source_settings$dir,
      pattern = "\\.CSV|\\.csv",
      full.names = T
    )
    # iterate over list and check for presence of required filenames:
    # FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV
    check <- sapply(filelist, function(i) {
      cat("\n", i, "\n")
      return(grepl("FALL\\.CSV$|FAB\\.CSV$|ICD\\.CSV$|OPS.CSV$", i))
    })

    outflag <- tryCatch({
      # test if there are exact 4 source files
      if (base::sum(check) != 4) {
        if (isFALSE(headless)) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Invalid path",
              paste0("The specified directory does not contain the 4 ",
                     "neccessary CSV-files ",
                     "(FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV).")
            )
          )
        }
        cat(
          paste0("\nThe specified directory does not contain the ",
                 "neccessary CSV-files ",
                 "(FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV).\n")
        )
        outflag <- NULL
        outflag
      } else {
        outflag <- TRUE
        outflag
      }
    }, error = function(e) {
      if (isFALSE(headless)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Invalid path",
            "There are no CSV-files in the specified directory."
          )
        )
      }
      cat("\nThere are no CSV-files in the specified directory.\n")
      outflag <- NULL
      outflag
    }, finally = function(f) {
      return(outflag)
    })

  } else {
    cat("\nThis source_db is not implemented yet\n")
    outflag <- NULL
  }
  return(outflag)
}
