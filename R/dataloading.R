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


# fire SQL to database
fire_sql_statement <- function(rv,
                               db_con,
                               sql,
                               headless = FALSE) {
  # for debugging:
  #% print(jsonobj)

  # Errorhandling
  if (!is.null(sql)) {
    # avoid sql-injection
    # https://db.rstudio.com/best-practices/run-queries-safely/
    sql <- DBI::sqlInterpolate(db_con, sql)
    outdat <-
      data.table::data.table(
        RPostgres::dbGetQuery(db_con, sql),
        stringsAsFactors = TRUE
      )
    return(outdat)
  } else {
    return(NULL)
  }
}

# load csv files
load_csv <- function(mdr,
                     inputdir,
                     sourcesystem,
                     headless) {

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(message = "Reading CSV file from directory",
                 value = 0)
  }

  # original beginning of function
  inputdir <- clean_path_name(inputdir)


  available_systems <- mdr[get("source_system_name") == sourcesystem &
                             get("source_system_type") == "csv", ]

  stopifnot(# fix test for multiple files
    (!any(
      !available_systems[, unique(get("source_table_name"))] %in%
        list.files(inputdir)
    ))
  )

  # define outlist
  outlist <- list()

  for (inputfile in available_systems[, unique(get("source_table_name"))]) {

    msg <- paste("Reading", inputfile, "from CSV.")
    cat("\n", msg, "\n")
    if (isFALSE(headless)) {
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(available_systems[, unique(get("source_table_name"))]),
        detail = paste("... working hard to read", inputfile, "..."))
    }


    input_vars <- available_systems[get("source_table_name") ==
                                      inputfile, c("source_variable_name",
                                                   "variable_type")]

    select_cols <- unlist(
      sapply(
        input_vars$source_variable_name,
        FUN = function(x) {
          map_var_types(
            input_vars[get("source_variable_name") == x, "variable_type"]
          )
        },
        simplify = TRUE,
        USE.NAMES = TRUE
      )
    )

    outlist[[inputfile]] <- data.table::fread(
      paste0(inputdir, inputfile),
      select = names(select_cols),
      colClasses = select_cols,
      header = T,
      na.strings = "",
      stringsAsFactors = TRUE
    )

    # TODO special MIRACUM treatment
    # treating of §21 chaperones
    if (tolower(inputfile) == "fall.csv") {
      if (outlist[[inputfile]][get("AUFNAHMEANLASS") == "B", .N] > 0) {
        cat(
          paste0(
            "\n",
            outlist[[inputfile]][get("AUFNAHMEANLASS") == "B", .N],
            paste0(" chaperones present in source data system.\n\n",
                   "These will be removed from further analyses.")
          )
        )
        outlist[[inputfile]] <-
          outlist[[inputfile]][get("AUFNAHMEANLASS") != "B" |
                                 is.na(get("AUFNAHMEANLASS")), ]
      } else {
        cat("\nNo chaperones present in your source data.\n")
      }
    }
  }
  if (isFALSE(headless)) {
    progress$close()
  }
  return(outlist)
}

map_var_types <- function(string) {
  stopifnot(
    length(string) == 1
  )

  if (string == "permittedValues") {
    outdat <- "factor"
  } else if (string == "integer") {
    outdat <- "numeric"
  } else if (string == "string") {
    outdat <- "character"
  } else if (string == "calendar") {
    outdat <- "character"
  } else if (string == "float") {
    outdat <- "numeric"
  } else {
    outdat <- NULL
  }
  return(outdat)
}



#' @title load_source helper function
#'
#' @description Internal function to load the source data
#'
#' @param rv A list object. Internal list simulating Shiny's
#'   'reactive values'.
#' @param keys_to_test A vector containing the names (keys) of
#'   the variables to test.
#' @inheritParams test_target_db
#'
#' @export
load_source <- function(rv,
                        keys_to_test,
                        headless = FALSE) {

  # initialize outlist
  outlist <- list()

  # read sourcedata
  outlist <- load_csv(
    mdr = rv$mdr,
    inputdir = rv$sourcefiledir,
    sourcesystem = "p21csv",
    headless = headless
  )

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(message = "Transforming source variable types",
                 value = 0)
  }

  # datatransformation source:
  for (i in keys_to_test) {
    # get column names
    col_names <- colnames(outlist[[i]])

    # check, if column name in variables of interest
    # var_names of interest:
    var_names <-
      rv$mdr[get("source_table_name") == i,
             ][
               grepl("dt\\.", get("key")), get("source_variable_name")]

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming source variable types", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)) {
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(keys_to_test),
        detail = paste("... transforming", i, "...")
      )
    }

    for (j in col_names) {
      if (j %in% var_names) {
        vn <- rv$mdr[get("source_table_name") == i,
                     ][
                       grepl("dt\\.", get("key")),
                       ][
                         get("source_variable_name") ==
                           j, get("variable_name")]
        colnames(outlist[[i]])[which(col_names == j)] <- vn

        # transform date_vars to dates
        if (vn %in% rv$date_vars) {
          outlist[[i]][, (vn) := as.Date(
            substr(as.character(get(vn)), 1, 8), format = "%Y%m%d"
          )]
        }

        if (vn %in% rv$trans_vars) {
          outlist[[i]][, (vn) := transform_factors(
            vector = get(vn),
            transformation = vn
          )]
        }

        # transform cat_vars to factor
        if (vn %in% rv$cat_vars) {
          outlist[[i]][, (vn) := factor(get(vn))]
        }
      }
    }
  }
  if (isFALSE(headless)) {
    progress$close()
  }
  return(outlist)
}


#' @title load_target helper function
#'
#' @description Internal function to load the target data
#'
#' @inheritParams load_source
#'
#' @export
load_target <- function(rv,
                        keys_to_test,
                        headless = FALSE) {

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(message = "Reading data from database",
                 value = 0)
  }

  # read target data
  outlist <- sapply(keys_to_test, function(i) {
    msg <- paste("Getting", i, "from database.")
    cat("\n", msg, "\n")
    if (isFALSE(headless)) {
      shinyjs::logjs(msg)

      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(keys_to_test),
        detail = paste("... working hard to read", i, "...")
      )
    }

    fire_sql_statement(
      rv = rv,
      db_con = rv$db_con_target,
      sql = rv$sql_target[[i]],
      headless = headless
    )
  }, simplify = F, USE.NAMES = T)

  if (isFALSE(headless)) {
    progress$close()
  }

  RPostgres::dbDisconnect(rv$db_con_target)

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(message = "Transforming target variable types",
                 value = 0)
  }

  for (i in keys_to_test) {
    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming target variable types", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)) {
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(keys_to_test),
        detail = paste("... transforming", i, "...")
      )
    }

    # get column names
    col_names <- colnames(outlist[[i]])

    # check, if column name in variables of interest
    for (j in col_names) {
      if (j %in% rv$trans_vars) {
        outlist[[i]][, (j) := transform_factors(
          vector = get(j),
          transformation = j
        )]
      }

      # transform cat_vars to factor
      if (j %in% rv$cat_vars) {
        outlist[[i]][, (j) := factor(get(j))]
      }
    }
  }
  if (isFALSE(headless)) {
    progress$close()
  }
  return(outlist)
}
