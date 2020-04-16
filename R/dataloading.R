# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2020 Universitätsklinikum Erlangen
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
load_csv_files <- function(mdr,
                           inputdir,
                           sourcesystem,
                           headless = T,
                           logfile_dir) {

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(
      message = paste0("Reading CSV file from directory: ",
                       inputdir),
      value = 0
    )
  }

  # original beginning of function
  inputdir <- DIZutils::clean_path_name(inputdir)


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
    DIZutils::feedback(msg, logjs = isFALSE(headless), findme = "73c0aae8d4",
                       logfile_dir = logfile_dir,
                       headless = headless)
    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(available_systems[, unique(get("source_table_name"))]),
        detail = paste("... working hard to read", inputfile, "..."))
    }



    input_vars <- unique(
      available_systems[get("source_table_name") ==
                          inputfile &
                          !is.na(get("variable_type")),
                        c("source_variable_name",
                          "variable_type")]
    )

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
        DIZutils::feedback(
          paste0(
            outlist[[inputfile]][get("AUFNAHMEANLASS") == "B", .N],
            paste0(" chaperones present in source data system.\n\n",
                   "These will be removed from further analyses.")
          ),
          findme = "14ae722d8b",
          logfile_dir = logfile_dir,
          headless = headless
        )
        outlist[[inputfile]] <-
          outlist[[inputfile]][get("AUFNAHMEANLASS") != "B" |
                                 is.na(get("AUFNAHMEANLASS")), ]
      } else {
        DIZutils::feedback("No chaperones present in your source data.",
                           findme = "469a0f6dde",
                           logfile_dir = logfile_dir,
                           headless = headless)
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



#' @title load_csv helper function
#'
#' @description Internal function to load the source data
#'
#' @param rv A list object. Internal list simulating Shiny's
#'   'reactive values'.
#' @param keys_to_test A vector containing the names (keys) of
#'   the variables to test.
#' @param system The system object rv$system
#' @inheritParams test_csv
#'
#' @export
load_csv <- function(rv,
                     keys_to_test,
                     headless = FALSE,
                     system) {

  stopifnot(is.character(system$settings$path))

  # initialize outlist
  outlist <- list()

  # read sourcedata
  outlist <- load_csv_files(
    mdr = rv$mdr,
    inputdir = DIZutils::clean_path_name(system$settings$path),
    sourcesystem = system$system_name,
    headless = headless,
    logfile_dir = rv$log$logfile_dir
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
    var_names <- rv$mdr[get("source_table_name") == i &
                          get("source_system_name") == system$system_name,
    ][
      , get("source_variable_name")
    ]
    stopifnot(is.character(var_names))

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming source variable types", i)
    DIZutils::feedback(msg, logjs = isFALSE(headless), findme = "776ba03cbf",
                       logfile_dir = rv$log$logfile_dir,
                       headless = rv$headless)
    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(keys_to_test),
        detail = paste("... transforming", i, "...")
      )
    }

    for (j in col_names) {
      if (j %in% var_names) {
        vn <- rv$mdr[get("source_table_name") == i &
                       get("source_system_name") == system$system_name,
        ][
          get("source_variable_name") ==
            j, unique(get("variable_name"))]
        colnames(outlist[[i]])[which(col_names == j)] <- vn

        # transform date_vars to dates
        if (vn %in% rv$date_vars) {
          outlist[[i]][, (vn) := as.Date(
            substr(as.character(get(vn)), 1, 8), format = "%Y%m%d"
          )]
        } else if (vn %in% rv$cat_vars) {
          # transform cat_vars to factor
          outlist[[i]][, (vn) := factor(get(vn))]
        }
        # the following should not be necessary due to the variable type
        # mapping in the fread function
        #% else if (vn %in% rv$num_vars) {
        #%   # transform num_vars to numeric
        #%   outlist[[i]][, (vn) := as.numeric(get(vn))]
        #% }
      }
    }
  }
  if (isFALSE(headless)) {
    progress$close()
  }
  return(outlist)
}


#' @title load_database helper function
#'
#' @description Internal function to load the target data
#'
#' @param sql_statements The SQL-Statement-object
#' @param db_con The connection-socket
#' @param db_name The database name
#'
#' @inheritParams load_csv
#'
#' @export
load_database <- function(rv,
                          sql_statements,
                          db_con,
                          keys_to_test,
                          db_name,
                          headless = FALSE) {

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(
      message = paste0(
        "Reading data from database: ",
        db_name
      ),
      value = 0
    )
  }

  # read target data
  outlist <- sapply(keys_to_test, function(i) {
    msg <- paste("Getting", i, "from database:", db_name)
    DIZutils::feedback(msg,
                       logjs = isFALSE(headless),
                       findme = "c12a1dd9ce",
                       logfile_dir = rv$log$logfile_dir,
                       headless = rv$headless)

    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(keys_to_test),
        detail = paste("... working hard to read", i, "...")
      )
    }

    stopifnot(!is.null(sql_statements[[i]]))

    fire_sql_statement(
      rv = rv,
      db_con = db_con,
      sql = sql_statements[[i]],
      headless = headless
    )
  }, simplify = F, USE.NAMES = T)

  if (isFALSE(headless)) {
    progress$close()
  }

  RPostgres::dbDisconnect(db_con)

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
    DIZutils::feedback(msg, logjs = isFALSE(headless), findme = "7a3e28f291",
                       logfile_dir = rv$log$logfile_dir,
                       headless = rv$headless)
    if (isFALSE(headless)) {
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

      # transform cat_vars to factor
      if (j %in% rv$cat_vars) {
        outlist[[i]][, (j) := factor(get(j))]

        # transform date vars
      } else if (j %in% rv$date_vars) {
        if (outlist[[i]][, is.factor(get(j))]) {
          outlist[[i]][, (j) := as.Date(
            substr(as.character(get(j)), 1, 8), format = "%Y%m%d"
          )]
        }
      } else if (j %in% rv$num_vars) {
        # transform num_vars to numeric
        outlist[[i]][, (j) := as.numeric(get(j))]
      }
    }
  }
  if (isFALSE(headless)) {
    progress$close()
  }
  return(outlist)
}

#' @title data_loading helper function
#'
#' @description Internal function to load the source and target data
#'
#' @param rv The complete reactive-value dataset
#'
#' @param system The part of the rv-list which should be loaded
#'   (e.g. rv$source or rv$target)
#'
#' @inheritParams load_csv
#'
#' @export
data_loading <- function(rv, system, keys_to_test) {
  # TODO: Test it!
  #

  # check if all now necessary parameters are correct:
  stopifnot(
    # rv:
    !is.null(rv) & is.list(rv) & length(rv) > 0,
    # system:
    !is.null(system) & is.list(system) & length(system) > 0,
    # system$settings:
    !is.null(system$settings) &
      is.list(system$settings) & length(system$settings) > 0,
    # system$system_name:
    !is.null(system$system_name) &
      is.character(system$system_name),
    # keys_to_test:
    !is.null(keys_to_test) &
      is.character(keys_to_test),
    # rv$headless:
    !is.null(rv$headless) &
      is.logical(rv$headless),
    !is.null(rv$mdr) &
      data.table::is.data.table(rv$mdr)
  )

  # create return object
  outlist <- list()

  if (system$system_type == "csv") {
    test_csv_result <- test_csv(
      settings = system$settings,
      source_db = system$system_name,
      mdr = rv$mdr,
      headless = rv$headless,
      logfile_dir = rv$log$logfile_dir
    )
    stopifnot(isTRUE(test_csv_result))

    # load csv
    outlist$outdata <- load_csv(
      rv = rv,
      keys_to_test = keys_to_test,
      headless = rv$headless,
      system = system
    )
    outlist$sql_statements <- NA

  } else if (system$system_type == "postgres") {
    # import target SQL
    outlist$sql_statements <- load_sqls(utils_path = rv$utilspath,
                                        db = system$system_name)
    stopifnot(is.list(outlist$sql_statements))

    # test target_db
    db_con <-
      DIZutils::db_connection(
        db_name = system$system_name,
        db_type = system$system_type,
        headless = rv$headless,
        logfile_dir = rv$log$logfile_dir
      )
    stopifnot(!is.null(db_con))

    # load target data
    outlist$outdata <- load_database(
      rv = rv,
      sql_statements = outlist$sql_statements,
      db_con = db_con,
      keys_to_test = keys_to_test,
      headless = rv$headless,
      db_name = system$system_name
    )
    rm(db_con)

  } else {
    stop("\nThis source_system_type is currently not implemented.\n\n")
  }
  return(outlist)
}
