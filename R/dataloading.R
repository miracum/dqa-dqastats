# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2021 Universitätsklinikum Erlangen
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

# load csv files
load_csv_files <- function(mdr,
                           inputdir,
                           sourcesystem,
                           headless = T,
                           logfile_dir,
                           restricting_date = list(use_it = FALSE)) {

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

    input_vars <- unique(
      available_systems[
        get("source_table_name") == inputfile &
          !is.na(get("variable_type")),
        c("source_variable_name", "variable_type"),
        with = FALSE
      ]
    )

    select_cols <- unlist(
      sapply(
        X = input_vars$source_variable_name,
        FUN = function(x) {
          map_var_types(
            input_vars[
              get("source_variable_name") == x,
              get("variable_type")
            ]
          )
        },
        simplify = TRUE,
        USE.NAMES = TRUE
      )
    )

    unfiltered_table <- NULL
    filtered_table <- NULL

    unfiltered_table <- data.table::fread(
      paste0(inputdir, inputfile),
      select = names(select_cols),
      colClasses = select_cols,
      header = T,
      na.strings = "",
      stringsAsFactors = TRUE
    )

    ## Apply time filtering:
    if (restricting_date$use_it) {
      filtered_table <-
        apply_time_restriciton(
          data = unfiltered_table,
          filter_colname = unique(mdr[get("source_system_name") == sourcesystem &
                                        get("source_table_name") == inputfile,
                                      get("restricting_date_var")]),
          lower_limit = restricting_date$start,
          upper_limit = restricting_date$end
        )
    } else {
      filtered_table <- unfiltered_table
    }

    outlist[[inputfile]] <- filtered_table

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
    logfile_dir = rv$log$logfile_dir,
    restricting_date = rv$restricting_date
  )

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

    for (j in col_names) {

      var_type <- rv$mdr[get("source_system_name") == system$system_name &
                           get("source_table_name") == i &
                           get("source_variable_name") == j,
                         unique(get("variable_type"))]

      if (j %in% var_names && j %in% colnames(outlist[[i]])) {
        vn <- rv$mdr[get("source_table_name") == i &
                       get("source_system_name") == system$system_name,
        ][
          get("source_variable_name") ==
            j, unique(get("variable_name"))]
        colnames(outlist[[i]])[which(col_names == j)] <- vn

        if (var_type %in% c("permittedValues", "string", "catalog")) {
          # transform to factor
          outlist[[i]][, (vn) := factor(get(vn))]
        } else if (var_type == "calendar") {
          # transform date variables
          date_format <- rv$mdr[
            get("source_system_name") == system$system_name &
              get("source_table_name") == i &
              get("variable_name") == vn,
            unique(get("constraints"))
          ]
          if (is.na(date_format) || grepl("^\\s*$", date_format)) {
            # set date format to default value
            date_format <- "%Y-%m-%d"
          }
          outlist[[i]][, (vn) := as.Date(
            as.character(get(vn)),
            format = date_format
          )]
        } else if (var_type %in% c("integer", "float")) {
          # transform numeric variables
          outlist[[i]][, (vn) := as.numeric(
            as.character(get(vn))
          )]
        }
      }
    }
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
#' @param db_type The database type (postgres/oracle)
#'
#' @inheritParams load_csv
#'
#' @export
load_database <- function(rv,
                          sql_statements,
                          db_con,
                          keys_to_test,
                          db_name,
                          headless = FALSE,
                          db_type) {

  # initialize outlist
  outlist <- list()

  # read target data
  outlist <- sapply(keys_to_test, function(i) {
    msg <- paste("Getting", i, "from database:", db_name)
    DIZutils::feedback(msg,
                       logjs = isFALSE(headless),
                       findme = "c12a1dd9ce",
                       logfile_dir = rv$log$logfile_dir,
                       headless = rv$headless)

    stopifnot(!is.null(sql_statements[[i]]))


    ## Apply time filtering:
    if (rv$restricting_date$use_it) {
      ## Filter SQL
      sql <- apply_time_restriciton(
        data = sql_statements[[i]],
        filter_colname = unique(rv$mdr[get("key") == i &
                                         get("source_system_name") == db_name &
                                         get("dqa_assessment") == 1, get("restricting_date_var")]),
        lower_limit = rv$restricting_date$start,
        upper_limit = rv$restricting_date$end,
        system_type = db_type
      )
    } else {
      ## Unfiltered:
      sql <- sql_statements[[i]]
    }

    dat <- DIZutils::query_database(
      db_con = db_con,
      sql_statement = sql
    )



    # check, if table has more than two columns and thus does not comply
    # with DQAstats table requirements for SQL based systems
    if (dim(dat)[2] > 2) {
      msg <- paste0(
        "Table of data element '", i,
        "' has > 2 columns. Aborting session.\n",
        "Please adjust the SQL statement to return max. 2 columns."
      )
      DIZutils::feedback(msg,
                         type = "Error",
                         logjs = isFALSE(headless),
                         findme = "c1902dd9cf",
                         logfile_dir = rv$log$logfile_dir,
                         headless = rv$headless)
      # raise error
      stop(msg)
    } else {
      return(dat)
    }

  }, simplify = F, USE.NAMES = T)

  RPostgres::dbDisconnect(db_con)

  for (i in keys_to_test) {
    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming target variable types", i)
    DIZutils::feedback(msg, logjs = isFALSE(headless), findme = "7a3e28f291",
                       logfile_dir = rv$log$logfile_dir,
                       headless = rv$headless)

    # get column names
    col_names <- colnames(outlist[[i]])

    # check, if column name in variables of interest
    for (j in col_names) {

      var_type <- rv$mdr[get("source_system_name") == db_name &
                           get("key") == i &
                           get("variable_name") == j, get("variable_type")]


      if (var_type %in% c("permittedValues", "string", "catalog")) {
        # transform to factor
        outlist[[i]][, (j) := factor(get(j))]
      } else if (var_type == "calendar") {
        # transform date variables
        # transform date variables
        date_format <- rv$mdr[
          get("source_system_name") == db_name &
            get("key") == i &
            get("variable_name") == j,
          unique(get("constraints"))
        ]
        if (is.na(date_format) || grepl("^\\s*$", date_format)) {
          # set date format to default value
          date_format <- "%Y%m%d"
        }
        outlist[[i]][, (j) := as.Date(
          as.character(get(j)),
          format = date_format
        )]
      } else if (var_type %in% c("integer", "float")) {
        # transform numeric variables
        outlist[[i]][, (j) := as.numeric(
          as.character(get(j))
        )]
      }
    }
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
    if (is.null(system$settings)) {
      ## Use environment-settings:
      db_con <-
        DIZutils::db_connection(
          db_name = system$system_name,
          db_type = system$system_type,
          headless = rv$headless,
          logfile_dir = rv$log$logfile_dir
        )
    } else {
      ## Use included settings:
      db_con <-
        DIZutils::db_connection(
          db_name = system$system_name,
          db_type = system$system_type,
          headless = rv$headless,
          logfile_dir = rv$log$logfile_dir,
          from_env = FALSE,
          settings = system$settings
        )
    }

    stopifnot(!is.null(db_con))

    # load target data
    outlist$outdata <- load_database(
      rv = rv,
      sql_statements = outlist$sql_statements,
      db_con = db_con,
      keys_to_test = keys_to_test,
      headless = rv$headless,
      db_name = system$system_name,
      db_type = system$system_type
    )
    rm(db_con)

  }  else if (system$system_type == "oracle") {
    # import target SQL
    outlist$sql_statements <- load_sqls(utils_path = rv$utilspath,
                                        db = system$system_name)
    stopifnot(is.list(outlist$sql_statements))

    # test target_db
    if (is.null(system$settings)) {
      ## Use environment-settings:
      db_con <-
        DIZutils::db_connection(
          db_name = system$system_name,
          db_type = system$system_type,
          headless = rv$headless,
          logfile_dir = rv$log$logfile_dir,
          lib_path = Sys.getenv(paste0(
            toupper(system$system_name), "_DRIVER"
          ))
        )
    } else {
      ## Use included settings:
      db_con <-
        DIZutils::db_connection(
          db_name = system$system_name,
          db_type = system$system_type,
          headless = rv$headless,
          logfile_dir = rv$log$logfile_dir,
          lib_path = Sys.getenv(paste0(
            toupper(system$system_name), "_DRIVER"
          )),
          from_env = FALSE,
          settings = system$settings
        )
    }

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
