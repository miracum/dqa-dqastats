# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2022 Universitätsklinikum Erlangen
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
                           headless = TRUE,
                           logfile_dir,
                           restricting_date = list(use_it = FALSE)) {

  # original beginning of function
  inputdir <- DIZtools::clean_path_name(inputdir)


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
    DIZtools::feedback(msg, logjs = isFALSE(headless), findme = "73c0aae8d4",
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
      header = TRUE,
      na.strings = "",
      stringsAsFactors = TRUE
    )

    msg <- paste("Getting ", inputfile)

    ## Apply time filtering:
    if (restricting_date$use_it) {
      filtered_table <-
        apply_time_restriciton(
          data = unfiltered_table,
          key = NULL,
          lower_limit = restricting_date$start,
          upper_limit = restricting_date$end,
          system_type = "csv",
          system_name = inputfile,
          logfile_dir = logfile_dir,
          mdr = mdr
        )
      msg <- paste0(msg, " (using a TEMPORAL VIEW)")
    } else {
      filtered_table <- unfiltered_table
    }

    DIZtools::feedback(print_this = msg,
                       logjs = isFALSE(headless),
                       findme = "81ba7f702f",
                       logfile_dir = logfile_dir,
                       headless = headless)


    outlist[[inputfile]] <- filtered_table

    # TODO special MIRACUM treatment
    # treating of §21 chaperones
    if (tolower(inputfile) == "fall.csv") {
      if (outlist[[inputfile]][get("AUFNAHMEANLASS") == "B", .N] > 0) {
        DIZtools::feedback(
          paste0(
            outlist[[inputfile]][get("AUFNAHMEANLASS") == "B", .N],
            paste0(" chaperones present in source database.\n\n",
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
        DIZtools::feedback("No chaperones present in your source data.",
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

  if (string == "enumerated") {
    outdat <- "factor"
  } else if (string == "integer") {
    outdat <- "numeric"
  } else if (string == "string") {
    outdat <- "character"
  } else if (string == "datetime") {
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
#' @description Internal function to load the data from CSV files.
#'
#' @param rv A list object. Internal list simulating Shiny's
#'   'reactive values'.
#' @param keys_to_test A vector containing the names (keys) of
#'   the variables to test.
#' @param system The system object rv$system
#'
#' @return A list with data.tables for each unique CSV file as defined in the
#'   'source_system_table' field of the MDR.
#'
#' @inheritParams test_csv
#'
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
    inputdir = DIZtools::clean_path_name(pathname = system$settings$path,
                                         remove.slash = FALSE),
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
    DIZtools::feedback(msg, logjs = isFALSE(headless), findme = "776ba03cbf",
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

        if (var_type %in% c("enumerated", "string", "catalog")) {
          # transform to factor
          outlist[[i]][, (vn) := factor(get(vn))]
        } else if (var_type == "datetime") {
          # transform date variables
          date_format <- rv$mdr[
            get("source_system_name") == system$system_name &
              get("source_table_name") == i &
              get("variable_name") == vn,
            unique(get("constraints"))
          ]
          if (is.na(date_format) ||
              grepl("^\\s*$", date_format) ||
              is.null(jsonlite::fromJSON(
                date_format
              )[["datetime"]][["format"]])) {
            # set date format to default value
            date_format <- "%Y-%m-%d"
          } else{
            date_format <- jsonlite::fromJSON(
              date_format
            )[["datetime"]][["format"]]
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
#' @description Internal function to load the data from SQL databases.
#'
#' @param sql_statements The SQL-Statement-object
#' @param db_con The connection-socket
#' @param db_name The database name
#' @param db_type The database type (postgres/oracle)
#'
#' @return A list with a data.table for each data element as defined in the
#'   in the MDR.
#'
#' @inheritParams load_csv
#'
load_database <- function(rv,
                          sql_statements,
                          db_con,
                          keys_to_test,
                          db_name,
                          headless = FALSE,
                          db_type) {

  ## Initialize outlist:
  outlist <- list()

  ## Read data:
  outlist <- sapply(
    X = keys_to_test,
    FUN = function(i) {
      stopifnot(!is.null(sql_statements[[i]]))

      msg <- paste("Getting", i, "from database", db_name)
      sql_extended <- NULL

      ## Apply time filtering (if needed):
      if (rv$restricting_date$use_it) {
        if (Sys.getenv(paste0(toupper(db_name), "_SQLMODIFY")) == "TRUE") {
          restricting_date_var <- rv$mdr[
            get("key") == i &
              get("source_system_name") == db_name,
            get("restricting_date_var")
          ]
          replace_string <- paste0(
            "AS r_intermediate WHERE r_intermediate.",
            restricting_date_var, " >= '",
            rv$restricting_date$start,
            "' AND r_intermediate.",
            restricting_date_var, " <= '",
            rv$restricting_date$end,
            "' "
          )
          sql <- gsub("AS r_intermediate", replace_string, sql_statements[[i]])
          msg <- paste0(msg, " (using a MODIFIED SUBSELECT)")
        } else {
          ## Filter SQL
          sql_list <- apply_time_restriciton(
            data = sql_statements[[i]],
            # filter_colname = unique(rv$mdr[get("key") == i &
            # get("source_system_name") == db_name &
            # get("dqa_assessment") == 1, get("restricting_date_var")]),
            lower_limit = rv$restricting_date$start,
            upper_limit = rv$restricting_date$end,
            system_name = db_name,
            system_type = db_type,
            key = i,
            mdr = rv$mdr,
            db_con = db_con,
            logfile_dir = rv$log$logfile_dir,
            sql_create_view_all = rv$db_name$sql_create_view_all
          )
          sql <- sql_list$sql
          sql_extended <- sql_list$sql_extended
          rv$db_name$sql_create_view_all <- sql_list$sql_create_view_all
          msg <- paste0(msg, " (using a TEMPORAL VIEW)")
        }
      } else {
        ## Unfiltered:
        sql <- sql_statements[[i]]
      }

      ## The `sql_extended` is the same like the normal `sql` but extened with
      ## additional information needed to run the SQL, e.g. the commands
      ## to create a view which the `sql` utilizes:
      if (is.null(sql_extended) ||
          !is.character(sql_extended)) {
        sql_extended <- sql
      } else {
        DIZtools::feedback(
          print_this = paste0(
            "Found extended SQL information. Using this one now: ",
            sql_extended
          ),
          findme = "060a2a152d"
        )
      }

      DIZtools::feedback(print_this = sql,
                         logjs = isFALSE(headless),
                         findme = "f45a1dc9ca",
                         logfile_dir = rv$log$logfile_dir,
                         headless = rv$headless)


      DIZtools::feedback(print_this = msg,
                         logjs = isFALSE(headless),
                         findme = "c12a1dd9ce",
                         logfile_dir = rv$log$logfile_dir,
                         headless = rv$headless)

      dat <- tryCatch({
        ## Note that there is also a `sql_extended`, which also has
        ## commands to create the necessary view(s) in it. BUT: This one
        ## would create the same temporal filtered view again for every
        ## data element. To avoid this, the view will be created if not
        ## existing in the previous `apply_time_restriciton` call and
        ## during the data extraction here, this view is assumed as existing.
        ## Thats the reason why we only use the `sql` here and NOT the
        ## extended `sql_extended.` The temporal view will automatically be
        ## deleted after the connection is closed. So no need to manually
        ## close it.
        DIZutils::query_database(db_con = db_con,
                                 sql_statement = sql)
      },
      error = function(cond) {
        DIZtools::feedback(
          print_this = paste0(
            "Error while trying to get the data for element '",
            i,
            "'. The sql was '",
            sql,
            "'. The error message is: '",
            cond,
            "'."
          ),
          type = "Error",
          findme = "c5291c15e3",
          logfile_dir = rv$log$logfile_dir,
          headless = rv$headless
        )
        stop("See error above.")
        return(NULL)
      })


      # check, if table has more than two columns and thus does not comply
      # with DQAstats table requirements for SQL based systems
      if (is.null(dat) || ncol(dat) > 2) {
        msg <- paste0(
          "Table of data element '",
          i,
          "' has > 2 columns. Aborting session.\n",
          "Please adjust the SQL statement to return max. 2 columns."
        )
        DIZtools::feedback(
          print_this = msg,
          type = "Error",
          logjs = isFALSE(headless),
          findme = "c1902dd9cf",
          logfile_dir = rv$log$logfile_dir,
          headless = rv$headless
        )
        # raise error
        stop(msg)
      } else {
        sql_extended <- gsub(
          pattern = "(,|AND|SELECT|FROM|JOIN|ON|WHERE)\\s?",
          replacement = "\\1\n",
          x = sql_extended,
          ignore.case = FALSE
        )
        return(list(
          "outdata" = dat,
          "sql_statements" = sql_extended
        ))
      }

    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  DIZutils::close_connection(db_con)

  for (i in keys_to_test) {
    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming target variable types", i)
    DIZtools::feedback(
      print_this = msg,
      logjs = isFALSE(headless),
      findme = "7a3e28f291",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless
    )

    # get column names
    col_names <- colnames(outlist[[i]][["outdata"]])

    # sometimes, colnames are altered by SQL-statement.
    # The next step is required to fix these wrong colnames
    # check colnames are in MDR variable_names
    "%notin%" <- utils::getFromNamespace(
      x = "%notin%",
      ns = "DIZtools"
    )

    # get true names
    mdr_var_names <- rv$mdr[
      get("source_system_name") == db_name, get("variable_name")
    ]

    # get wrong colnames
    wrong_colnames <- col_names[col_names %notin% mdr_var_names]

    if (length(wrong_colnames) > 0) {
      for (wcn in wrong_colnames) {
        correct_colname <- mdr_var_names[
          agrepl(
            pattern = wcn,
            x = mdr_var_names
          )
        ]
        if (length(correct_colname) == 1) {
          data.table::setnames(
            x = outlist[[i]][["outdata"]],
            old = wcn,
            new = correct_colname
          )
          DIZtools::feedback(
            print_this = paste0(
              "Replaced colname '", wcn, "' with '",
              correct_colname, "'."
            ),
            headless = rv$headless,
            findme = "d45dbd4b72"
          )
        } else {
          stop("\nNo correct colname found (cff40d689f)\n\n")
        }
      }
    }

    # check, if column name in variables of interest
    for (j in col_names) {
      var_type <- rv$mdr[get("source_system_name") == db_name &
                           #get("key") == i &
                           get("variable_name") == j, get("variable_type")]

      if (var_type %in% c("enumerated", "string", "catalog")) {
        # transform to factor
        outlist[[i]][["outdata"]][, (j) := factor(get(j))]
      } else if (var_type == "datetime") {
        # transform date variables
        # transform date variables
        date_format <- rv$mdr[
          get("source_system_name") == db_name &
            get("key") == i &
            get("variable_name") == j,
          unique(get("constraints"))
        ]
        if (is.na(date_format) ||
            grepl("^\\s*$", date_format) ||
            is.null(jsonlite::fromJSON(
              date_format
            )[["datetime"]][["format"]])) {
          # set date format to default value
          date_format <- "%Y-%m-%d"
        } else{
          date_format <- jsonlite::fromJSON(
            date_format
          )[["datetime"]][["format"]]
        }
        outlist[[i]][["outdata"]][, (j) := as.Date(
          as.character(get(j)),
          format = date_format
        )]
      } else if (var_type %in% c("integer", "float")) {
        # transform numeric variables
        outlist[[i]][["outdata"]][, (j) := as.numeric(
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
#' @return A list with the fields '$outdata' and if testing an SQL-based
#'   database also '$sql_statements'.
#'
#' @inheritParams load_csv
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' rv <- list()
#' rv$mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename = mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' rv <- c(rv, create_helper_vars(
#'   mdr = rv$mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' ))
#' # save source/target vars
#' rv$source$system_name <- source_system_name
#' rv$target$system_name <- target_system_name
#' rv$source$system_type <- "csv"
#' rv$target$system_type <- "csv"
#'
#' rv$log$logfile_dir <- tempdir()
#'
#' # set headless (without GUI, progressbars, etc.)
#' rv$headless <- TRUE
#'
#' # set configs
#' demo_files <- system.file("demo_data", package = "DQAstats")
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)
#'
#' # get configs
#' rv$source$settings <- DIZutils::get_config_env(
#'   system_name = rv$source$system_name,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#' rv$target$settings <- DIZutils::get_config_env(
#'   system_name = tolower(rv$target$system_name),
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#' # set start_time (e.g. when clicking the 'Load Data'-button in shiny
#' rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
#'
#' # define restricting date
#' rv$restricting_date$use_it <- FALSE
#'
#' data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
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
      is.list(system$settings) &
      ifelse(system$system_type == "csv", TRUE, length(system$settings) > 0),
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
    ## Get path to csv files from environment or variable:
    env_var_name <- paste0(toupper(system$system_name), "_PATH")
    if (dir.exists(Sys.getenv(env_var_name))) {
      system$settings$path <- Sys.getenv(env_var_name)
      DIZtools::feedback(
        print_this = paste0(
          "Found the path to the csv files in the environment: '",
          system$settings$path
        ),
        headless = rv$headless,
        findme = "d45dad8b72"
      )
    } else if (dir.exists(system$settings$path)) {
      DIZtools::feedback(
        print_this = paste0(
          "Found the path to the csv files in 'system$settings$path': '",
          system$settings$path,
          "'. Environment variable ",
          env_var_name,
          " was '",
          Sys.getenv(env_var_name),
          "'."
        ),
        headless = rv$headless,
        findme = "46a2f26236"
      )
    } else {
      DIZtools::feedback(
        print_this = paste0(
          "No existing path to the csv files could be found in",
          "'system$settings$path' (=",
          system$settings$path,
          ") or in the environment (='",
          Sys.getenv(paste0(toupper(
            system$system_name
          ), "_PATH")),
          "').",
          system$settings$path
        ),
        headless = rv$headless,
        findme = "cf220c689f",
        type = "Error"
      )
      stop("See error above.")
    }
    rm(env_var_name)
    stopifnot(nchar(system$settings$path) > 0)

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

  } else if (system$system_type %in% c("oracle", "postgres")) {
    if (system$system_type == "postgres") {
      # import target SQL
      sql_statements <- load_sqls(utils_path = rv$utilspath,
                                          db = system$system_name)
      stopifnot(is.list(sql_statements))

      # test target_db
      if (is.null(system$settings)) {
        ## Use environment-settings:
        db_con <-
          DIZutils::db_connection(
            system_name = system$system_name,
            db_type = system$system_type,
            headless = rv$headless,
            logfile_dir = rv$log$logfile_dir
          )
      } else {
        ## Use included settings:
        db_con <-
          DIZutils::db_connection(
            system_name = system$system_name,
            db_type = system$system_type,
            headless = rv$headless,
            logfile_dir = rv$log$logfile_dir,
            from_env = FALSE,
            settings = system$settings
          )
      }
      stopifnot(!is.null(db_con))

    }  else if (system$system_type == "oracle") {
      # import target SQL
      sql_statements <- load_sqls(utils_path = rv$utilspath,
                                          db = system$system_name)
      stopifnot(is.list(sql_statements))

      # test target_db
      if (is.null(system$settings)) {
        ## Use environment-settings:
        db_con <-
          DIZutils::db_connection(
            system_name = system$system_name,
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
            system_name = system$system_name,
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
    }
    # load target data
    loaded_from_db <- load_database(
      rv = rv,
      sql_statements = sql_statements,
      db_con = db_con,
      keys_to_test = keys_to_test,
      headless = rv$headless,
      db_name = system$system_name,
      db_type = system$system_type
    )
    outlist$outdata <- sapply(
      X = names(loaded_from_db),
      FUN = function(x) {
        loaded_from_db[[x]][["outdata"]]
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    outlist$sql_statements <- sapply(
      X = names(loaded_from_db),
      FUN = function(x) {
        loaded_from_db[[x]][["sql_statements"]]
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    rm(db_con)

  } else {
    stop("\nThis source_system_type is currently not implemented.\n\n")
  }
  return(outlist)
}
