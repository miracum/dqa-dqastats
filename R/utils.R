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

# needed for markdown formating
kable_table <- function(data) {
  if (" " %in% colnames(data)) {
    return(
      knitr::kable(
        data,
        digits = 3,
        format = "latex",
        col.names = NULL
      ) %>%
        kableExtra::kable_styling(full_width = F)
    )
  } else {
    return(
      knitr::kable(data,
                   digits = 3,
                   format = "latex") %>%
        kableExtra::row_spec(0, bold = TRUE) %>%
        kableExtra::kable_styling(full_width = F)
    )
  }
}


#' @title load_sqls helper function
#'
#' @description Internal function to load the SQL statements.
#'
#' @inheritParams dqa
#' @param db A character string. The name of the corresponding database.
#'
#' @export
#'
load_sqls <- function(utils_path, db) {
  return(jsonlite::fromJSON(paste0(utils_path, "SQL/SQL_", db, ".JSON")))
}


get_where_filter <- function(filter) {
  return(jsonlite::fromJSON(filter))
}


#' @title parallel helper function
#'
#' @description Internal function to initialize the parallel backend.
#'
#' @inheritParams dqa
#'
#' @export
parallel <- function(parallel, logfile_dir, ncores) {
  if (isTRUE(parallel) && future::availableCores() > 1) {
    if (ncores < future::availableCores()) {
      ncores <- future::availableCores()
    }

    if (.Platform$OS.type == "unix") {
      DIZutils::feedback(
        "using future::plan(\"multicore\")",
        logjs = FALSE,
        findme = "0888fa600d",
        logfile_dir = logfile_dir,
        headless = TRUE
      )
      suppressWarnings(future::plan("multicore", worker = ncores))

    } else {
      DIZutils::feedback(
        "using future::plan(\"multisession\")",
        logjs = FALSE,
        findme = "0888fa600d",
        logfile_dir = logfile_dir,
        headless = TRUE
      )
      suppressWarnings(future::plan("multisession", worker = ncores))
    }
  } else {
    DIZutils::feedback(
      "using future::plan(\"sequential\")",
      logjs = FALSE,
      findme = "0885ba600d",
      logfile_dir = logfile_dir,
      headless = TRUE
    )
    suppressWarnings(future::plan("sequential"))
  }
}

#' @title Checking the mdr integrity for time restrictions
#'
#' @description Internal function to check if for every input table there is
#'   one single (or empty) column where to apply the time restriction to.
#'   If the input is valid, it will just print a success-message, if the
#'   data is invalid, the function will call `stop()`.
#'
#' @param mdr The mdr as data.table
#' @param restriction_date (list) If `restriction_date$use_it == FALSE`,
#'   the result will always be true since it doesn't matter if the restriction
#'   parameters are valid, because we don't use them.
#' @inheritParams dqa
#'
check_date_restriction_requirements <-
  function(mdr,
           system_names,
           restricting_date,
           logfile_dir) {
    colname_restricting_date_var <- "restricting_date_var"
    if (restricting_date$use_it == FALSE) {
      return()
    }

    error <- FALSE

    if (!colname_restricting_date_var %in% colnames(mdr)) {
      error <- TRUE
      DIZutils::feedback(
        print_this = paste0(
          "Can't find column `",
          colname_restricting_date_var,
          "` in the mdr."
        ),
        type = "Error",
        logfile = logfile_dir,
        findme = "fd9413e7a0"
      )
    } else {
      for (system_name in system_names) {
        different_tables <-
          unique(mdr[get("source_system_name") == system_name][["source_table_name"]])
        for (table in different_tables) {
          restricting_date_cols <-
            unique(mdr[get("source_table_name") == table, get(colname_restricting_date_var)])
          if (length(restricting_date_cols) != 1) {
            DIZutils::feedback(
              print_this = paste0(
                "\U2718 Date restriction parameters are invalid in the MDR.",
                " Expected one (or empty) column per table where to apply",
                " date restriction to but found more for table '",
                table,
                "': ",
                paste(restricting_date_cols, collapse = ", ")
              ),
              logfile = logfile_dir,
              type = "Error",
              findme = "cf1148fd73"
            )
            error <- TRUE
          }
        }
      }
    }


    if (!error) {
      DIZutils::feedback(print_this = "\U2714 Date restriction parameters are valid in the MDR.",
                         logfile = logfile_dir,
                         findme = "47da559fd2")
    } else {
      stop("See above.")
    }
  }

#' @title Time filtering of data.table or sql-strings.
#'
#' @description Internal function to filter the input data (or SQL) depending
#'   on provided time information. Sensitive to SQL dialects.
#'
#' @param data If system_type is a database, the sql-string goes here.
#'   If system_type is 'csv', the data.table of this csv goes here.
#' @param filter_colname The name of the column to apply the time-filtering to.
#' @param lower_limit The posixct timestamp of the lower filtering boundary.
#' @param upper_limit The posixct timestamp of the upper filtering boundary.
#' @param system_name 'i2b2'/'p21csv'/'omop'/...
#' @param system_type 'postgres'/'oracle'/'csv'
#' @param mdr (Optional for non-database-changes) The internal MDR
#'   (get it from `rv$mdr`)
#' @param logfile_dir (Optional) The directory to store the logfile in.
#'   Defaults to NULL.
#' @param db_con (Optional for non-database-changes) The connection to the
#'   database. Used to create the views we need later to apply the SQLs to.
#'
#' @return If system_type is a database, the new sql-string containing the
#'   temporal filtering will be returned ('order by' parts will be removed).
#'   If system_type is 'csv', the filtered data.table will be returned.
#'
apply_time_restriciton <-
  function(data,
           filter_colname,
           lower_limit,
           upper_limit,
           system_name,
           system_type,
           mdr = NULL,
           logfile_dir = NULL,
           db_con = NULL) {
    if (system_type == "csv") {
      ## Format the filter-column as posixct:
      colname_tmp <- "__TMP_FILTER__"
      data[, (colname_tmp) := parsedate::parse_date(dates = data[, get(filter_colname)])]

      ## Apply the filter:
      data <-
        data[get(colname_tmp) >= lower_limit &
               get(colname_tmp) <= upper_limit]
      data[, (colname_tmp) := NULL]
      res <- data
      return(res)
    } else if (system_type %in% c("postgres", "oracle")) {
      if (is.null(system_name) || is.null(mdr) || is.null(db_con)) {
        DIZutils::feedback(
          print_this = paste0("At least one of the necessary input parameters was missing."),
          type = "Error",
          findme = "60a301773a",
          logfile_dir = logfile_dir
        )
        stop("See error above")
      }

      sql_unfiltered <- tolower(data)

      ## Remove '\n', '\t' etc.:
      sql_tmp <-
        gsub(
          pattern = "\\s+",
          replacement = " ",
          x = sql_unfiltered,
          ignore.case = TRUE
        )

      ## Remove an 'order by' (and everything behind it) if existing:
      sql_tmp <- gsub(
        pattern = ".(order by)+.*",
        replacement = "",
        x = sql_tmp,
        ignore.case = TRUE
      )

      ## Remove tailing ';' (if existing):
      sql_tmp <- gsub(
        pattern = ";$",
        replacement = "",
        x = sql_tmp,
        ignore.case = TRUE
      )

      ## Add db-specific time filtering:
      ## (Switched to VIEWs, so we don't use this anymore)
      # if (system_type == "postgres") {
      #   sql_filtered <-
      #     paste0(
      #       sql_tmp,
      #       " and ",
      #       filter_colname,
      #       " >= timestamp '",
      #       format(x = lower_limit, format = "%Y-%m-%d %M:%S:00"),
      #       "' and ",
      #       filter_colname,
      #       " <= timestamp '",
      #       format(x = upper_limit, format = "%Y-%m-%d %M:%S:00"),
      #       "'"
      #     )
      # } else if (system_type == "oracle") {
      #   sql_filtered <-
      #     paste0(
      #       sql_tmp,
      #       " and ",
      #       filter_colname,
      #       " >= to_timestamp('",
      #       format(x = lower_limit, format = "%d-%m-%Y %M:%S:00"),
      #       "', 'dd-mm-yyyy hh24:mi:ss')",
      #       " and ",
      #       filter_colname,
      #       " <= to_timestamp('",
      #       format(x = upper_limit, format = "%d-%m-%Y %M:%S:00"),
      #       "', 'dd-mm-yyyy hh24:mi:ss')"
      #     )
      # }

      ## Get all tables needed for this SQL:
      tables <- unique(mdr[get("source_system_name") == system_name,
                           .SD,
                           .SDcols = c("source_table_name", "restricting_date_var")])
      if (nrow(tables) != length(unique(tables[["source_table_name"]]))) {
        DIZutils::feedback(
          print_this = paste0(
            "Expected exactly one unique 'restricting_date_var'",
            " for every table in system ",
            system_name,
            " but found these: "
          ),
          type = "Error",
          logfile_dir = logfile_dir,
          findme = "4ec2f96277"
        )
        print(tables)
        stop("See error above")
      } else {
        for (table in tables$source_table_name) {
          if (is.na(tables[get("source_table_name") == table][["restricting_date_var"]])) {
            ## No time-restriction needed. Skip this and don't change the SQL.
          } else {
            ## Remove db-scheme from table name
            ## 'scheme.tablename' --> 'tablename'
            view_name <-
              paste0(gsub(
                pattern = "(.)*(\\.)",
                replacement = "",
                x = table
              ),
              "__dqa_tmp")
            sql_create_view <- paste0(
              "CREATE TEMPORARY VIEW ",
              view_name,
              " AS (SELECT * FROM ",
              table,
              " WHERE ",
              tables[get("source_table_name") == table, get("restricting_date_var")],
              " >= timestamp '",
              format(x = lower_limit, format = "%Y-%m-%d %M:%S:00"),
              "' AND ",
              tables[get("source_table_name") == table, get("restricting_date_var")],
              " <= timestamp '",
              format(x = upper_limit, format = "%Y-%m-%d %M:%S:00"),
              "')"
            )
            if (system_type == "oracle") {
              DIZutils::feedback(
                print_this = paste0(
                  "VIEWs for oracle are not yet implemented.",
                  " Please implement. You find me here -->"
                ),
                type = "Error",
                findme = "ce1c5c6f3f",
                logfile_dir = logfile_dir
              )
              stop("See error above.")
            }
            ## Create VIEW if it is not already created:
            if (DIZutils::check_if_table_exists(db_con = db_con, table_name = view_name)) {
              DIZutils::feedback(
                print_this = paste0(
                  "Found a temporary VIEW for table '",
                  table,
                  "' which will be used now."
                ),
                findme = "dd695dbbe6",
                logfile_dir = logfile_dir
              )
              ## VIEW is already there. Normally we can be sure that this
              ## VIEW is the right one and not an older one, because we
              ## only create TEMP VIEWs which will automatically be removed
              ## when the connection closes.
              ## If you want to be sure, drop it and re-create it:
              sql_drop_view <- paste0("DROP VIEW ", view_name)
              ## Drop it:
              DIZutils::query_database(db_con = db_con, sql_statement = sql_drop_view)
              ## Re-create it:
              DIZutils::query_database(db_con = db_con, sql_statement = sql_create_view)
            } else {
              DIZutils::feedback(
                print_this = paste0(
                  "Did not find a temporary VIEW for table '",
                  table,
                  "'. Creating it now."
                ),
                findme = "e1a20a8b94",
                logfile_dir = logfile_dir
              )
              ## Create the time-restricted VIEW:
              DIZutils::query_database(db_con = db_con, sql_statement = sql_create_view)
            }
            ## Replace the original not-time-filtered table-calls from the
            ## SQL with the new time-filtered tables:
            sql_tmp <-
              gsub(
                pattern = paste0(" from ", table),
                replacement = paste0(" from ", view_name),
                x = sql_tmp,
                ignore.case = TRUE,
                fixed = TRUE
              )
          }
        }
      }
      print(sql_tmp)
      return(sql_tmp)
    } else {
      return(NULL)
    }
  }
