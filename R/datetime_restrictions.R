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



#' @title Checking the mdr integrity for time restrictions
#'
#' @description Internal function to check if for every input table there is
#'   one single (or empty) column where to apply the time restriction to.
#'   If the input is valid, it will just print a success-message, if the
#'   data is invalid, the function will call `stop()`.
#'
#' @param mdr The mdr as data.table
#' @param enable_stop (Boolean, default = TRUE) If true (default) this function
#'   will call `stop()` in case of the check fails. If `enable_stop = FALSE`
#'   it will return `TRUE` if the check was successful and `FALSE` if the
#'   check failed. Use `enable_stop = FALSE` to avoid the need of a try/catch
#'   block around this function.
#' @param system_names (String) The name of the systems (source and target)
#'   to check for possible date restriction in the mdr.
#' @param headless (Boolean) Is this a console application? Otherwise
#'   (if `headless = FALSE`) there is a GUI and there will be GUI-feedback.
#' @return TRUE/FALSE: TRUE if the check was successful and the given
#'   systems can be time filtered, FALSE if something went wrong and no time
#'   filtering is possible.
#'
#' @inheritParams dqa
#'
#' @return A boolean to indicate if the date restriction requirements are met
#'   (TRUE) or not (FALSE).
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename = mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' DIZtools::cleanup_old_logfile(logfile_dir = tempdir())
#'
#' check_date_restriction_requirements(
#'   mdr = mdr,
#'   system_names = c(source_system_name, target_system_name),
#'   logfile_dir = tempdir(),
#'   headless = TRUE,
#'   enable_stop = TRUE
#' )
#'
#' @export
#'
check_date_restriction_requirements <- # nolint
  function(mdr,
           system_names,
           logfile_dir,
           headless = TRUE,
           enable_stop = TRUE) {
    colname_restricting_date_var <- "restricting_date_var"

    error <- FALSE

    if (!colname_restricting_date_var %in% colnames(mdr)) {
      error <- TRUE
      DIZtools::feedback(
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
          unique(
            mdr[get("source_system_name") == system_name,
                get("source_table_name")]
          )
        different_restricting_date_cols <- c() # nolint
        for (table in different_tables) {
          restricting_date_cols <-
            unique(
              mdr[get("source_table_name") == table,
                  get(colname_restricting_date_var)]
            )
          different_restricting_date_cols <- # nolint
            c(different_restricting_date_cols,
              restricting_date_cols)
          if (length(restricting_date_cols) != 1) {
            DIZtools::feedback(
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
        if (all(is.na(different_restricting_date_cols))) {
          error <- TRUE
          DIZtools::feedback(
            print_this = paste0(
              "You specified that you want to time-filter the input data.",
              " Unfortunatelly no column for applying time restriction to was",
              " found for database '",
              system_name,
              "' in the mdr. This might lead to false results, if source and",
              " target database are not identically filtered."
            ),
            type = "Warning",
            # ui = !headless,
            findme = "a178197913"
          )
        }
      }
    }


    if (!error) {
      DIZtools::feedback(
        print_this = paste0(
          "\U2714 Date restriction parameters ",
          "are valid in the MDR."
        ),
        logfile = logfile_dir,
        findme = "47da559fd2"
      )
      return(TRUE)
    } else {
      if (enable_stop) {
        stop("See above.")
      } else {
        return(FALSE)
      }
    }
  }

#' @title Time filtering of data.table or sql-strings.
#'
#' @description Internal function to filter the input data (or SQL) depending
#'   on provided time information. Sensitive to SQL dialects.
#'
#' @param data If system_type is a database, the sql-string goes here.
#'   If system_type is 'csv', the data.table of this csv goes here.
#'   Sensitive to SQL dialects.
#'
#' @param data If system_type is a database, the sql-string goes here.
#'   If system_type is 'csv', the data.table of this csv goes here.
#' @param key The key from the mdr.
#' @param lower_limit The posixct timestamp of the lower filtering boundary.
#' @param upper_limit The posixct timestamp of the upper filtering boundary.
#' @param system_name (Optional for non-database-changes)
#'   'i2b2'/'p21csv'/'omop'/...
#' @param system_type 'postgres'/'oracle'/'csv'
#' @param mdr (Optional for non-database-changes) The internal MDR
#'   (get it from `rv$mdr`)
#' @param logfile_dir (Optional) The directory to store the logfile in.
#'   Defaults to NULL.
#' @param db_con (Optional for non-database-changes) The connection to the
#'   database. Used to create the views we need later to apply the SQLs to.
#' @param sql_create_view_all (Optional, list). A list containing the SQLs to
#'   create all Views for the time-filtering. This is needed for the
#'   printing-friendly SQL including this view-creating SQLs and the actual
#'   data-extracting SQL query.
#'
#' @return If system_type is a database, a list with the new sql-string
#'   containing the temporal filtering will be returned under $sql
#'  ('order by' parts will be removed) and a printable sql containing the
#'  commands to create the view needed to run the sql under $sql_extended.
#'  If system_type is 'csv', the filtered data.table will be returned.
#'
apply_time_restriciton <- function(data,
                                   # filter_colname,
                                   key,
                                   lower_limit,
                                   upper_limit,
                                   system_name = NULL,
                                   system_type,
                                   mdr,
                                   logfile_dir = NULL,
                                   db_con = NULL,
                                   sql_create_view_all = list()) {


  if (system_type == "csv") {
    filter_colname <- unique(
      mdr[get("source_table_name") == system_name &
            get("dqa_assessment") == 1, get("restricting_date_var")]
    )

    if (is.na(filter_colname)) {
      DIZtools::feedback(
        print_this = paste0("No filter-column specified for key '",
                            key, "'. Skipping."),
        findme = "3d04f6de77",
        logfile_dir = logfile_dir
      )
      return(data)
    }
    ## Format the filter-column as posixct:
    colname_tmp <- "__TMP_FILTER__"
    format_params <- tryCatch({
      unique(mdr[get("source_table_name") == system_name,
                 .SD,
                 .SDcols = c(
                   "source_table_name",
                   "restricting_date_var",
                   "restricting_date_format"
                 )])[["restricting_date_format"]]
    },
    error = function(cond) {
      DIZtools::feedback(
        print_this = paste0(
          "Error while trying to extract the format parameters for applying",
          " date restriction to the data. Will try to parse the date format",
          " automatically. This is the original error message: ",
          cond
        ),
        type = "Warning",
        findme = "86e635027a",
        logfile_dir = logfile_dir
      )
      return(NA)
    })

    if (is.na(format_params)) {
      ## We need to guess the format:
      data[, (colname_tmp) := parsedate::parse_date(
        dates = data[, get(filter_colname)],
        approx = FALSE
      )]
    } else {
      ## Puuh - we can use the user-provided timestamp-parameters:
      data[, (colname_tmp) := as.POSIXct(
        x = get(filter_colname),
        format = format_params
      )]
    }

    ## Apply the filter:
    data <-
      data[get(colname_tmp) >= lower_limit &
             get(colname_tmp) <= upper_limit]
    data[, (colname_tmp) := NULL]
    res <- data
    return(res)
  } else if (system_type %in% c("postgres", "oracle")) {
    if (is.null(system_name) || is.null(mdr) || is.null(db_con)) {
      DIZtools::feedback(
        print_this = paste0(
          "At least one of the necessary input parameters was missing."
        ),
        type = "Error",
        findme = "60a301773a",
        logfile_dir = logfile_dir
      )
      stop("See error above")
    }

    sql_unfiltered <- data

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

    # nolint start
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
    #       format(x = lower_limit, format = "%d-%m-%Y %H:%M:%S"),
    #       "', 'dd-mm-yyyy hh24:mi:ss')",
    #       " and ",
    #       filter_colname,
    #       " <= to_timestamp('",
    #       format(x = upper_limit, format = "%d-%m-%Y %H:%M:%S"),
    #       "', 'dd-mm-yyyy hh24:mi:ss')"
    #     )
    # }
    # nolint end

    ## Get all tables needed for this SQL:
    tables <- mdr[get("source_system_name") == system_name,
            .SD,
            .SDcols = c("source_table_name",
                        "restricting_date_var",
                        "restricting_date_format")
        ] %>%
      unique()
    # ignore any where statements and stuff
    tables[
      ,
      ("source_table_name") := gsub(
        "^(\\S*)(\\s*WHERE.*)?$", "\\1", get("source_table_name")
      )
    ]
    tables <- tables %>%
      unique()
    if (nrow(tables) != length(unique(tables[["source_table_name"]]))) {
      DIZtools::feedback(
        print_this = paste0(
          "Expected exactly one unique 'restricting_date_var'",
          " for every table in database ",
          system_name,
          " but found these: "
        ),
        type = "Error",
        logfile_dir = logfile_dir,
        findme = "4ec2f96277"
      )
      stop("See error above")
    } else {
      ## Here all commands to create the views will be stored for later
      ## display in the GUI:
      if(is.null(sql_create_view_all) ||
         all(sapply(sql_create_view_all, function(x) {
           is.na(x) || is.nan(x)
         }))) {
        sql_create_view_all <- list()
      }

      for (table in tables$source_table_name) {
        if (is.na(tables[
          get("source_table_name") == table,
        ][["restricting_date_var"]])) {
          ## No time-restriction needed. Skip this and don't change the SQL.
          DIZtools::feedback(
            print_this = paste0("No filter-column specified for table '",
                                table, "'. Skipping."),
            findme = "1c3eb6499d",
            logfile_dir = logfile_dir
          )
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
          format_tmp <-
            tables[
              get("source_table_name") == table,
            ][["restricting_date_format"]]
          if (is.na(format_tmp)) {
            ## We assume that the column is formatted as timestamp,
            ## so we don't need to cast anything:
            timestamp_col_sql <-
              tables[
                get("source_table_name") == table,
                get("restricting_date_var")
              ]
          } else {
            ## We assume that the column is formatted as string and needs
            ## to be casted/formatted with the given format-string
            ## `format_tmp`:
            timestamp_col_sql <- paste0(
              "TO_TIMESTAMP(\"",
              tables[get("source_table_name") == table,
                     get("restricting_date_var")],
              "\", '",
              tables[get("source_table_name") == table,
                     get("restricting_date_format")],
              "')"
            )
          }
          sql_create_view <- paste0(
            "CREATE TEMPORARY VIEW ",
            view_name,
            " AS (SELECT * FROM ",
            table,
            " WHERE ",
            timestamp_col_sql,
            " >= timestamp '",
            format(x = lower_limit, format = "%Y-%m-%d %H:%M:%S"),
            "' AND ",
            timestamp_col_sql,
            " <= timestamp '",
            format(x = upper_limit, format = "%Y-%m-%d %H:%M:%S"),
            "')"
          )

          if (system_type == "oracle") {
            DIZtools::feedback(
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

          # store view in list
          sql_create_view_all[[view_name]] <- sql_create_view

          ## Create VIEW if it is not already created:
          if (isFALSE(DIZutils::check_if_table_exists(
            db_con = db_con,
            table_name = view_name
          ))) {
            DIZtools::feedback(
              print_this = paste0(
                "Didn't find a temporary VIEW for table '",
                table,
                "'. Creating it now using:\n",
                sql_create_view
              ),
              findme = "e1a20a8b94",
              logfile_dir = logfile_dir
            )

            ## Create the time-restricted VIEW:
            DIZutils::query_database(db_con = db_con,
                                     sql_statement = sql_create_view,
                                     no_result = TRUE)
          }
          ## Replace the original not-time-filtered table-calls from the
          ## SQL with the new time-filtered tables:
          sql_tmp <-
            gsub(
              pattern = paste0("(\\s)*FROM(\\s)*", table),
              replacement = paste0(" FROM ", view_name),
              x = sql_tmp,
              ignore.case = TRUE
              # fixed = TRUE # nolint
              ## Caution: If you enable 'fixed' here, 'ignore_case' will
              ## not be applied which leads to false results due to case
              ## sensitive column names!
            )

          DIZtools::feedback(
            print_this = paste0(
              "The SQL now uses the temporal filtered view:\n",
              sql_tmp
            ),
            type = "Info",
            findme = "789ass8f3f",
            logfile_dir = logfile_dir
          )
        }
      }
    }

    if(is.null(sql_create_view_all) ||
       all(sapply(sql_create_view_all, function(x) {
         is.na(x) || is.nan(x)
       }))) {
      DIZtools::feedback(
        print_this = paste0("Couldn't get information about ",
                            " the SQL views."),
        type = "Warning",
        findme = "9292c14a02"
      )
    }

    # nolint start
    # print("Old SQL:")
    # print(data)
    # print("New SQL:")
    # print(sql_tmp)
    # nolint end
    return(list(
      "sql" = sql_tmp,
      "sql_extended" = paste0(
        "-- Create the VIEWs:\n",
        paste(sql_create_view_all, collapse = ";\n"),
        ";\n\n",
        "-- The actual SQL to extract the data:\n",
        sql_tmp,
        ";\n\n",
        "-- If needed, drop the temporal VIEWs:\n",
        paste("DROP VIEW", names(sql_create_view_all), collapse = ";\n"),
        ";"
      ),
      "sql_create_view_all" = sql_create_view_all
    ))
  } else {
    return(NULL)
  }
}



#' @title Get a formatted string containing start and end time of the
#'   date restriction applied to the data.
#'
#' @description See title.
#'
#' @param restricting_date The list applied from rv$restricting_date
#' @param lang Language of the result. "de"/"en" (en = default).
#'   If language is not yet implemented, "en" is used.
#' @param date Should the date be included in the result string?
#' @param time Should the time be included in the result string?
#'
#' @return String containing start and end date obtained from the list of
#'   `restricting_date`.
#'
get_restricting_date_info <- function(restricting_date,
                                      lang = "en",
                                      date = TRUE,
                                      time = TRUE) {

  lang <- tolower(lang)
  res <- ""
  if (!is.null(restricting_date) &&
      !is.null(restricting_date$use_it) &&
      restricting_date$use_it == TRUE) {
    if (lang == "de") {
      prefix <- "Betrachteter Zeitraum: "
      separator <- " bis "
    } else {
      ## Default: lang = "en" (or not yet implemented):
      prefix <- "Period considered: "
      separator <- " to "
    }
    res <-
      paste0(
        prefix,
        DIZtools::format_posixct(
          x = restricting_date$start,
          lang = lang,
          date = date,
          time = time
        ),
        separator,
        DIZtools::format_posixct(
          x = restricting_date$end,
          lang = lang,
          date = date,
          time = time
        )
      )
  } else {
    if (lang == "de") {
      res <-
        paste0(
          "Keine zeitliche Einschr",
          intToUtf8("228"),
          "nkung. Alle vorliegenden Daten wurden analysiert"
        )
    } else {
      res <- "No time restriction. All available data were analysed"
    }
  }
  return(res)
}
