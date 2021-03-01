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
  function(mdr, restricting_date, logfile_dir) {
    if (restricting_date$use_it == FALSE) {
      return(TRUE)
    }

    error <- FALSE
    different_tables <- unique(mdr[["source_table_name"]])
    for (table in different_tables) {
      restricting_date_cols <-
        unique(mdr[get("source_table_name") == table, get("restricting_date_var")])
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
    if (!error) {
      DIZutils::feedback(print_this = "\U2714 Date restriction parameters are valid in the MDR.",
                         logfile = logfile_dir,
                         findme = "47da559fd2")
    } else {
      stop("See above.")
    }
  }

apply_time_restriciton <-
  function(data,
           filter_colname,
           lower_limit,
           upper_limit) {
    ## Format the filter-column as posixct:
    colname_tmp <- "__TMP_FILTER__"
    data[, (colname_tmp) := parsedate::parse_date(dates = data[, get(filter_colname)])]

    ## Apply the filter:
    data <-
      data[get(colname_tmp) >= lower_limit &
             get(colname_tmp) <= upper_limit]
    data[, (colname_tmp) := NULL]

    res <- data

    print("Filtered the data.")

    return(res)
  }
