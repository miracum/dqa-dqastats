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
        kableExtra::kable_styling(full_width = F,
                                  latex_options = "HOLD_position")
    )
  } else {
    return(
      knitr::kable(data,
                   digits = 3,
                   format = "latex") %>%
        kableExtra::row_spec(0, bold = TRUE) %>%
        kableExtra::kable_styling(full_width = F,
                                  latex_options = "HOLD_position")
    )
  }
  # info: https://stackoverflow.com/questions/53153537/
  # rmarkdown-setting-the-position-of-kable
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
        findme = "8e8817df64",
        logfile_dir = logfile_dir,
        headless = TRUE
      )
      suppressWarnings(future::plan("multicore", worker = ncores))

    } else {
      DIZutils::feedback(
        "using future::plan(\"multisession\")",
        logjs = FALSE,
        findme = "d142855e3c",
        logfile_dir = logfile_dir,
        headless = TRUE
      )
      suppressWarnings(future::plan("multisession", worker = ncores))
    }
  } else {
    DIZutils::feedback(
      "using future::plan(\"sequential\")",
      logjs = FALSE,
      findme = "4294f43e54",
      logfile_dir = logfile_dir,
      headless = TRUE
    )
    suppressWarnings(future::plan("sequential"))
  }
}
