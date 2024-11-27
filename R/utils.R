# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2024 Universitätsklinikum Erlangen
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
        kableExtra::kable_styling(full_width = FALSE,
                                  latex_options = "HOLD_position")
    )
  } else {
    return(
      knitr::kable(data,
                   digits = 3,
                   format = "latex") %>%
        kableExtra::row_spec(0, bold = TRUE) %>%
        kableExtra::kable_styling(full_width = FALSE,
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
#' @return No return value. Depending on the specified arguments, this function
#'   enables a parallel backend for faster computations.
#'
#' @examples
#' parallel(parallel = FALSE, logfile_dir = tempdir(), ncores = 1)
#'
#' @export
parallel <- function(parallel, logfile_dir, ncores) {
  if (isTRUE(parallel) && future::availableCores() > 1) {
    if (ncores < future::availableCores()) {
      ncores <- future::availableCores() %>%
        unname()
    }

    if (.Platform$OS.type == "unix") {
      DIZtools::feedback(
        "using future::plan(\"multicore\")",
        logjs = FALSE,
        findme = "8e8817df64",
        logfile_dir = logfile_dir,
        headless = TRUE
      )

      fplan <- tryCatch(
        expr = {
          suppressWarnings(
            fplan <- future::plan("multicore", workers = ncores)
          )
          fplan
        }, error = function(e) {
          DIZtools::feedback(
            e,
            logjs = FALSE,
            findme = "8e8817df63",
            logfile_dir = logfile_dir,
            headless = TRUE
          )
          fplan <- "error"
          fplan
        }, finally = function(f) {
          fplan
        }
      )

    } else {
      DIZtools::feedback(
        "using future::plan(\"multisession\")",
        logjs = FALSE,
        findme = "d142855e3c",
        logfile_dir = logfile_dir,
        headless = TRUE
      )
      fplan <- tryCatch(
        expr = {
          suppressWarnings(
            fplan <- future::plan("multisession", workers = ncores)
          )
          fplan
        }, error = function(e) {
          DIZtools::feedback(
            e,
            logjs = FALSE,
            findme = "8e8817df62",
            logfile_dir = logfile_dir,
            headless = TRUE
          )
          fplan <- "error"
          fplan
        }, finally = function(f) {
          fplan
        }
      )
    }
  }

  if (isFALSE(parallel) || fplan == "error") {
    DIZtools::feedback(
      "using future::plan(\"sequential\")",
      logjs = FALSE,
      findme = "4294f43e54",
      logfile_dir = logfile_dir,
      headless = TRUE
    )
    suppressWarnings(
      fplan <- future::plan("sequential")
    )
  }
  # https://www.rdocumentation.org/packages/future/versions/1.24.0/topics/plan
  on.exit(future::plan(fplan), add = TRUE)
}

#' @title Checks if there is a LaTeX installation available
#'
#' @description Internal function to determine if a LaTeX installation is
#'   available. Used before creating/knitr-ing the PDF report.
#'
#' @inheritParams test_csv
#'
#' @return TRUE if there is a LaTeX installation, FALSE if not.
#'
#' @examples
#' is_latex_installed()
#'
#' @export
is_latex_installed <- function(logfile_dir = NULL,
                               headless = TRUE) {
  catch_msg <- paste0(
    "Something went wrong with tinytex.",
    " Is it installed correctly?",
    " Try reinstalling it by running ",
    "`remotes::update_packages('tinytex', upgrade = 'always')` ",
    "and `tinytex::install_tinytex()`\n\n",
    "!!! DQAstats is not able to render the PDF report !!!"
  )
  out <- tryCatch({
    # Just to highlight: if you want to use more than one
    # R expression in the "try" part then you'll have to
    # use curly brackets.
    # 'tryCatch()' will return the last evaluated expression
    # in case the "try" part was completed successfully
    if (tinytex::tinytex_root() != "" ||
        tinytex::is_tinytex()) {
      TRUE
    } else {
      FALSE
    }
  },
  error = function(cond) {
    DIZtools::feedback(
      print_this = catch_msg,
      type = "Error",
      findme = "7d26ce78e5",
      logfile_dir = logfile_dir,
      headless = headless
    )
    return(FALSE)
  },
  warning = function(cond) {
    DIZtools::feedback(
      print_this = catch_msg,
      type = "Warning",
      findme = "7d27e403ce",
      logfile_dir = logfile_dir,
      headless = headless
    )
    return(FALSE)
  })
  return(out)
}
