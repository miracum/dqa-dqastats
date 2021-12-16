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


#' @title value_conformance helper function
#'
#' @description Internal function to perform value conformance checks.
#'
#' @inheritParams descriptive_results
#' @param results A list object. The list should contain the results of
#'   either 'rv$results_descriptive' or 'rv$results_plausibility_atemporal'.
#' @param scope A character. Either "plausibility" or "descriptive".
#' @inheritParams dqa
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
#'   mdr_filename <- mdr_filename
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
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' rv$data_plausibility$atemporal <- get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # add the plausibility raw data to data_target and data_source
#' for (i in names(rv$data_plausibility$atemporal)) {
#'   for (k in c("source_data", "target_data")) {
#'     w <- gsub("_data", "", k)
#'     raw_data <- paste0("data_", w)
#'     rv[[raw_data]][[i]] <-
#'       rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
#'     rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
#'   }
#'   gc()
#' }
#'
#' # calculate descriptive results
#' rv$results_descriptive <- descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # calculate atemporal plausibilites
#' rv$results_plausibility_atemporal <- atemp_plausi_results(
#'   rv = rv,
#'   atemp_vars = rv$data_plausibility$atemporal,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # calculate unique plausibilites
#' rv$results_plausibility_unique <- uniq_plausi_results(
#'   rv = rv,
#'   uniq_vars = rv$pl$uniq_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' value_conformance(
#'   rv = rv,
#'   scope = "descriptive",
#'   results = rv$results_descriptive,
#'   headless = rv$headless,
#'   logfile_dir = rv$log$logfile_dir
#' )
#'
#' @export
#'
value_conformance <- function(
  rv,
  results,
  scope,
  headless = FALSE,
  logfile_dir) {

  stopifnot(
    scope %in% c("plausibility", "descriptive")
  )

  # workaround until DIZutils is on CRAN
  # (when using 'importFrom DIZutils %notin%', error exists due to
  # referencing package in NAMESPACE but not as Import in DESCRIPTION)
  "%notin%" <- utils::getFromNamespace(
    x = "%notin%",
    ns = "DIZutils"
  )

  # get names
  obj_names <- names(results)

  outlist <- future.apply::future_sapply(
    X = obj_names,
    FUN = function(i) {
      local({
        # initialize inner outlist
        outlist <- list()

        msg <- paste0("Performing value conformance check for '", i, "'")
        DIZutils::feedback(
          print_this = msg,
          findme = "5d061425eb",
          logfile_dir = logfile_dir
        )

        desc_out <- results[[i]]$description
        stat_out <- results[[i]]$statistics

        # internal variable name
        int_name <- desc_out$source_data$internal_variable_name

        key_cols <- get_key_col(rv)
        key_col_name_src <- key_cols$source
        key_col_name_tar <- key_cols$target

        for (j in c("source_data", "target_data")) {
          d_out <- desc_out[[j]]
          s_out <- stat_out[[j]]

          if (j == "source_data") {
            raw_data <- "data_source"
          } else if (j == "target_data") {
            raw_data <- "data_target"
          }

          # parse constraints
          constraints <- tryCatch(
            expr = {
              if (is.null(d_out$checks$constraints) ||
                  is.na(d_out$checks$constraints)) {
                c <- NA
              } else {
                c <- jsonlite::fromJSON(d_out$checks$constraints)
              }
              c
            }, error = function(e) {
              DIZutils::feedback(
                print_this = paste0("Error in fromJSON in function",
                                    " `value_conformance()`: ", e),
                findme = "f269cc7211",
                type = "Error",
                logfile_dir = logfile_dir
              )
              c <- NA
              c
            }
          )

          if (j == "source_data") {
            tab <- rv$mdr[get("source_system_name") == rv$source$system_name &
                            get("designation") == i &
                            get("dqa_assessment") == 1, get(key_col_name_src)]
          } else {
            tab <- rv$mdr[get("source_system_name") == rv$target$system_name &
                            get("designation") == i &
                            get("dqa_assessment") == 1, get(key_col_name_tar)]
          }
          # internal_variable_name is equal for source and target
          # and only stored with source description
          ih <- desc_out$source_data$internal_variable_name

          # add logic to test for future dates by default, if no datetime
          # constraint is set
          if (d_out$checks$var_type == "datetime" &&
              is.na(constraints) &&
              scope == "descriptive") {
            constraints <- "future_dates"
          }

          if (any(!is.na(constraints))) {
            if (length(constraints[[1]]) > 0) {
              # initialize outlist
              outlist2 <- list()

              DIZutils::feedback(
                print_this = paste0(
                  "Analyzing variable '",
                  i,
                  "' with type '",
                  d_out$checks$var_type,
                  "' (",
                  j,
                  ")..."
                ),
                findme = "d9f44d5c97",
                logfile_dir = logfile_dir,
                headless = headless
              )

              # categorical treatment (value_set)
              if (d_out$checks$var_type == "enumerated") {

                if (((nrow(s_out) == 1) && is.na(s_out[[1, 1]])) ||
                    (nrow(s_out) == 0)) {
                  outlist2$conformance_error <- TRUE
                  outlist2$conformance_results <-
                    "No data available to perform conformance checks."
                } else {
                  # get valueset from mdr
                  constraints <-
                    unlist(strsplit(constraints$value_set, ", ", fixed = T))
                  # get levels from results
                  levels_results <-
                    s_out[, levels(get(colnames(s_out)[1]))]
                  # compare levels from results to constraints from valueset
                  #% (TRUE = constraint_error)
                  if (is.null(levels_results)) {
                    outlist2$conformance_error <- TRUE
                  } else {
                    outlist2$conformance_error <- any(levels_results %notin%
                                                        constraints)
                  }
                  # if TRUE, get those values, that do not fit
                  outlist2$conformance_results <-
                    ifelse(
                      isTRUE(outlist2$conformance_error),
                      paste0(
                        "Levels that are not conform with the value set:  \n",
                        paste(
                          levels_results[levels_results %notin% constraints],
                          collapse = "  \n"
                        )
                      ),
                      "No 'value conformance' issues found."
                    )

                  # return affected ids in case, if conformance_error = true
                  if (isTRUE(outlist2$conformance_error)) {
                    if (scope == "plausibility") {
                      vec <- setdiff(
                        colnames(rv[[raw_data]][[i]]),
                        d_out$var_dependent
                      )
                    } else if (scope == "descriptive") {
                      # does only work with tables with 2 columns (most
                      # probably not with CSV files)
                      vec <- setdiff(
                        colnames(rv[[raw_data]][[tab]]),
                        ih
                      )
                    }

                    if (length(vec) != 1) {
                      msg <- paste("Error occured when trying to get",
                                   "errorneous IDs of", i, "from", j)
                      DIZutils::feedback(
                        print_this = msg,
                        type = "Warning",
                        findme = "5d05678955eb",
                        logfile_dir = logfile_dir
                      )
                    } else {

                      if (scope == "plausibility") {
                        outlist2$affected_ids <- unique(
                          rv[[raw_data]][[i]][
                            get(d_out$var_dependent) %notin% constraints,
                            vec,
                            with = F
                          ]
                        )
                      } else if (scope == "descriptive") {
                        outlist2$affected_ids <- unique(
                          rv[[raw_data]][[tab]][
                            get(ih) %notin% constraints,
                            vec,
                            with = F
                          ]
                        )
                      }
                    }
                  }
                }

                # numerics treatment (range: min, max, unit)
              } else if (d_out$checks$var_type %in% c("integer", "float")) {

                if (is.null(s_out) || nrow(s_out) == 0 ||
                    (any(is.na(s_out[, 1])) ||
                     (s_out[1, 1] == "NaN"))) {
                  outlist2$conformance_error <- TRUE
                  outlist2$conformance_results <-
                    "No data available to perform conformance checks."
                } else {

                  # set colnames (we need them here to correctly
                  # select the data)
                  colnames(s_out) <- c("name", "value")

                  error_flag <- FALSE

                  # TODO add value_thresholds here as tolerance-/border zone
                  result_min <- as.numeric(
                    s_out[get("name") == "Minimum", get("value")]
                  )
                  result_max <- as.numeric(
                    s_out[get("name") == "Maximum", get("value")]
                  )

                  # compare levels from results to constraints from valueset
                  #% (TRUE = constraint_error)
                  if (result_min < constraints$range$min) {
                    DIZutils::feedback(
                      paste(i, "from", j, ": result_min < range$min"),
                      findme = "21abaa37e2",
                      logfile_dir = logfile_dir
                    )
                    error_flag <- TRUE
                  }

                  if (result_max > constraints$range$max) {
                    DIZutils::feedback(
                      paste(i, "from", j, ": result_max > range$max"),
                      findme = "44264e3a64",
                      logfile_dir = logfile_dir
                    )
                    error_flag <- TRUE
                  }

                  outlist2$conformance_error <- error_flag
                  outlist2$conformance_results <-
                    ifelse(
                      isTRUE(error_flag),
                      "Extrem values are not conform with constraints.",
                      "No 'value conformance' issues found."
                    )

                  if (isTRUE(outlist2$conformance_error)) {
                    if (scope == "plausibility") {
                      vec <- setdiff(
                        colnames(rv[[raw_data]][[i]]),
                        d_out$var_dependent
                      )
                    } else if (scope == "descriptive") {
                      # does only work with tables with 2 columns (most
                      # probably not with CSV files)
                      vec <- setdiff(
                        colnames(rv[[raw_data]][[tab]]),
                        ih
                      )
                    }

                    if (length(vec) != 1) {
                      msg <- paste("Error occured when trying to get",
                                   "errorneous IDs of", i, "from", j)
                      DIZutils::feedback(
                        print_this = msg,
                        type = "Warning",
                        findme = "5d05698563eb",
                        logfile_dir = logfile_dir
                      )
                    } else {
                      if (scope == "plausibility") {
                        outlist2$affected_ids <- unique(
                          rv[[raw_data]][[i]][
                            get(d_out$var_dependent) < constraints$range$min |
                              get(d_out$var_dependent) > constraints$range$max,
                            vec,
                            with = FALSE
                          ]
                        )
                      } else if (scope == "descriptive") {
                        outlist2$affected_ids <- unique(
                          rv[[raw_data]][[tab]][
                            get(ih) < constraints$range$min |
                              get(ih) > constraints$range$max,
                            vec,
                            with = FALSE
                          ]
                        )
                      }
                    }
                  }
                }

                # string treatment (regex)
              } else if (d_out$checks$var_type == "string") {

                if (((nrow(s_out) == 1) && is.na(s_out[[1, 1]])) ||
                    (nrow(s_out) == 0)) {
                  outlist2$conformance_error <- TRUE
                  outlist2$conformance_results <-
                    "No data available to perform conformance checks."
                } else {
                  # get regex-pattern
                  pattern <- constraints$regex

                  # returns the number of not matching items
                  errors <- !grepl(
                    pattern = pattern,
                    x = as.character(
                      s_out[!is.na(get(int_name)), get(int_name)]
                    )
                  )
                  cnt_errors <- sum(errors)

                  error_flag <- ifelse(cnt_errors > 0, TRUE, FALSE)

                  outlist2$conformance_error <- error_flag
                  outlist2$conformance_results <-
                    ifelse(
                      isTRUE(error_flag),
                      paste0(
                        "Values that are not conform with ",
                        "regular expression:  \n",
                        paste(as.character(s_out[!is.na(get(int_name)),
                                                 get(int_name)])[errors],
                              collapse = "  \n")
                      ),
                      "No 'value conformance' issues found."
                    )

                  if (isTRUE(outlist2$conformance_error)) {
                    if (scope == "plausibility") {
                      vec <- setdiff(
                        colnames(rv[[raw_data]][[i]]),
                        d_out$var_dependent
                      )
                    } else if (scope == "descriptive") {
                      # does only work with tables with 2 columns (most
                      # probably not with CSV files)
                      vec <- setdiff(
                        colnames(rv[[raw_data]][[tab]]),
                        ih
                      )
                    }

                    if (length(vec) != 1) {
                      msg <- paste("Error occured when trying to get",
                                   "errorneous IDs of", i, "from", j)
                      DIZutils::feedback(
                        print_this = msg,
                        type = "Warning",
                        findme = "5d01111111eb",
                        logfile_dir = logfile_dir
                      )
                    } else {

                      if (scope == "plausibility") {
                        outlist2$affected_ids <- unique(
                          rv[[raw_data]][[i]][
                            !grepl(
                              pattern = pattern,
                              x = get(d_out$var_dependent)
                            ),
                            vec,
                            with = FALSE
                          ]
                        )
                      } else if (scope == "descriptive") {
                        outlist2$affected_ids <- unique(
                          rv[[raw_data]][[tab]][
                            !grepl(
                              pattern = pattern,
                              x = get(ih)
                            ),
                            vec,
                            with = FALSE
                          ]
                        )
                      }
                    }
                  }
                }
              } else if (d_out$checks$var_type == "datetime") {
                if (((nrow(s_out) == 1) && is.na(s_out[[1, 1]])) ||
                    (nrow(s_out) == 0)) {
                  outlist2$conformance_error <- TRUE
                  outlist2$conformance_results <-
                    "No data available to perform conformance checks."
                } else if (constraints == "future_dates" &&
                           scope == "descriptive") {
                  # check for future dates
                  fut_dat <- rv[[raw_data]][[tab]][
                    get(ih) > Sys.Date(),
                  ]

                  error_flag <- ifelse(nrow(fut_dat) > 0, TRUE, FALSE)

                  outlist2$conformance_error <- error_flag
                  outlist2$rule <- "No future dates allowed."
                  outlist2$conformance_results <-
                    ifelse(
                      isTRUE(error_flag),
                      paste0(
                        "Values that are not conform with ",
                        "rule 'No future dates allowed.':  \n",
                        paste(
                          as.character(
                            unique(
                              fut_dat[, get(ih)]
                            )
                          ),
                          collapse = "  \n")
                      ),
                      "No 'value conformance' issues found."
                    )

                  if (isTRUE(outlist2$conformance_error)) {

                    vec <- setdiff(
                      colnames(rv[[raw_data]][[tab]]),
                      ih
                    )

                    outlist2$affected_ids <- unique(
                      fut_dat[
                        ,
                        vec,
                        with = FALSE
                      ]
                    )
                  }

                } else {
                  ## Check if there only is a datetime_format in the
                  ## constraints (this is implicitly already checked
                  ## in load_database(), search for 'datetime_format'):
                  constraints_names <- names(constraints)
                  if (length(constraints_names) == 1 &&
                        constraints_names[[1]] == "datetime_format") {
                    DIZutils::feedback(
                      print_this = paste0(
                        "Value conformance check for variable '",
                        i,
                        "' will be set to 'passed', because there is no other",
                        " constraint given next to the format of the date",
                        " which was already tested and applied due to the",
                        " data loading process."
                      ),
                      findme = "38486b5105",
                      logfile_dir = logfile_dir,
                      headless = headless
                    )
                  } else {
                    DIZutils::feedback(
                      print_this = paste0(
                        "Cannot perform value conformance check for variable '",
                        i,
                        "' because there is no logic implemented ",
                        "for constrains '",
                        paste(
                          constraints_names[
                            constraints_names != "datetime_format"
                          ], collapse = "', '"),
                        "'. Search for this value in the ",
                        "brackets in the code to implement:"
                      ),
                      type = "Warning",
                      findme = "4c83f9bb78",
                      logfile_dir = logfile_dir,
                      headless = headless
                    )
                  }
                  outlist2$conformance_error <- FALSE
                  outlist2$conformance_results <-
                    "No 'value conformance' issues found."
                }
              } else {
                DIZutils::feedback(
                  print_this = paste0(
                    "Cannot check ",
                    scope,
                    " value conformance for variable '",
                    i,
                    "' because there is no check for vartype '",
                    d_out$checks$var_type,
                    "' implemented yet."
                  ),
                  type = "Warning",
                  findme = "4817d8aec4",
                  logfile_dir = logfile_dir
                )
              }
              outlist[[j]] <- outlist2
            }
          } else {
            DIZutils::feedback(
              print_this = paste0(
                "Didn't perform value conformance checks, because there",
                " are no constraints in the MDR for designation '",
                i,
                "' (key = '",
                tab,
                "')."
              ),
              findme = "4b27c49aa9",
              logfile_dir = logfile_dir
            )
          }
        }
        if (length(outlist) > 0) {
          return(outlist)
        } else {
          return()
        }
      })
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  # which list elements have a valid value?
  vec <- sapply(
    X = outlist,
    FUN = function(x) {
      return(length(x))
    }
  )

  return(outlist[vec > 0])
}


#' @title value_conformance_checks helper function
#'
#' @description Internal function to perform value conformance checks.
#'
#' @param results A list object. The list should contain the results of
#'   the function \code{value_conformance}.
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
#'   mdr_filename <- mdr_filename
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
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' rv$data_plausibility$atemporal <- get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # add the plausibility raw data to data_target and data_source
#' for (i in names(rv$data_plausibility$atemporal)) {
#'   for (k in c("source_data", "target_data")) {
#'     w <- gsub("_data", "", k)
#'     raw_data <- paste0("data_", w)
#'     rv[[raw_data]][[i]] <-
#'       rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
#'     rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
#'   }
#'   gc()
#' }
#'
#' # calculate descriptive results
#' rv$results_descriptive <- descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # calculate atemporal plausibilites
#' rv$results_plausibility_atemporal <- atemp_plausi_results(
#'   rv = rv,
#'   atemp_vars = rv$data_plausibility$atemporal,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # calculate unique plausibilites
#' rv$results_plausibility_unique <- uniq_plausi_results(
#'   rv = rv,
#'   uniq_vars = rv$pl$uniq_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' rv$conformance$value_conformance <- value_conformance(
#'   rv = rv,
#'   scope = "descriptive",
#'   results = rv$results_descriptive,
#'   headless = rv$headless,
#'   logfile_dir = rv$log$logfile_dir
#' )
#'
#' value_conformance_checks(results = rv$conformance$value_conformance)
#'
#' @export
#'
value_conformance_checks <- function(results) {
  # get names
  obj_names <- names(results)

  # initialize output table
  out <- data.table::data.table(
    "Variable" = character(0),
    "Check Source Data" = character(0),
    "Check Target Data" = character(0)
  )


  for (i in obj_names) {
    error_source <-
      ifelse(
        !is.null(results[[i]]$source_data),
        ifelse(
          results[[i]]$source_data$conformance_error,
          "failed",
          "passed"
        ),
        "ERROR"
      )
    error_target <-
      ifelse(
        !is.null(results[[i]]$target_data),
        ifelse(
          results[[i]]$target_data$conformance_error,
          "failed",
          "passed"
        ),
        "ERROR"
      )
    out <- rbind(
      out,
      data.table::data.table(
        "Variable" = i,
        "Check Source Data" = error_source,
        "Check Target Data" = error_target
      )
    )
  }
  return(out)
}
