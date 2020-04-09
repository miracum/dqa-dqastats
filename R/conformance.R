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


#' @title value_conformance helper function
#'
#' @description Internal function to perform value conformance checks.
#'
#' @inheritParams descriptive_results
#' @param results A list object. The list should contain the results of
#'   either 'rv$results_descriptive' or 'rv$results_plausibility_atemporal'.
#' @param scope A character. Either "plausibility" or "descriptive".
#' @inheritParams feedback
#'
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

  assignInMyNamespace(
    x = "%notin%",
    value = DIZutils:::`%notin%`
  )

  # get names
  obj_names <- names(results)

  # initialize final list to output
  outlist <- list()

  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's
    # an error
    on.exit(progress$close())
    progress$set(message = "Performing value conformance check",
                 value = 0)
  }

  # loop over objects
  for (i in obj_names) {
    msg <- paste("Performing value conformance check", i)
    feedback(paste0("", msg), logjs = isFALSE(headless), findme = "5d061425eb",
             logfile_dir = logfile_dir,
             headless = headless)
    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress$inc(
        1 / length(obj_names),
        detail = paste("... checking", i, "..."))
    }

    desc_out <- results[[i]]$description
    stat_out <- results[[i]]$statistics

    # internal variable name
    int_name <- desc_out$source_data$internal_variable_name

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
          c <- jsonlite::fromJSON(d_out$checks$constraints)
        }, error = function(e) {
          print(e)
          c <- NA
          c
        }, finally = function(f) {
          return(c)
        }
      )

      if (!is.na(constraints)) {
        if (length(constraints[[1]]) > 0) {
          # initialize outlist
          outlist2 <- list()

          # categorical treatment (value_set)
          if (d_out$checks$var_type == "permittedValues") {

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
                    paste(levels_results[levels_results %notin% constraints],
                          collapse = "  \n")
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
                  tab <- rv$dqa_assessment[get("designation") == i,
                                           get("key")]
                  ih <- desc_out$source_data$internal_variable_name
                  vec <- setdiff(
                    colnames(rv[[raw_data]][[tab]]),
                    ih
                  )
                }

                if (length(vec) != 1) {
                  msg <- paste("Error occured when trying to get",
                               "duplicates of", i)
                  feedback(paste0("", msg), logjs = isFALSE(headless),
                           findme = "5d05678955eb",
                           logfile_dir = logfile_dir,
                           headless = headless)
                  next
                }

                if (scope == "plausibility") {
                  outlist2$affected_ids <-
                    unique(
                      rv[[raw_data]][[i]][get(d_out$var_dependent) %notin%
                                            constraints, vec, with = F]
                    )
                } else if (scope == "descriptive") {
                  outlist2$affected_ids <-
                    unique(
                      rv[[raw_data]][[tab]][
                        get(ih) %notin% constraints, vec, with = F]
                    )
                }
              }
            }

            # numerics treatment (range: min, max, unit)
          } else if (d_out$checks$var_type %in% c("integer", "float")) {

            # set colnames (we need them here to correctly select the data)
            colnames(s_out) <- c("name", "value")

            if (any(is.na(s_out$value)) ||
                (s_out[1, get("value")] == "NaN")) {
              outlist2$conformance_error <- TRUE
              outlist2$conformance_results <-
                "No data available to perform conformance checks."
            } else {

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
                feedback(paste0(i, "/ ", j, ": result_min < range$min"),
                         findme = "21abaa37e2",
                         logfile_dir = logfile_dir,
                         headless = headless)
                error_flag <- TRUE
              }

              if (result_max > constraints$range$max) {
                feedback(paste0(i, "/ ", j, ": result_max > range$max"),
                         findme = "44264e3a64",
                         logfile_dir = logfile_dir,
                         headless = headless)
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
                  tab <- rv$dqa_assessment[get("designation") == i,
                                           get("key")]
                  ih <- desc_out$source_data$internal_variable_name
                  vec <- setdiff(
                    colnames(rv[[raw_data]][[tab]]),
                    ih
                  )
                }

                if (length(vec) != 1) {
                  msg <- paste("Error occured when trying to get",
                               "duplicates of", i)
                  feedback(paste0("", msg), logjs = isFALSE(headless),
                           findme = "5d05698563eb",
                           logfile_dir = logfile_dir,
                           headless = headless)
                  next
                }

                if (scope == "plausibility") {
                  outlist2$affected_ids <-
                    unique(
                      rv[[raw_data]][[i]][get(d_out$var_dependent) <
                                            result_min |
                                            get(d_out$var_dependent) >
                                            result_max, vec, with = F]
                    )
                } else if (scope == "descriptive") {
                  outlist2$affected_ids <-
                    unique(
                      rv[[raw_data]][[tab]][
                        get(ih) < result_min |
                          get(ih) >
                          result_max, vec, with = F]
                    )
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
                x = as.character(s_out[!is.na(get(int_name)), get(int_name)])
              )
              cnt_errors <- sum(errors)

              error_flag <- ifelse(cnt_errors > 0, TRUE, FALSE)

              outlist2$conformance_error <- error_flag
              outlist2$conformance_results <-
                ifelse(
                  isTRUE(error_flag),
                  paste0(
                    "Values that are not conform with regular expression:  \n",
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
                  tab <- rv$dqa_assessment[get("designation") == i,
                                           get("key")]
                  ih <- desc_out$source_data$internal_variable_name
                  vec <- setdiff(
                    colnames(rv[[raw_data]][[tab]]),
                    ih
                  )
                }

                if (length(vec) != 1) {
                  msg <- paste("Error occured when trying to get",
                               "duplicates of", i)
                  feedback(paste0("", msg), logjs = isFALSE(headless),
                           findme = "5d01111111eb",
                           logfile_dir = logfile_dir,
                           headless = headless)
                  next
                }

                if (scope == "plausibility") {
                  outlist2$affected_ids <-
                    unique(
                      rv[[raw_data]][[i]][!grepl(
                        pattern = pattern,
                        x = get(d_out$var_dependent)
                      ), vec, with = F]
                    )
                } else if (scope == "descriptive") {
                  outlist2$affected_ids <-
                    unique(
                      rv[[raw_data]][[tab]][!grepl(
                        pattern = pattern,
                        x = get(ih)
                      ), vec, with = F]
                    )
                }
              }
            }
          }
          outlist[[i]][[j]] <- outlist2
        }
      }
    }
  }

  if (isFALSE(headless)) {
    progress$close()
  }

  return(outlist)
}


#' @title value_conformance_checks helper function
#'
#' @description Internal function to perform value conformance checks.
#'
#' @param results A list object. The list should contain the results of
#'   the function \code{value_conformance}.
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
