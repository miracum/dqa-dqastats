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


#' @title etl_checks helper function
#'
#' @description Internal function to perform etl conformance checks.
#'
#' @param results A list object. The list should contain the results
#'   'rv$results_descriptive'.
#'
#' @export
#'
# quick checks
etl_checks <- function(results) {
  # get names
  obj_names <- names(results)

  # initialize output table
  out <- data.table::data.table(
    "Variable" = character(0),
    "Check Distincts" = character(0),
    "Check Valids" = character(0),
    "Check Missings" = character(0)
  )


  for (i in obj_names) {
    check_distinct <- ifelse(
      results[[i]]$counts$source_data$cnt$distinct ==
        results[[i]]$counts$target_data$cnt$distinct,
      "passed",
      "failed"
    )
    check_valids <- ifelse(
      results[[i]]$counts$source_data$cnt$valids ==
        results[[i]]$counts$target_data$cnt$valids,
      "passed",
      "failed"
    )
    check_missings <- ifelse(
      results[[i]]$counts$source_data$cnt$missings ==
        results[[i]]$counts$target_data$cnt$missings,
      "passed",
      "failed"
    )
    out <- rbind(
      out,
      data.table::data.table(
        "Variable" = i,
        "Check Distincts" = check_distinct,
        "Check Valids" = check_valids,
        "Check Missings" = check_missings
      )
    )
  }
  return(out)
}
