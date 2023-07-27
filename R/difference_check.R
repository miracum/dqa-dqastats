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


#' @title difference_checks helper function
#'
#' @description Internal function to calculate differences
#'
#' @param results A list object. The list should contain the results
#'   'rv$results_descriptive'.
#'
#' @return A data.table with the difference in total, distinct, valid and ,
#'   missing values of source and target database. Result is represented as a
#'   string containing the absolute difference as well as the percentage
#'
#' @examples
#'  \donttest{# runtime ~ 5 sec.
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
#' difference_checks(results = rv$results_descriptive)
#'}
#'
#' @export
#'
#'
difference_checks <- function(results) {
  # get names
  obj_names <- names(results)

  # initialize output table
  out <- data.table::data.table(
    "Variable" = character(0),
    "Difference in Totals" = character(0),
    "Difference in Distincts" = character(0),
    "Difference in Valids" = character(0),
    "Difference in Missings" = character(0)
  )


  for (i in obj_names) {
    # check, if there are data available
    if (results[[i]]$counts$source_data$cnt$n == 0 &&
        results[[i]]$counts$target_data$cnt$n == 0) {
      check_total <- "no data available"
      check_distinct <- check_total
      check_valids <- check_total
      check_missings <- check_total

    } else {

      check_total <- calculate_difference(
        results[[i]]$counts$source_data$cnt$n,
        results[[i]]$counts$target_data$cnt$n
      )

      check_distinct <- calculate_difference(
        results[[i]]$counts$source_data$cnt$distinct,
          results[[i]]$counts$target_data$cnt$distinct
      )

      check_valids <- calculate_difference(
        results[[i]]$counts$source_data$cnt$valids,
          results[[i]]$counts$target_data$cnt$valids
      )
      check_missings <- calculate_difference(
        results[[i]]$counts$source_data$cnt$missings,
          results[[i]]$counts$target_data$cnt$missings
      )
    }

    out <- rbind(
      out,
      data.table::data.table(
        "Variable" = i,
        "Difference in Totals" = check_total,
        "Difference in Distincts" = check_distinct,
        "Difference in Valids" = check_valids,
        "Difference in Missings" = check_missings
      )
    )
  }
  return(out)
}


calculate_difference <- function(count_source, count_target) {

  result <- NULL
  absolut_diff <- count_target - count_source

  if (absolut_diff == 0) {

    result <- "no diff."

  } else {
    percent_diff <- (absolut_diff / count_source) * 100

    result  <- paste(absolut_diff, " (", signif(percent_diff, 2), "%)")
  }

  return(result)
}
