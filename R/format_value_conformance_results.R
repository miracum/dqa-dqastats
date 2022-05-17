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


#' @title format_value_conformance_results helper function
#'
#' @description Internal function to format the value conformance results
#'
#' @param results A list containing the value conformance results for one data
#'   element.
#' @param desc_out A list containing the descriptive results for the same data
#'   element.
#' @param source A character: either `source_data` or `target_data` to indicate,
#'   which results to render.
#'
#' @return The function returns a list with the formatted value conformance
#'   results for one data element.
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
#' # format the results (wrap functioncall into `sapply` to get results for all
#' # available data elements):
#' value_conformance_formatted <- sapply(
#'   X = names(rv$results_descriptive),
#'   FUN = function(i) {
#'     desc_out <- rv$results_descriptive[[i]]$description
#'
#'     if (!is.null(rv$conformance$value_conformance[[i]])) {
#'       format_value_conformance_results(
#'         results = rv$conformance$value_conformance[[i]],
#'         desc_out = desc_out,
#'         source = "source_data"
#'       )
#'     }
#'   }
#' )
#' @export

format_value_conformance_results <- function(results,
                                             desc_out,
                                             source) {
  outlist <- list(
    conformance_check = NULL,
    constraining_rules = NULL,
    kable = NULL,
    conformance_results = NULL
  )

  if (results[[source]]$conformance_results ==
      "No data available to perform conformance checks.") {
    outlist$conformance_check <- paste0(
      "Conformance check: ",
      results[[source]]$conformance_results
    )
  } else {

    outlist$conformance_check <- paste0(
      "Conformance check: ",
      ifelse(
        results[[source]]$conformance_error,
        "failed",
        "passed"
      ))

    # get value set
    if (!is.na(desc_out[[source]]$checks$constraints)) {
      json_obj <- jsonlite::fromJSON(
        desc_out[[source]]$checks$constraints
      )
    }

    if (desc_out[[source]]$checks$var_type ==
        "enumerated") {
      outlist$constraining_rules <- paste0("Constraining values/rules: '",
                                           paste(json_obj$value_set,
                                                 collapse = ", "),
                                           "'")

    } else if (desc_out[[source]]$checks$var_type ==
               "string") {
      outlist$constraining_rules <- paste0("Constraining values/rules: '",
                                           json_obj$regex, "'")

    } else if (desc_out[[source]]$checks$var_type %in%
               c("integer", "float")) {
      outlist$constraining_rules <- paste0("Constraining values/rules:")
      outlist$kable <- data.table::as.data.table(json_obj$range)

    } else if (desc_out[[source]]$checks$var_type ==
               "datetime") {
      rule <- results[[source]]$rule
      if (is.list(rule)) {
        outlist$constraining_rules <- paste0("Constraining values/rules:")
        outlist$kable <- data.table::as.data.table(rule)
      } else if (is.character(rule) && length(rule) == 1) {
        outlist$constraining_rules <- paste0(
          "Constraining values/rules: '", rule, "'")
      }
    }

    if (isTRUE(results[[source]]$conformance_error)) {
      outlist$conformance_results <- results[[source]]$conformance_results
    }
  }

  return(outlist)
}
