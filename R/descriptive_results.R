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

#' @title descriptive_results helper function
#'
#' @description Internal function to generate the descriptive results.
#'
#' @inheritParams load_csv
#' @inheritParams dqa
#'
#' @return A list with one entry for each dataelement containing the results of
#'   the descriptive results. Each entry contains the following (nested) list
#'   items:
#'   \describe{
#'   \item{description}{A nested list with the description of the dataelement
#'   for the source data system and the target data system.}
#'   \item{counts}{A nested list with the frequency count results for the
#'   source data system and the target data system.}
#'   \item{statistics}{A nested list with the descriptive results for the
#'   source data system and the target data system stored as data.table
#'   objects.}
#'   }
#'
#' @examples
#' \donttest{# runtime ~ 5 sec.
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
#' descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#' }
#'
#' @export
#'
descriptive_results <- function(rv,
                                headless = FALSE) {

  outlist <- future.apply::future_sapply(
    X = names(rv$variable_list),
    FUN = function(i) {
      local({
        # initialize outlist
        outlist <- list()

        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Getting variable descriptions of", i)
        DIZtools::feedback(
          msg,
          logjs = isFALSE(headless),
          findme = "eb95542ec1",
          logfile_dir = rv$log$logfile_dir,
          headless = headless
        )

        # generate descriptions
        desc_dat <- rv$mdr[
          get("dqa_assessment") == 1 &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name),
        ][
          get("variable_name") == rv$variable_list[[i]] &
            get("designation") == i,
          c(
            "designation",
            "source_system_name",
            "source_variable_name",
            "source_table_name",
            "variable_name",
            #"fhir",
            "definition",
            "variable_type",
            "constraints",
            #"value_threshold",
            #"missing_threshold",
            "filter"
          ), with = FALSE
        ]

        if (nrow(desc_dat) > 1 ||
            rv$source$system_name == rv$target$system_name) {
          outlist$description <- calc_description(desc_dat, rv)
        } else {
          msg <- paste0("Error occured during creating ",
                        "descriptions of source database")
          DIZtools::feedback(
            msg,
            logjs = isFALSE(headless),
            type = "Error",
            findme = "b640b3c662",
            logfile_dir = rv$log$logfile_dir,
            headless = headless
          )
          return()
        }

        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Calculating variable counts of", i)
        DIZtools::feedback(
          msg,
          logjs = isFALSE(headless),
          findme = "056f1ee2e0",
          logfile_dir = rv$log$logfile_dir,
          headless = headless
        )

        # generate counts
        cnt_dat <- rv$mdr[
          get("dqa_assessment") == 1 &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name),
        ][
          get("variable_name") == rv$variable_list[[i]] &
            get("designation") == i, c(
              "source_system_name",
              "source_variable_name",
              "source_table_name",
              "variable_type",
              "key",
              "variable_name",
              "filter"
            ), with = FALSE
        ]

        outlist$counts <- calc_counts(
          cnt_dat = cnt_dat,
          count_key = rv$variable_list[[i]],
          rv = rv,
          datamap = TRUE
        )


        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Calculating variable statistics of", i)
        DIZtools::feedback(
          msg,
          logjs = isFALSE(headless),
          findme = "edf4f006a9",
          logfile_dir = rv$log$logfile_dir,
          headless = headless
        )


        # generate statistics
        stat_dat <- rv$mdr[
          get("dqa_assessment") == 1 &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name),
        ][
          get("variable_name") == rv$variable_list[[i]] &
            get("designation") == i, c(
              "source_system_name",
              "source_variable_name",
              "source_table_name",
              "variable_type",
              "key",
              "variable_name",
              "filter"
            ), with = FALSE
        ]

        if (stat_dat[, unique(get("variable_type"))] %in%
            c("enumerated", "string")) {
          outlist$statistics <- calc_cat_stats(
            stat_dat = stat_dat,
            stat_key = rv$variable_list[[i]],
            rv = rv
          )
          # for target_data; our data is in rv$list_target$key
        } else {
          outlist$statistics <- calc_num_stats(
            stat_dat = stat_dat,
            stat_key = rv$variable_list[[i]],
            rv = rv
          )
        }
        return(outlist)
      })
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  gc()

  return(outlist)
}
