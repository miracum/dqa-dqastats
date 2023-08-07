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


#' @title time_compare helper function
#'
#' @description Internal function to calculate differences
#'  betwween source and target based on a timestamp comparison
#'
#' @param headless (Boolean) Is this a console application? Otherwise
#'   (if `headless = FALSE`) there is a GUI and there will be GUI-feedback.
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
time_compare <- function(data_target,
                        data_source,
                        logfile_dir,
                        headless = FALSE) {

  DIZtools::feedback(print_this = "Start the time_compare",
                     logjs = isFALSE(headless),
                     findme = "tc5a1dwezt",
                     logfile_dir = logfile_dir,
                     headless = headless)

  # get the items that have TIMESTAMP column (which means three columns)
  getTimestampItems <- function(data) {
    returnValues <- c(NULL)
    for (name in names(data)) {
      if (length(data[[name]]) == 3) {
        returnValues <- c(returnValues, name)
      }
    }
    return(returnValues)
  }

  source_items <- getTimestampItems(data_source)
  target_items <- getTimestampItems(data_target)

  # get the items that have a TIMESTAMP column in both databases
  items_toCheck <- intersect(source_items, target_items)

  # initialize a list for the results
  All_results <- list()

  for (item in items_toCheck) {

    # get the TIMESTAMP columns
    source_ts <- data_source[[item]]$TIMESTAMP
    target_ts <- data_target[[item]]$TIMESTAMP


    # check if the columns have the correct format:
    if (!lubridate::is.POSIXct(source_ts)
        || !lubridate::is.POSIXct(target_ts)) {
      DIZtools::feedback(print_this = paste0("TIMESTAMP columns are not",
                                             "in POSIXct format"),
                         logjs = isFALSE(headless),
                         type = "Error",
                         findme = "tcer1dc7zt",
                         logfile_dir = logfile_dir,
                         headless = headless)
      stop("\n TIMESTAMP columns are not in the correct format\n\n")
    }

    # convert them to characters for easy comparison
    source_ts <- as.character(source_ts)
    target_ts <- as.character(target_ts)

    # get a list with all Timestamps that are present in source and/or target
    all_ts <- unique(c(source_ts, target_ts))

    # group timestamps
    group_source_ts <- table(source_ts)
    group_target_ts <- table(target_ts)

    # initialize result list
    result_compare <- list()

    DIZtools::feedback(print_this = paste0("Start comparing timestamps of: ",
                                           item),
                       logjs = isFALSE(headless),
                       findme = "tcst1dwe12",
                       logfile_dir = logfile_dir,
                       headless = headless)

    # compare ToDo: needs to be faster
    for (date in all_ts) {

      # Get the count for the current name in listA and listB
      count_source_ts <- group_source_ts[date]
      count_target_ts <- group_target_ts[date]

      # If the date is not present  set count to 0
      if (is.na(count_source_ts)) {
        count_source_ts <- 0
      }

      if (is.na(count_target_ts)) {
        count_target_ts <- 0
      }

      # calculate the difference
      count_diff <- count_target_ts - count_source_ts

      if (count_diff != 0) {
        result_compare <- rbind(result_compare,
                            data.frame(DiffTimestamps = date,
                                      Count_source = count_source_ts,
                                      Count_target = count_target_ts,
                                      Count_diff = count_diff))
      }

    }




    DIZtools::feedback(print_this = "End of compare",
                       logjs = isFALSE(headless),
                       findme = "tcen1dwe67",
                       logfile_dir = logfile_dir,
                       headless = headless)

    All_results[[item]] <- result_compare
  }

  #ToDo: add Data to the Timestamps

  # # initialize output table
  # result_table <- data.table::data.table(
  #   "Time" = character(0),
  #   "Count Diff" = character(0),
  #   "source Value1" = character(0),
  #   "source Value2" = character(0),
  #   "target Value1" = character(0),
  #   "target Value2" = character(0)
  # )

  return(All_results)
}


