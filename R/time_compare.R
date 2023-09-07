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

    DIZtools::feedback(print_this = paste0("Start comparing timestamps of: ",
                                           item),
                       logjs = isFALSE(headless),
                       findme = "tcst1dwe12",
                       logfile_dir = logfile_dir,
                       headless = headless)


    # get all the needed raw data
    source_item_all <- data_source[[item]]
    target_item_all <- data_target[[item]]


    # check if the TIMESTAMP columns have the correct format:
    if (!lubridate::is.POSIXct(source_item_all$TIMESTAMP)
        || !lubridate::is.POSIXct(target_item_all$TIMESTAMP)) {
      DIZtools::feedback(print_this = paste0("TIMESTAMP columns are not",
                                             "in POSIXct format"),
                         logjs = isFALSE(headless),
                         type = "Error",
                         findme = "tcer1dc7zt",
                         logfile_dir = logfile_dir,
                         headless = headless)
      stop("\n TIMESTAMP columns are not in the correct format\n\n")
    }

    # # convert TIMESTAMP columns to characters for easy comparison
    source_item_all$TIMESTAMP <- as.character(source_item_all$TIMESTAMP)
    target_item_all$TIMESTAMP <- as.character(target_item_all$TIMESTAMP)

    # get a list with all timestamps that are present in source and/or target
    all_ts <- unique(c(source_item_all$TIMESTAMP, target_item_all$TIMESTAMP))

    # group timestamps
    group_source_ts <- table(source_item_all$TIMESTAMP)
    group_target_ts <- table(target_item_all$TIMESTAMP)


    # write a table with all the counts and set na values to 0
    Table_all <- data.frame(Time = all_ts)
    Table_all$Count_source <-
      group_source_ts[match(Table_all$Time, names(group_source_ts))]

    Table_all$Count_target <-
      group_target_ts[match(Table_all$Time, names(group_target_ts))]

    Table_all$Count_target[is.na(Table_all$Count_target)] <- 0
    Table_all$Count_source[is.na(Table_all$Count_source)] <- 0

    # calculate the differences
    Table_all$Diff_count <- Table_all$Count_target - Table_all$Count_source

    # create a result table with all data where the difference is not 0
    result_table <- subset(Table_all, Diff_count != 0)

    # filter the original data by the result timestamps using a filter column
    # (better solutions to accomplish this are welcome)

    source_item_all <- data.frame(source_item_all)
    source_item_all$filter <- source_item_all$TIMESTAMP %in% result_table$Time
    suspect_data_source <- subset(source_item_all, filter == TRUE)
    suspect_data_source$filter <- NULL

    target_item_all <- data.frame(target_item_all)
    target_item_all$filter <- target_item_all$TIMESTAMP %in% result_table$Time
    suspect_data_target <- subset(target_item_all, filter == TRUE)
    suspect_data_target$filter <- NULL

    # create a result vector with all the results
    results_item <- list(result_table = result_table,
                         suspect_data_source = suspect_data_source,
                         suspect_data_target = suspect_data_target)

    # add this to All_results
    All_results[[item]] <- results_item

  }

  DIZtools::feedback(print_this = "End of time-compare",
                     logjs = isFALSE(headless),
                     findme = "tcen1dwe67",
                     logfile_dir = logfile_dir,
                     headless = headless)

  return(All_results)
}


