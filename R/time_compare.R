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
#'  between source and target based on a timestamp comparison. It can help to
#'  identify potential missing resources.
#'
#' @return a list of time-compare results for each analyzed element.
#' For every element, there are three dataframes available. The first dataframe
#' (result_table), presents an overview table that displays the counts for each
#' timestamp. The other two dataframes (suspect_data_source and
#' suspect_data_target), contain all the data associated with the identified
#' timestamps found in the source or target data.
#'
#' @inheritParams load_csv
#' @inheritParams value_conformance

#' @examples
#'  \donttest{# runtime ~ 5 sec.
#' library(dplyr)
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
#' # time_compare
#' time_compare_results <- time_compare(
#'   rv = rv,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#'}
#'
#' @export
time_compare <- function(
    rv,
    logfile_dir,
    headless = FALSE
) {

  DIZtools::feedback(print_this = "Start the time_compare",
                     logjs = isFALSE(headless),
                     findme = "tc5a1dwezt",
                     logfile_dir = logfile_dir,
                     headless = headless)

  source_items <- get_timestamp_items(rv$data_source)
  target_items <- get_timestamp_items(rv$data_target)

  # get the items that have a TIMESTAMP column in both databases
  items_to_check <- intersect(source_items, target_items)

  # workaround for .csv files. If data is read from .csv the item key is the
  # filename. This one key contains all data. So if there are timestamps in both
  # files, do the compare.

  if ((rv$source$system_type == "csv") && (rv$target$system_type == "csv")) {
    if (!is.null(source_items) && (!is.null(target_items))) {
      items_to_check <- source_items[1]
    }
  }

  # initialize a list for the results
  all_results <- list()

  for (item in items_to_check) {

    DIZtools::feedback(
      print_this = paste0("Start comparing timestamps of: ",
                          item),
      logjs = isFALSE(headless),
      findme = "tcst1dwe12",
      logfile_dir = logfile_dir,
      headless = headless
    )


    # get all the needed raw data
    source_item_all <- rv$data_source[[item]]
    target_item_all <- rv$data_target[[item]]

    # workaround for .csv.files. Target data has another key, so just take
    # the first (and only) entry

    if ((rv$source$system_type == "csv") && (rv$target$system_type == "csv")) {
      target_item_all <- rv$data_target[[1]]
    }

    # check if the TIMESTAMP columns have the correct format:
    if (!inherits(source_item_all$TIMESTAMP, "POSIXct")
        || !inherits(target_item_all$TIMESTAMP, "POSIXct")) {
      DIZtools::feedback(
        print_this = paste0("TIMESTAMP columns are not",
                            "in POSIXct format"),
        logjs = isFALSE(headless),
        type = "Error",
        findme = "tcer1dc7zt",
        logfile_dir = logfile_dir,
        headless = headless
      )
      stop("\n TIMESTAMP columns are not in the correct format\n\n")
    }

    # convert TIMESTAMP columns to characters for easy comparison
    source_item_all$TIMESTAMP <- as.character(source_item_all$TIMESTAMP)
    target_item_all$TIMESTAMP <- as.character(target_item_all$TIMESTAMP)

    # get a list with all timestamps that are present in source and/or target
    all_ts <- unique(c(source_item_all$TIMESTAMP, target_item_all$TIMESTAMP))

    # group timestamps
    group_source_ts <- table(source_item_all$TIMESTAMP)
    group_target_ts <- table(target_item_all$TIMESTAMP)

    # write a table with all the counts and set na values to 0
    table_all <- data.table::data.table(Time = all_ts)
    table_all$Count_source <-
      group_source_ts[match(table_all$Time, names(group_source_ts))]

    table_all$Count_target <-
      group_target_ts[match(table_all$Time, names(group_target_ts))]

    table_all$Count_target[is.na(table_all$Count_target)] <- 0
    table_all$Count_source[is.na(table_all$Count_source)] <- 0

    # calculate the differences
    table_all$Diff_count <- table_all$Count_target - table_all$Count_source

    # create a result table with all data where the difference is not 0
    result_table <- subset(table_all, table_all$Diff_count != 0)

    # filter the original data by the result timestamps using a filter column
    source_item_all <- data.frame(source_item_all)
    source_item_all$filter <- source_item_all$TIMESTAMP %in% result_table$Time
    suspect_data_source <- subset(source_item_all, filter == TRUE)
    suspect_data_source$filter <- NULL

    target_item_all <- data.frame(target_item_all)
    target_item_all$filter <- target_item_all$TIMESTAMP %in% result_table$Time
    suspect_data_target <- subset(target_item_all, filter == TRUE)
    suspect_data_target$filter <- NULL

    #rearrange so that timestamp column is first
    suspect_data_source <- suspect_data_source[,
      c("TIMESTAMP", setdiff(names(suspect_data_source), "TIMESTAMP"))]

    suspect_data_target <- suspect_data_target[,
      c("TIMESTAMP", setdiff(names(suspect_data_target), "TIMESTAMP"))]

    #sort the TIMESTAMPS
    result_table <- result_table[order(result_table$Time), ]
    suspect_data_source <-
      suspect_data_source[order(suspect_data_source$TIMESTAMP), ]
    suspect_data_target <-
      suspect_data_target[order(suspect_data_target$TIMESTAMP), ]

    # if there are dates columns, convert them to character. This is needed
    # for a proper display later on
    date_columns_source <- sapply(suspect_data_source,
      function(x) inherits(x, "Date"))
    for (col in names(suspect_data_source)[date_columns_source]) {
      suspect_data_source[[col]] <- as.character(suspect_data_source[[col]])
    }
    date_columns_target <- sapply(suspect_data_target,
      function(x) inherits(x, "Date"))
    for (col in names(suspect_data_target)[date_columns_target]) {
      suspect_data_target[[col]] <- as.character(suspect_data_target[[col]])
    }

    # create a result vector with all the results
    results_item <- list(result_table = result_table,
                         suspect_data_source = suspect_data_source,
                         suspect_data_target = suspect_data_target)

    # add this to All_results
    all_results[[item]] <- results_item

  }

  DIZtools::feedback(
    print_this = "End of time-compare",
    logjs = isFALSE(headless),
    findme = "tcen1dwe67",
    logfile_dir = logfile_dir,
    headless = headless
  )

  return(all_results)
}

# get the items that have TIMESTAMP column
get_timestamp_items <- function(data) {
  return_values <- NULL
  for (name in names(data)) {
    if ("TIMESTAMP" %in% names(data[[name]]))  {
      return_values <- c(return_values, name)
    }
  }
  return(return_values)
}
