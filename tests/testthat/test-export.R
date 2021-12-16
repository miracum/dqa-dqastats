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

context("test export")

library(data.table)

test_that("correct functioning of export_aggregated", {


  utils_path <- system.file(
    "demo_data/utilities/",
    package = "DQAstats"
  )
  mdr_filename <- "mdr_example_data.csv"
  rv <- list()
  rv$mdr <- read_mdr(
    utils_path = utils_path,
    mdr_filename <- mdr_filename
  )

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  rv <- c(rv, create_helper_vars(
    mdr = rv$mdr,
    source_db = source_system_name,
    target_db = target_system_name
  ))
  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name
  rv$source$system_type <- "csv"
  rv$target$system_type <- "csv"

  rv$log$logfile_dir <- tempdir()

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # set configs
  demo_files <- system.file("demo_data", package = "DQAstats")
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)

  # get configs
  rv$source$settings <- DIZutils::get_config_env(
    system_name = rv$source$system_name,
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )
  rv$target$settings <- DIZutils::get_config_env(
    system_name = tolower(rv$target$system_name),
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = T, tz = "CET")

  # define restricting date
  rv$restricting_date$use_it <- FALSE

  # load source data
  tempdat <- data_loading(
    rv = rv,
    system = rv$source,
    keys_to_test = rv$keys_source
  )
  rv$data_source <- tempdat$outdata

  # load target data
  tempdat <- data_loading(
    rv = rv,
    system = rv$target,
    keys_to_test = rv$keys_target
  )
  rv$data_target <- tempdat$outdata

  rv$data_plausibility$atemporal <- get_atemp_plausis(
    rv = rv,
    atemp_vars = rv$pl$atemp_vars,
    mdr = rv$mdr,
    headless = rv$headless
  )

  # add the plausibility raw data to data_target and data_source
  for (i in names(rv$data_plausibility$atemporal)) {
    for (k in c("source_data", "target_data")) {
      w <- gsub("_data", "", k)
      raw_data <- paste0("data_", w)
      rv[[raw_data]][[i]] <-
        rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
      rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
    }
    gc()
  }

  # calculate descriptive results
  rv$results_descriptive <- descriptive_results(
    rv = rv,
    headless = rv$headless
  )

  # completeness
  rv$completeness <- completeness(results = rv$results_descriptive,
                                  headless = rv$headless,
                                  logfile_dir = rv$log$logfile_dir)

  rv$datamap <- generate_datamap(
    results = rv$results_descriptive,
    db = rv$target$system_name,
    mdr = rv$mdr,
    rv = rv,
    headless = rv$headless
  )

  # checks$value_conformance
  rv$checks$value_conformance <-
    value_conformance_checks(results = rv$conformance$value_conformance)

  # checks$etl
  rv$checks$etl <- etl_checks(results = rv$results_descriptive)

  output_dir <- tempdir()

  expect_silent(
    {
      export_aggregated(
        output_dir = output_dir,
        rv = rv
      )
    }
  )

})






test_that("correct functioning of export_aggregated", {


  utils_path <- system.file(
    "demo_data/utilities/",
    package = "DQAstats"
  )
  mdr_filename <- "mdr_example_data.csv"
  rv <- list()
  rv$mdr <- read_mdr(
    utils_path = utils_path,
    mdr_filename <- mdr_filename
  )

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  rv <- c(rv, create_helper_vars(
    mdr = rv$mdr,
    source_db = source_system_name,
    target_db = target_system_name
  ))
  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name
  rv$source$system_type <- "csv"
  rv$target$system_type <- "csv"

  rv$log$logfile_dir <- tempdir()

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # set configs
  demo_files <- system.file("demo_data", package = "DQAstats")
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)

  # get configs
  rv$source$settings <- DIZutils::get_config_env(
    system_name = rv$source$system_name,
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )
  rv$target$settings <- DIZutils::get_config_env(
    system_name = tolower(rv$target$system_name),
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = T, tz = "CET")

  # define restricting date
  rv$restricting_date$use_it <- FALSE

  # load source data
  tempdat <- data_loading(
    rv = rv,
    system = rv$source,
    keys_to_test = rv$keys_source
  )
  rv$data_source <- tempdat$outdata

  # load target data
  tempdat <- data_loading(
    rv = rv,
    system = rv$target,
    keys_to_test = rv$keys_target
  )
  rv$data_target <- tempdat$outdata

  rv$data_plausibility$atemporal <- get_atemp_plausis(
    rv = rv,
    atemp_vars = rv$pl$atemp_vars,
    mdr = rv$mdr,
    headless = rv$headless
  )

  # add the plausibility raw data to data_target and data_source
  for (i in names(rv$data_plausibility$atemporal)) {
    for (k in c("source_data", "target_data")) {
      w <- gsub("_data", "", k)
      raw_data <- paste0("data_", w)
      rv[[raw_data]][[i]] <-
        rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
      rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
    }
    gc()
  }

  # calculate descriptive results
  rv$results_descriptive <- descriptive_results(
    rv = rv,
    headless = rv$headless
  )

  # calculate unique plausibilites
  rv$results_plausibility_unique <- uniq_plausi_results(
    rv = rv,
    uniq_vars = rv$pl$uniq_vars,
    mdr = rv$mdr,
    headless = rv$headless
  )

  output_dir <- tempdir()
  export_affected_ids(
   rv = rv,
   output_dir = output_dir,
   object = rv$results_plausibility_unique
  )
})
