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

library(data.table)

test_that("correct functioning of dataloading", {
  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  demo_files <- system.file("demo_data", package = "DQAstats")
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)

  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(tempdir(),
                       "output/")

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  rv$log$logfile_dir <- tempdir()

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

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

  expect_true(!is.null(rv$source$settings$path))
  expect_true(!is.null(rv$target$settings$path))

  # clean paths (to append the ending slash)
  rv$utilspath <- DIZtools::clean_path_name(utils_path)
  output_dir <- DIZtools::clean_path_name(output_dir)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")


  # read MDR
  rv$mdr <- read_mdr(utils_path = rv$utilspath,
                     mdr_filename = rv$mdr_filename)


  # read system_types
  rv$source$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$source$system_name, unique(get("source_system_type"))]
  rv$target$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$target$system_name, unique(get("source_system_type"))]


  reactive_to_append <- create_helper_vars(
    mdr = rv$mdr,
    target_db = rv$target$system_name,
    source_db = rv$source$system_name
  )

  # workaround, to keep "rv" an reactiveValues object in shiny app
  #% (rv <- c(rv, reactive_to_append)) does not work!
  for (i in names(reactive_to_append)) {
    rv[[i]] <- reactive_to_append[[i]]
  }
  rm(reactive_to_append)
  invisible(gc())

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")

  # define restricting date
  rv$restricting_date$use_it <- FALSE

  # load source data:
  temp_dat <- data_loading(
    rv = rv,
    system = rv$source,
    keys_to_test = rv$keys_source
  )
  rv$data_source <- temp_dat$outdata
  rv$source$sql <- temp_dat$sql_statements
  rm(temp_dat)
  invisible(gc())

  # load target_data
  if (rv$target$system_name != rv$source$system_name) {
    # load target
    temp_dat <- data_loading(
      rv = rv,
      system = rv$target,
      keys_to_test = rv$keys_target
    )
    rv$data_target <- temp_dat$outdata
    rv$target$sql <- temp_dat$sql_statements
    rm(temp_dat)
    invisible(gc())
  } else {
    rv$data_target <- rv$data_source
  }


  expect_type(rv$data_source, "list")
  expect_length(rv$data_source, 1)

  expect_type(rv$data_target, "list")
  expect_length(rv$data_target, 1)

  expect_s3_class(rv$data_source$dqa_example_data_01.csv, "data.table")
  expect_equal(nrow(rv$data_source$dqa_example_data_01.csv), 23)
  expect_equal(ncol(rv$data_source$dqa_example_data_01.csv), 12)

  expect_s3_class(rv$data_target$dqa_example_data_02.csv, "data.table")
  expect_equal(nrow(rv$data_target$dqa_example_data_02.csv), 23)
  expect_equal(ncol(rv$data_target$dqa_example_data_02.csv), 12)

  expect_type(rv$source, "list")
  expect_type(rv$target, "list")


  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = TRUE)
  unlink(output_dir, recursive = TRUE)

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )
})
