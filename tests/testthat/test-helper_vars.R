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

test_that("correct functioning of helper vars", {
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


  # We don't need this explicitely because it is tested later in the
  # 'rv'-object.
  expect_type(reactive_to_append, "list")
  expect_length(reactive_to_append, 5)
  expect_equal(reactive_to_append$keys_source, "dqa_example_data_01.csv")
  expect_equal(reactive_to_append$keys_target, "dqa_example_data_02.csv")
  expect_s3_class(reactive_to_append$dqa_assessment, "data.table")
  expect_equal(nrow(reactive_to_append$dqa_assessment), 12)
  expect_equal(ncol(reactive_to_append$dqa_assessment), 6)
  expect_type(reactive_to_append$variable_list, "list")
  expect_length(reactive_to_append$variable_list, 12)
  expect_type(reactive_to_append$pl, "list")
  expect_length(reactive_to_append$pl, 6)
  expect_s3_class(reactive_to_append$pl$atemp_vars, "data.table")
  expect_s3_class(reactive_to_append$pl$uniq_vars, "data.table")


  # workaround, to keep "rv" an reactiveValues object in shiny app
  #% (rv <- c(rv, reactive_to_append)) does not work!
  for (i in names(reactive_to_append)) {
    rv[[i]] <- reactive_to_append[[i]]
  }
  rm(reactive_to_append)
  invisible(gc())

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")

  expect_type(rv, "list")
  expect_length(rv, 14)
  expect_equal(rv$keys_source, "dqa_example_data_01.csv")
  expect_equal(rv$keys_target, "dqa_example_data_02.csv")
  expect_s3_class(rv$dqa_assessment, "data.table")
  expect_equal(nrow(rv$dqa_assessment), 12)
  expect_equal(ncol(rv$dqa_assessment), 6)
  expect_type(rv$variable_list, "list")
  expect_length(rv$variable_list, 12)
  expect_type(rv$pl, "list")
  expect_length(rv$pl, 6)
  expect_s3_class(rv$pl$atemp_vars, "data.table")
  expect_s3_class(rv$pl$uniq_vars, "data.table")


  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = T)
  unlink(output_dir, recursive = T)

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )
})
