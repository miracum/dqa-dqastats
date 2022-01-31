# DQAstats - Perform data quality assessment (DQA)
# of electronic health records (EHR)
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

context("test MDR function")

library(data.table)

test_that("correct functioning of MDR", {
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
  rv$utilspath <- DIZutils::clean_path_name(utils_path)
  output_dir <- DIZutils::clean_path_name(output_dir)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")


  # read MDR
  rv$mdr <- read_mdr(utils_path = rv$utilspath,
                     mdr_filename = rv$mdr_filename)


  expect_type(rv, "list")
  expect_length(rv, 8)

  expect_type(rv$mdr, "list")
  expect_equal(nrow(rv$mdr), 24)
  expect_true(ncol(rv$mdr) == 18)
  expect_s3_class(rv$mdr, "data.table")

  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = T)
  unlink(output_dir, recursive = T)
})
