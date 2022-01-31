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

context("test DQA function")

library(data.table)

test_that("correct functioning of DQA", {

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  demo_files <- system.file("demo_data", package = "DQAstats")
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)

  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(tempdir(), "/output/")


  ## Testfunction to test it all:
  all_results <- dqa(
    source_system_name = source_system_name,
    target_system_name = target_system_name,
    utils_path = utils_path,
    mdr_filename = mdr_filename,
    output_dir = output_dir,
    logfile_dir = tempdir(),
    parallel = FALSE
  )

  expect_type(all_results, "list")
  expect_length(all_results, 26)

  if (tinytex::is_tinytex()) {

    outputfiles <- list.files(output_dir)
    expect_true("DQA_report.md" %in% outputfiles)
    expect_true(any(grepl("^DQA_report_([[:digit:]])+.pdf$", outputfiles)))
    expect_true(any(grepl("^DQA_report_([[:digit:]])+.tex$", outputfiles)))

  }

  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})
