# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2020 Universitätsklinikum Erlangen
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

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/DQAstats/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else {
  prefix <- "./"
}

settings <- paste0(prefix, "tests/testthat/test_settings.yml")
file.copy(settings,
          paste0(prefix, "tests/testthat/test_settings_use.yml"),
          overwrite = T)
settings <- paste0(prefix, "tests/testthat/test_settings_use.yml")
tx  <- readLines(settings)
tx2  <- gsub(
  pattern = "replace_me",
  replacement = paste0("\"",
                       system.file("demo_data", package = "DQAstats"),
                       "\""),
  x = tx
)
writeLines(tx2, con = settings)

library(data.table)

test_that("correct functioning of DQA", {

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"
  config_file <- settings
  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(prefix,
                       "output/")


  ## Testfunction to test it all:
  all_results <- dqa(
    source_system_name = source_system_name,
    target_system_name = target_system_name,
    config_file = config_file,
    utils_path = utils_path,
    mdr_filename = mdr_filename,
    output_dir = output_dir,
    logfile_dir = paste0(prefix, "tests/testthat/")
  )

  expect_type(all_results, "list")
  expect_length(all_results, 23)

  outputfiles <- list.files(output_dir)
  expect_true("DQA_report.md" %in% outputfiles)
  expect_true(any(grepl("^DQA_report_([[:digit:]])+.pdf$", outputfiles)))
  expect_true(any(grepl("^DQA_report_([[:digit:]])+.tex$", outputfiles)))

  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = T)
  unlink(output_dir, recursive = T)
  file.remove(paste0(prefix, "tests/testthat/logfile.log"))
  file.remove(settings)
})
