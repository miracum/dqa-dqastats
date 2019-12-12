# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019 Universitätsklinikum Erlangen
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

context("test import names and settings")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/DQAstats/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
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

test_that("correct functioning of importing names and settings", {
  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"
  config_file <- settings
  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(prefix,
                       "output/")

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  # get configs
  rv$source$settings <- get_config(config_file = config_file,
                                   config_key = tolower(rv$source$system_name))
  rv$target$settings <- get_config(config_file = config_file,
                                   config_key = tolower(rv$target$system_name))

  expect_true(!is.null(rv$source$settings$dir))
  expect_true(!is.null(rv$target$settings$dir))

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # clean paths (to append the ending slash)
  rv$utilspath <- clean_path_name(utils_path)
  output_dir <- clean_path_name(output_dir)

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

  expect_type(rv$source$system_type, "character")
  expect_type(rv$target$system_type, "character")

  expect_length(rv$source, 3)
  expect_length(rv$target, 3)

  expect_length(rv$source$system_type, 1)
  expect_length(rv$target$system_type, 1)


  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = T)
  unlink(output_dir, recursive = T)
  file.remove(settings)
})