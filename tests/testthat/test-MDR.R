# DQAstats - Perform data quality assessment (DQA) of electronic health records (EHR)
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

context("test MDR function")

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
  replace = paste0("\"",
                   paste0(prefix, "inst/demo_data/"),
                   "\""),
  x = tx
)
writeLines(tx2, con = settings)

library(data.table)

test_that("correct functioning of MDR", {
  source_system_name = "exampleCSV_source"
  target_system_name = "exampleCSV_target"
  config_file = settings
  utils_path = paste0(prefix,
                      "inst/demo_data/utilities")
  mdr_filename = "mdr_example_data.csv"
  output_dir = paste0(prefix,
                      "output/")


  ## --- Workflow to test: --- ##
  if (missing(target_system_name)) {
    target_system_name <- source_system_name
  }

  stopifnot(
    is.character(source_system_name),
    is.character(target_system_name),
    is.character(config_file),
    is.character(utils_path),
    is.character(mdr_filename),
    is.character(output_dir)
  )

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
  stopifnot(data.table::is.data.table(rv$mdr))


  ## --- Things we expect from a successfull run: --- ##
  # Stuff concerning the 'rv'-object:
  expect_type(rv, "list")
  expect_length(rv, 7)
  expect_known_hash(rv, "2166edee5cf84b0e11de084ff87f4")

  # Stuff concerning the 'rv$mdr'-object:
  expect_type(rv$mdr, "list")
  expect_length(rv$mdr, 37)
  expect_known_hash(rv$mdr, "0be3d9590f84e11d907a1b8241311")






  # Remove the settings and output-folder:
  unlink(output_dir, recursive = T)
  file.remove(settings)
})
