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

context("test atemporal plausibilities")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/DQAstats/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
  prefix <- "./"
}

library(data.table)

test_that("correct functioning of atemporal plausibilities", {

  # TODO
  skip("not yet implemented")

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  demo_files <- system.file("demo_data", package = "DQAstats")
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)

  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(prefix,
                       "output/")

  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = T)
  unlink(output_dir, recursive = T)
  file.remove(paste0(prefix, "tests/testthat/logfile.log"))
  file.remove(settings)
})
