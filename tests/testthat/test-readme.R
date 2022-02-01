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

context("test readme example")

library(data.table)

test_that("correct functioning of readme example", {

  # Set environment vars to demo files paths:
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = system.file("demo_data",
                                                    package = "DQAstats"))
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = system.file("demo_data",
                                                    package = "DQAstats"))
  # Set path to utilities folder where to find the mdr and template files:
  utils_path <- system.file("demo_data/utilities",
                            package = "DQAstats")

  # Execute the DQA and generate a PDF report:
  results <- DQAstats::dqa(
    source_system_name = "exampleCSV_source",
    target_system_name = "exampleCSV_target",
    utils_path = utils_path,
    mdr_filename = "mdr_example_data.csv",
    output_dir = paste0(tempdir(), "/output/"),
    parallel = FALSE
  )

  expect_type(results, "list")
  expect_length(results, 27)

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})
