# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2024 Universitätsklinikum Erlangen
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

test_that("correct functioning of check_date_restriction_requirements", {


  utils_path <- system.file(
    "demo_data/utilities/",
    package = "DQAstats"
  )
  mdr_filename <- "mdr_example_data.csv"
  mdr <- read_mdr(
    utils_path = utils_path,
    mdr_filename = mdr_filename
  )

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  DIZtools::cleanup_old_logfile(logfile_dir = tempdir())

  res <- check_date_restriction_requirements(
    mdr = mdr,
    system_names = c(source_system_name, target_system_name),
    logfile_dir = tempdir(),
    headless = TRUE,
    enable_stop = TRUE
  )

  expect_true(res)

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})
