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

test_that("correct functioning of datetime requirements", {

  local_edition(3)
  local_reproducible_output(rstudio = TRUE)

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

  ret <- check_date_restriction_requirements(
    mdr = mdr,
    system_names = c(source_system_name, target_system_name),
    logfile_dir = tempdir(),
    headless = TRUE,
    enable_stop = TRUE
  )

  expect_true(ret)

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})


test_that("correct functioning of apply time restrictions", {
  skip_on_cran()

  local_edition(3)
  local_reproducible_output(rstudio = TRUE)

  sql_statement <- "SELECT patient_num FROM i2b2miracum.patient_dimension;"

  fake_mdr <- data.table::data.table(
    cbind(
      "source_table_name" = c("patient_dimension", "visit_dimension"),
      "restricting_date_var" = "start_date",
      "restricting_date_format" = NA,
      "source_system_name" = "i2b2",
      "source_system_type" = "postgres",
      "key" = c("patient", "encounter")
    )
  )
  test <- apply_time_restriciton(
    data = sql_statement,
    key = "patient",
    lower_limit = "2010-01-01",
    upper_limit = "2015-12-31",
    system_name = "i2b2",
    system_type = "postgres",
    mdr = fake_mdr,
    logfile_dir = NULL,
    db_con = "FAKE",
    sql_create_view_all = NULL,
    verify_on_db = FALSE
  )

  expect_snapshot(
    test,
    cran = FALSE,
    error = FALSE
  )

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})



test_that("correct functioning of get_restricting_date_info", {
  skip_on_cran()

  local_edition(3)
  local_reproducible_output(rstudio = TRUE)

  test <- get_restricting_date_info(
    restricting_date = list("use_it" = TRUE,
                            start = "2010-01-01",
                            end = "2011-01-01")
  )

  expect_snapshot(
    test,
    cran = FALSE,
    error = FALSE
  )

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})
