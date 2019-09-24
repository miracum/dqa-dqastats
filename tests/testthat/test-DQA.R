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

context("test DQA function")

prefix <- "./"
#prefix <- "tests/testthat/"

library(data.table)

test_that("correct functioning of DQA",{

  skip_on_cran()
  skip("Skipping DQA-Test on build")
  expect_true(DQA(target_config = paste0(prefix, "testdata/config_i2b2.yml"),
                  source_config = paste0(prefix, "testdata/config_csv.yml"),
                  target_db = "i2b2",
                  source_db = "p21csv",
                  utils = system.file("application/_utilities", package = "miRacumDQA")))

})

# i2b2
# prefix <- "tests/testthat/"
# target_config = paste0(prefix, "testdata/config_i2b2.yml")
# source_config = paste0(prefix, "testdata/config_p21csv.yml")
# target_db = "i2b2"
# source_db = "p21csv"
# utils = system.file("application/_utilities", package = "miRacumDQA")

# omop
# prefix <- "tests/testthat/"
# target_config = paste0(prefix, "testdata/config_omop.yml")
# source_config = paste0(prefix, "testdata/config_p21csv.yml")
# target_db = "omop"
# source_db = "p21csv"
# utils = system.file("application/_utilities", package = "miRacumDQA")
