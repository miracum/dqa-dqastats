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

context("test statistics function")

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
                       paste0(prefix, "inst/demo_data/"),
                       "\""),
  x = tx
)
writeLines(tx2, con = settings)

library(data.table)

test_that("correct functioning of statistics", {

  set.seed(1)
  testdat <- data.table::data.table(
    "gender" = sample(x = c("m", "f", "u"),
                      size = 20,
                      replace = T),
    "age" = rnorm(20, mean = 45, sd = 10)
  )
  testdat[,("gender") := factor(get("gender"))]

  testres <- count_uniques(
    data = testdat,
    var = "gender",
    sourcesystem = "testsystem",
    datamap = F
  )

  expect_true(testres$valids == 20)


  testres <- extensive_summary(
    vector = testdat$age
  )

  expect_known_hash(testres, "f7e9e19a68")

})
