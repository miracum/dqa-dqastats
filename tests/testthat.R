library(testthat)
library(DQAstats)
library(utils)

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("logfile_dir"))
}

test_check("DQAstats")
