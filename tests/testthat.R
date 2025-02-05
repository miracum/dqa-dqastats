library(testthat)
library(DQAstats)
library(utils)

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("logfile_dir"))
}
# https://github.com/Rdatatable/data.table/issues/5658
Sys.setenv("OMP_THREAD_LIMIT" = 2)
test_check("DQAstats")
