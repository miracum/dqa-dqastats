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
# source_config = paste0(prefix, "testdata/config_csv.yml")
# target_db = "omop"
# source_db = "p21csv"
# utils = system.file("application/_utilities", package = "miRacumDQA")
