prefix <- "tests/testthat/"
target_config = paste0(prefix, "testdata/config_i2b2.yml")
source_config = paste0(prefix, "testdata/config_p21csv.yml")
target_db = "i2b2"
source_db = "p21csv"
utils_path = system.file("application/_utilities", package = "miRacumDQA")
mdr_filename <- "mdr.csv"
