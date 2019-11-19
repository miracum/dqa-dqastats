# prefix <- "tests/testthat/"
# target_config = paste0(prefix, "testdata/config_i2b2.yml")
# source_config = paste0(prefix, "testdata/config_p21csv.yml")
# target_db = "i2b2"
# source_db = "p21csv"
# utils_path = system.file("application/_utilities", package = "miRacumDQA")
# mdr_filename <- "mdr.csv"

# using example data
source_system_name = "exampleCSV_source"
target_system_name = "exampleCSV_target"
config_file = "inst/demo_data/utilities/settings/demo_settings.yml"
utils_path = "inst/demo_data/utilities"
mdr_filename = "mdr_example_data.csv"


all_results <- dqa(source_system_name,
                   target_system_name,
                   config_file,
                   utils_path,
                   mdr_filename)


# using real i2b2 data
source_system_name = "p21csv"
target_system_name = "i2b2"
config_file = "test/testthat/testdata/demo_settings_INTERNAL.yml"
utils_path = system.file("application/_utilities/", package = "miRacumDQA")
mdr_filename = "mdr.csv"
