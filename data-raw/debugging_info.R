## using example data:
# source_system_name = "exampleCSV_source"
# target_system_name = "exampleCSV_target"
# config_file = "inst/demo_data/utilities/settings/demo_settings.yml"
# utils_path = "inst/demo_data/utilities"
# mdr_filename = "mdr_example_data.csv"


## using real i2b2 data:
source_system_name = "i2b2"
target_system_name = "p21csv"
config_file = "tests/testthat/testdata/demo_settings_INTERNAL.yml"
utils_path = system.file("application/_utilities/", package = "miRacumDQA")
mdr_filename = "mdr-test.csv"
output_dir = "output/"

## using real omop data:
# source_system_name = "p21csv"
# target_system_name = "omop"
# config_file = "tests/testthat/testdata/demo_settings_INTERNAL.yml"
# utils_path = system.file("application/_utilities/", package = "miRacumDQA")
# mdr_filename = "mdr.csv"

## Testfunction to test it all:
all_results <- dqa(
  source_system_name = source_system_name,
  target_system_name = target_system_name,
  config_file = config_file,
  utils_path = utils_path,
  mdr_filename = mdr_filename
)
