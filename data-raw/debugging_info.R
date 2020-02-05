## using example data:
# source_system_name = "exampleCSV_source"
# target_system_name = "exampleCSV_source"
# config_file = "inst/demo_data/utilities/settings/demo_settings.yml"
# utils_path = "inst/demo_data/utilities"
# mdr_filename = "mdr_example_data.csv"
# output_dir = "output/"


## using real data:
source_system_name = "p21csv"
#source_system_name = "p21staging"
target_system_name = "i2b2"
config_file = "tests/testthat/testdata/demo_settings_INTERNAL.yml"
utils_path = system.file("application/_utilities/", package = "miRacumDQA")
#mdr_filename = "samply_export.csv"
mdr_filename = "mdr.csv"
output_dir = "output/"


## Testfunction to test it all:
all_results <- DQAstats::dqa(
  source_system_name = source_system_name,
  target_system_name = target_system_name,
  config_file = config_file,
  utils_path = utils_path,
  mdr_filename = mdr_filename,
  output_dir = output_dir
)
