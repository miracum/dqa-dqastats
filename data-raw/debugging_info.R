## using example data:
# source_system_name = "exampleCSV_source"
# target_system_name = "exampleCSV_source"
# config_file = "inst/demo_data/utilities/settings/demo_settings.yml"
# utils_path = "inst/demo_data/utilities"
# mdr_filename = "mdr_example_data.csv"
# output_dir = "output/"

devtools::load_all()
## using real data:
source_system_name = "i2b2"
# source_system_name = "kdb"
#source_system_name = "p21csv"
#source_system_name = "p21staging"
#source_system_name = "fhirgw"
target_system_name = "i2b2"
#target_system_name = "p21csv"
#target_system_name = "p21staging"
# target_system_name = "omop"
# config_file = "../demo_settings_INTERNAL.yml"
utils_path = system.file("application/_utilities/", package = "miRacumDQA")
#mdr_filename = "samply_export.csv"
mdr_filename = "mdr.csv"
# mdr_filename = "mdr_combined.csv"
output_dir = "output/"
logfile_dir = "./"

DIZutils::set_env_vars(
  paste0(
    "../",
    list.files(
      path = "../", pattern = "^env_INTERNAL.*"
    )
  )
)

# Sys.setenv("P21CSV_PATH" = "~/development/_p21/data/2018_merged/")


## Testfunction to test it all:
all_results <- DQAstats::dqa(
  source_system_name = source_system_name,
  target_system_name = target_system_name,
  # config_file = config_file,
  utils_path = utils_path,
  mdr_filename = mdr_filename,
  output_dir = output_dir,
  logfile_dir = logfile_dir,
  parallel = TRUE
)
