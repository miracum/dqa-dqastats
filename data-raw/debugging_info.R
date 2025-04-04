# nolint start
# Cleanup the backend in RStudio:
cat("\014") # Clears the console (imitates CTR + L)
rm(list = ls(all.names = TRUE)) # Clears the Global Environment/variables/data
invisible(gc()) # Garbage collector/Clear unused RAM

## using example data:
# source_system_name = "exampleCSV_source"
# target_system_name = "exampleCSV_target"
# utils_path <- system.file("demo_data/utilities",
#                           package = "DQAstats")
# Sys.setenv("EXAMPLECSV_SOURCE_PATH" = system.file("demo_data",
#                                                   package = "DQAstats"))
# Sys.setenv("EXAMPLECSV_TARGET_PATH" = system.file("demo_data",
#                                                   package = "DQAstats"))
# mdr_filename = "mdr_example_data.csv"
# output_dir = "output/"

#test
devtools::load_all()
## using real data:
#source_system_name = "i2b2"
# source_system_name = "kdb"
# source_system_name = "p21csv"
# source_system_name = "p21staging"
source_system_name = "kdb"
#target_system_name = "fhir_gw_miracumprofile"
# target_system_name = "p21csv"
# target_system_name = "p21staging"
# target_system_name = "omop"
target_system_name = "dwh"
# config_file = "../demo_settings_INTERNAL.yml"
#utils_path = system.file("application/_utilities/", package = "miRacumDQA")
utils_path = "../ume_dqatool/inst/application/_utilities/"
utils_path = "./inst/demo_data/utilities/"
#mdr_filename = "samply_export.csv"
mdr_filename = "mdr_mini.csv"
#mdr_filename = "mdr_debug.csv"
output_dir = "./output"
logfile_dir = tempdir()

DIZtools::setenv_file(
  paste0(
    "../",
    list.files(
      path = "../", pattern = "^env_INTERNAL.*"
    )
  )
)
parallel = TRUE
ncores = 4
restricting_date_start = "2021-01-01"
restricting_date_end = "2021-12-31"

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
  parallel = parallel
  , restricting_date_start = restricting_date_start
  , restricting_date_end = restricting_date_end
)


# Testfunction to run the demo files:
# Sys.setenv("EXAMPLECSV_SOURCE_PATH" = "inst/demo_data")
# Sys.setenv("EXAMPLECSV_TARGET_PATH" = "inst/demo_data")
# all_results <- DQAstats::dqa(
#   source_system_name = "dqa_example_data_01.csv",
#   target_system_name = "dqa_example_data_02.csv",
#   utils_path = "inst/demo_data/utilities",
#   mdr_filename = "mdr_example_data.csv",
#   output_dir = "output/"
#  restricting_date_start = "2014-01-01",
#  restricting_date_end = "2014-12-31"
# )
#
# source_system_name = "exampleCSV_source"
# target_system_name = "exampleCSV_target"
# utils_path = "inst/demo_data/utilities"
# mdr_filename = "mdr_example_data.csv"
# output_dir = "output/"
# restricting_date_start = "2014-01-01"
# restricting_date_end = "2014-12-31"
# logfile_dir = tempdir()
# parallel = TRUE
# ncores = 4

# nolint end
