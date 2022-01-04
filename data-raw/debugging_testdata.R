# Cleanup the backend in RStudio:
cat("\014") # Clears the console (imitates CTR + L)
rm(list = ls(all.names = TRUE)) # Clears the Global Environment/variables/data
invisible(gc()) # Garbage collector/Clear unused RAM

## using example data:
source_system_name = "exampleCSV_source"
target_system_name = "exampleCSV_target"
utils_path <- system.file("demo_data/utilities",
                          package = "DQAstats")
Sys.setenv("EXAMPLECSV_SOURCE_PATH" = system.file("demo_data",
                                                  package = "DQAstats"))
Sys.setenv("EXAMPLECSV_TARGET_PATH" = system.file("demo_data",
                                                  package = "DQAstats"))
mdr_filename = "mdr_example_data.csv"
output_dir = "./output/"
parallel = TRUE
ncores = 4
logfile_dir = tempdir()

devtools::load_all()

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
  # , restricting_date_start = restricting_date_start
  # , restricting_date_end = restricting_date_end
)
