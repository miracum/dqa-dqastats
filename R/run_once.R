# Cleanup the backend in RStudio:
cat("\014") # Clears the console (imitates CTR + L)
rm(list = ls(all.names = TRUE)) # Clears the Global Environment/variables/data
invisible(gc()) # Garbage collector/Clear unused RAM

devtools::load_all()
source_system_name = "source"
target_system_name = "target"
utils_path = system.file("application/_utilities/", package = "miRacumDQA")
mdr_filename = "mdr.csv"
output_dir = "/data/output/"
logfile_dir = "/data/output/logs/"

DIZutils::set_env_vars(env_file = "/data/input/.env")
parallel = TRUE
ncores = 4


## Testfunction to test it all:
all_results <- DQAstats::dqa(
  source_system_name = source_system_name,
  target_system_name = target_system_name,
  # config_file = config_file,
  utils_path = utils_path,
  mdr_filename = mdr_filename,
  output_dir = output_dir,
  logfile_dir = logfile_dir,
  parallel = parallel,
  ncores = ncores
  # , restricting_date_start = restricting_date_start
  # , restricting_date_end = restricting_date_end
)
