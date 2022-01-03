# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2021 Universitätsklinikum Erlangen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title test_csv helper function
#'
#' @description Internal function to test and get the database connection
#'   of the source data system.
#'
#' @param headless A boolean (default: FALSE). Indicating, if the function is
#'   run only in the console (headless = TRUE) or on a GUI frontend
#'   (headless = FALSE).
#'
#' @param settings A list object containing the database settings.
#' @inheritParams create_helper_vars
#' @inheritParams dqa
#'
#' @return A boolean indicating if the CSV files specified in the metadata
#'   repository are found in the specified locations.
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename = mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' DIZutils::cleanup_old_logfile(logfile_dir = tempdir())
#'
#' settings <- DIZutils::get_config_env(
#'   system_name = source_system_name,
#'   logfile_dir = tempdir(),
#'   headless = TRUE
#' )
#'
#' test_csv(
#'   settings = settings,
#'   source_db = source_system_name,
#'   mdr = mdr,
#'   headless = TRUE,
#'   logfile_dir = tempdir()
#' )
#'
#'
#' @export
#'
test_csv <- function(settings,
                     source_db,
                     mdr,
                     headless = FALSE,
                     logfile_dir) {

  # get filenames of csv files inside the provided directory
  filelist <- list.files(
    path = settings$path,
    pattern = "\\.CSV|\\.csv",
    full.names = T
  )

  # get expected filenames of csv files from MDR
  required_files <- mdr[get("source_system_name") ==
                          source_db, unique(get("source_table_name"))]
  required_files <- required_files[required_files != ""]

  # put those filenames into a regex-format, to be compared
  # to the filenames inside the provided directory
  files_pattern <- gsub("(()|(\\$\\|))$",
                        "\\$",
                        gsub("\\.",
                             "\\\\.",
                             paste0(required_files,
                                    collapse = "$|"
                             )
                        )
  )

  # iterate over list and check for presence of required filenames:
  # FALL.CSV, FAB.CSV, ICD.CSV, OPS.CSV
  check <- sapply(filelist, function(i) {
    DIZutils::feedback(i, findme = "208282630a",
             logfile_dir = logfile_dir,
             headless = headless)
    return(grepl(files_pattern, i))
  })

  outflag <- tryCatch({
    # test if provided files are matching expected files
    if (base::sum(check) != length(required_files)) {
      DIZutils::feedback(
        paste0("The specified directory does not contain the expected ",
               "neccessary CSV-files: ", paste0(required_files,
                                                collapse = ", ")),
        findme = "e8cdd32764",
        logfile_dir = logfile_dir,
        headless = headless
      )
      outflag <- NULL
      outflag
    } else {
      outflag <- TRUE
      outflag
    }
  }, error = function(e) {
    DIZutils::feedback(
      paste0(
        "There are no CSV-files in the specified directory ('",
        settings$path,
        "') for system ",
        source_db,
        ".\n"
      ),
      findme = "ed6dceaeaf",
      logfile_dir = logfile_dir,
      headless = headless
    )
    outflag <- NULL
    outflag
  })
  return(outflag)
}
