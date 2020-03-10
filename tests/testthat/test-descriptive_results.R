# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2020 Universitätsklinikum Erlangen
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

context("test descriptive results")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/DQAstats/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
  prefix <- "./"
}

settings <- paste0(prefix, "tests/testthat/test_settings.yml")
file.copy(settings,
          paste0(prefix, "tests/testthat/test_settings_use.yml"),
          overwrite = T)
settings <- paste0(prefix, "tests/testthat/test_settings_use.yml")
tx  <- readLines(settings)
tx2  <- gsub(
  pattern = "replace_me",
  replacement = paste0("\"",
                       system.file("demo_data", package = "DQAstats"),
                       "\""),
  x = tx
)
writeLines(tx2, con = settings)

library(data.table)

test_that("correct functioning of descriptive results", {

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"
  config_file <- settings
  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(prefix,
                       "output/")

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  # get configs
  rv$source$settings <- get_config(config_file = config_file,
                                   config_key = tolower(rv$source$system_name))
  rv$target$settings <- get_config(config_file = config_file,
                                   config_key = tolower(rv$target$system_name))

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # clean paths (to append the ending slash)
  rv$utilspath <- clean_path_name(utils_path)
  output_dir <- clean_path_name(output_dir)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")


  # read MDR
  rv$mdr <- read_mdr(utils_path = rv$utilspath,
                     mdr_filename = rv$mdr_filename)


  # read system_types
  rv$source$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$source$system_name, unique(get("source_system_type"))]
  rv$target$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$target$system_name, unique(get("source_system_type"))]


  reactive_to_append <- create_helper_vars(
    mdr = rv$mdr,
    target_db = rv$target$system_name,
    source_db = rv$source$system_name
  )

  # workaround, to keep "rv" an reactiveValues object in shiny app
  #% (rv <- c(rv, reactive_to_append)) does not work!
  for (i in names(reactive_to_append)) {
    rv[[i]] <- reactive_to_append[[i]]
  }
  rm(reactive_to_append)
  invisible(gc())

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = T, tz = "CET")



  # load source data:
  temp_dat <- data_loading(
    rv = rv,
    system = rv$source,
    keys_to_test = rv$keys_source
  )
  rv$data_source <- temp_dat$outdata
  rv$source$sql <- temp_dat$sql_statements
  rm(temp_dat)
  invisible(gc())

  # load target_data
  if (rv$target$system_name != rv$source$system_name) {
    # load target
    temp_dat <- data_loading(
      rv = rv,
      system = rv$target,
      keys_to_test = rv$keys_target
    )
    rv$data_target <- temp_dat$outdata
    rv$target$sql <- temp_dat$sql_statements
    rm(temp_dat)
    invisible(gc())
  } else {
    rv$data_target <- rv$data_source
  }

  # calculate descriptive results
  rv$results_descriptive <- descriptive_results(
    rv = rv,
    headless = rv$headless
  )

  expect_length(rv$results_descriptive, 12)
  expect_false(!any(sapply(rv$results_descriptive, length) == 3))

  # conformance
  rv$conformance$value_conformance <-
    value_conformance(results = rv$results_descriptive,
                      headless = rv$headless,
                      logfile_dir = rv$log$logfile_dir)

  expect_length(rv$conformance$value_conformance, 6)
  expect_false(!any(sapply(rv$conformance$value_conformance, length) == 2))
})




test_that("correct functioning of descriptive results - single source", {

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_source"
  config_file <- settings
  utils_path <- system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"
  output_dir <- paste0(prefix,
                       "output/")

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  rv$log$logfile_dir <- "logfile.log"


  expect_true(!is.null(rv$source$settings$dir))
  expect_true(!is.null(rv$target$settings$dir))

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # get configs
  rv$source$settings <- get_config(config_file = config_file,
                                   config_key = tolower(rv$source$system_name),
                                   logfile_dir = rv$log$logfile_dir,
                                   headless = rv$headless)
  rv$target$settings <- get_config(config_file = config_file,
                                   config_key = tolower(rv$target$system_name),
                                   logfile_dir = rv$log$logfile_dir,
                                   headless = rv$headless)

  # clean paths (to append the ending slash)
  rv$utilspath <- clean_path_name(utils_path)
  output_dir <- clean_path_name(output_dir)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")


  # read MDR
  rv$mdr <- read_mdr(utils_path = rv$utilspath,
                     mdr_filename = rv$mdr_filename)


  # read system_types
  rv$source$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$source$system_name, unique(get("source_system_type"))]
  rv$target$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$target$system_name, unique(get("source_system_type"))]


  reactive_to_append <- create_helper_vars(
    mdr = rv$mdr,
    target_db = rv$target$system_name,
    source_db = rv$source$system_name
  )

  # workaround, to keep "rv" an reactiveValues object in shiny app
  #% (rv <- c(rv, reactive_to_append)) does not work!
  for (i in names(reactive_to_append)) {
    rv[[i]] <- reactive_to_append[[i]]
  }
  rm(reactive_to_append)
  invisible(gc())

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = T, tz = "CET")



  # load source data:
  temp_dat <- data_loading(
    rv = rv,
    system = rv$source,
    keys_to_test = rv$keys_source
  )
  rv$data_source <- temp_dat$outdata
  rv$source$sql <- temp_dat$sql_statements
  rm(temp_dat)
  invisible(gc())

  # load target_data
  if (rv$target$system_name != rv$source$system_name) {
    # load target
    temp_dat <- data_loading(
      rv = rv,
      system = rv$target,
      keys_to_test = rv$keys_target
    )
    rv$data_target <- temp_dat$outdata
    rv$target$sql <- temp_dat$sql_statements
    rm(temp_dat)
    invisible(gc())
  } else {
    rv$data_target <- rv$data_source
  }

  # calculate descriptive results
  rv$results_descriptive <- descriptive_results(
    rv = rv,
    headless = rv$headless
  )

  expect_length(rv$results_descriptive, 12)
  expect_false(!any(sapply(rv$results_descriptive, length) == 3))

  # conformance
  rv$conformance$value_conformance <-
    value_conformance(results = rv$results_descriptive,
                      headless = rv$headless,
                      logfile_dir = rv$log$logfile_dir)

  expect_length(rv$conformance$value_conformance, 6)
  expect_false(!any(sapply(rv$conformance$value_conformance, length) == 2))

  # Remove the settings and output-folder:
  do.call(file.remove, list(list.files(
    paste0(output_dir, "_header"), full.names = TRUE
  )))
  unlink(paste0(output_dir, "_header"), recursive = T)
  unlink(output_dir, recursive = T)
  file.remove(settings)
})
