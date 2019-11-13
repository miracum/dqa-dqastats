# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019 Universitätsklinikum Erlangen
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


#' @title Perform Data Quality Assessment of Electronic Health Records.
#'
#' @description This function performs a data quality assessment (DQA)
#'   of electronic health records (EHR).#'
#'
#' @param source_system_name A character string. The name of the
#'   source-system, e.g. "P21" or "i2b2". This name must be identical and
#'   unique to one entry in the settings-yml file.
#' @param target_system_name  Optional. A character string or null.
#'   The name of the target-system, e.g. "P21" or "i2b2".
#'   This name must be identical and unique to one entry in the
#'   config-yml file or null. If the argument is empty, the source will
#'   be processed as standalone on its own.
#' @param config_file The config.yml-file containig all the information
#'   needed to access the source (and optional the target) system(s).
#' @param utils_path A character string. The path to the utils-folder,
#'   containing the required app utilities like the MDR and the settings folder.
#' @param mdr_filename A character string. The filename of the MDR e.g. "mdr_example_data.csv"
#' For a detailed description please visit \url{#TODO}.
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' DQA("type1_experimentaldata.csv", "type1_calibrationdata.csv",
#' samplelocusname = "BRAF")
#' }
#'
#' @export

dqa <- function(source_system_name,
                target_system_name = source_system_name,
                config_file,
                utils_path,
                mdr_filename = "mdr.csv") {
  # new arguments for debugging:
  source_system_name <- "exampleCSV"
  target_system_name <- "exampleCSV_target"
  # config_file <-
  #   "./inst/demo_data/utilities/settings/demo_settings.yml"
  config_file <-
    "tests/testthat/testdata/demo_settings_internal.yml"
  utils_path <- "./inst/demo_data/utilities/"
  mdr_filename <- "mdr_example_data.csv"

  stopifnot(
    is.character(source_system_name),
    is.character(target_system_name),
    is.character(config_file),
    is.character(utils_path),
    is.character(mdr_filename)
  )

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # clean utils paths (to append the ending slash)
  rv$utilspath <- clean_path_name(utils_path)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")

  # get configs
  rv$settings_target <- get_config(config_file = config_file,
                                   config_key = tolower(rv$target$system_name))

  rv$settings_source <- get_config(config_file = config_file,
                                   config_key = tolower(rv$source$system_name))

  # read MDR
  rv$mdr <- read_mdr(utils = rv$utilspath,
                     mdr_filename = rv$mdr_filename)
  stopifnot(data.table::is.data.table(rv$mdr))

  # read system_types
  rv$source$system_type <-
    rv$mdr[get("system_name") == rv$source$system_name, unique(get("source_system_type"))]
  stopifnot(length(rv$source$system_type) == 1)

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

  # load source_data
  if (rv$source$system_type == "csv") {
    # load csv
  } else if (rv$source$system_type == "postgres") {
    # load postgres
  } else {
    stop("\nThis source_system_type is currently not implemented.\n\n")
  }

  rv$data_source <-
    data_loading_function(system_type = rv$dqa_source, mdr) #load anything else? TODO JM


  # load target_data
  if (!is.null(rv$target$system_name)) {
    # load target
    rv$data_target <- data_loading_function()
  } else {
    rv$data_target <- rv$data_source
  }


  # get sourcefiledir
  rv$sourcefiledir <- clean_path_name(rv$settings_source$dir)

  # test source_db
  test_source <- test_source_db(
    source_settings = rv$settings_source,
    source_db = rv$source$system_name,
    headless = rv$headless
  )
  stopifnot(isTRUE(test_source))

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = T, tz = "CET")

  # load source data
  rv$data_source <- load_source(
    rv = rv,
    keys_to_test = rv$keys_source,
    headless = rv$headless
  )

  # import target SQL
  rv$sql_target <- load_sqls(utils = rv$utilspath,
                             db = rv$target$system_name)
  stopifnot(is.list(rv$sql_target))

  # test target_db
  test_target <-
    test_target_db(target_settings = rv$settings_target,
                   headless = rv$headless)
  stopifnot(!is.null(test_target))

  rv$db_con_target <- test_target
  rm(test_target)

  # load target data
  rv$data_target <- load_target(
    rv = rv,
    keys_to_test = rv$keys_target,
    headless = rv$headless
  )

  # get atemporal plausibilities
  rv$data_plausibility$atemporal <- get_atemp_plausis(
    rv = rv,
    atemp_vars = rv$pl$atemp_vars,
    mdr = rv$mdr,
    headless = rv$headless
  )

  # add the plausibility raw data to data_target and data_source
  for (i in names(rv$data_plausibility$atemporal)) {
    for (k in c("source_data", "target_data")) {
      w <- gsub("_data", "", k)
      n_key <- paste0(i, "_", w)
      raw_data <- paste0("data_", w)
      rv[[raw_data]][[n_key]] <-
        rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
      rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
    }
    gc()
  }

  # calculate descriptive results
  rv$results_descriptive <- descriptive_results(rv = rv,
                                                headless = rv$headless)

  # get time_interval
  rv$time_interval <-
    time_interval(rv$results_descriptive$EpisodeOfCare_period_end)

  # calculate plausibilites
  rv$results_plausibility_atemporal <- atemp_pausi_results(rv = rv,
                                                           headless = rv$headless)

  rv$results_plausibility_unique <- uniq_plausi_results(
    rv = rv,
    uniq_vars = rv$pl$uniq_vars,
    mdr = rv$mdr,
    headless = rv$headless
  )

  # delete raw data
  rv$data_source <- NULL
  rv$data_target <- NULL
  gc()

  # conformance
  rv$conformance$value_conformance <-
    value_conformance(results = rv$results_descriptive,
                      headless = rv$headless)

  value_conformance <- value_conformance(
    results = rv$results_plausibility_atemporal,
    headless = rv$headless
  )

  # workaround, to keep "rv" an reactiveValues object in shiny app
  for (i in names(value_conformance)) {
    rv$conformance$value_conformance[[i]] <- value_conformance[[i]]
  }

  # completeness
  rv$completeness <- completeness(results = rv$results_descriptive,
                                  headless = rv$headless)

  # generate datamap
  rv$datamap <- generate_datamap(
    results = rv$results_descriptive,
    db = rv$target$system_name,
    mdr = rv$mdr,
    headless = rv$headless
  )

  # checks$value_conformance
  rv$checks$value_conformance <-
    value_conformance_checks(results = rv$conformance$value_conformance)

  # checks$etl
  rv$checks$etl <- etl_checks(results = rv$results_descriptive)

  # create report
  create_markdown(
    rv = rv,
    utils = rv$utilspath,
    outdir = "./",
    headless = rv$headless
  )

  # set end_time
  rv$end_time <- format(Sys.time(), usetz = T, tz = "CET")
  # calc time-diff
  rv$duration <- difftime(rv$end_time,
                          rv$start_time,
                          units = "mins")

  print(rv$duration)
  return(TRUE)
}
