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
#' of electronic health records (EHR).#'
#'
#' @param target_config A character string. The path to the config.yml-file
#' containing the target database configuration.
#' @param source_config A character string. The path to the config.yml-file
#' containing the source database configuration.
#' @param target_db A character string. The name of the target database.
#' This string must be conform with the corresponding config section
#' in the config.yml-file.
#' @param source_db A character string. The name of the source database.
#' This string must be conform with the corresponding config section
#' in the config.yml-file.
#' @param utils A character string. The path to the utils-folder,
#' containing the requires app utilities.
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

dqa <- function(target_config,
                source_config,
                target_db,
                source_db,
                utils) {

  stopifnot(
    is.character(target_config),
    is.character(source_config),
    is.character(target_db),
    is.character(source_db)
  )

  # initialize rv-list
  rv <- list()
  # set headless
  rv$headless <- TRUE

  # clean utils paths
  rv$utilspath <- clean_path_name(utils)

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")

  # save db-names
  rv$db_target <- target_db
  rv$db_source <- source_db

  # get configs
  rv$settings_target <- get_config(
    config_file = target_config,
    config_key = rv$db_target
  )

  rv$settings_source <- get_config(
    config_file = source_config,
    config_key = rv$db_source
  )

  # read MDR
  rv$mdr <- read_mdr(rv$utilspath)
  stopifnot(data.table::is.data.table(rv$mdr))

  reactive_to_append <- create_helper_vars(
    mdr = rv$mdr,
    target_db = rv$db_target,
    source_db = rv$db_source
  )

  # workaround, to keep "rv" an reactiveValues object in shiny app
  #% (rv <- c(rv, reactive_to_append)) does not work!
  for (i in names(reactive_to_append)) {
    rv[[i]] <- reactive_to_append[[i]]
  }

  # get sourcefiledir
  rv$sourcefiledir <- clean_path_name(rv$settings_source$dir)

  # test source_db
  test_source <- test_source_db(
    source_settings = rv$settings_source,
    source_db = rv$db_source,
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
  rv$sql_target <- load_sqls(
    utils = rv$utilspath,
    db = rv$db_target
  )
  stopifnot(is.list(rv$sql_target))

  # test target_db
  test_target <- test_target_db(
    target_settings = rv$settings_target,
    headless = rv$headless
  )
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
  rv$results_descriptive <- descriptive_results(
    rv = rv,
    headless = rv$headless
  )

  # get time_interval
  rv$time_interval <- time_interval(
    rv$results_descriptive$EpisodeOfCare_period_end
  )

  # calculate plausibilites
  rv$results_plausibility_atemporal <- atemp_pausi_results(
    rv = rv,
    headless = rv$headless
  )

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
  rv$conformance$value_conformance <- value_conformance(
    results = rv$results_descriptive,
    headless = rv$headless
  )

  value_conformance <- value_conformance(
    results = rv$results_plausibility_atemporal,
    headless = rv$headless
  )

  # workaround, to keep "rv" an reactiveValues object in shiny app
  for (i in names(value_conformance)) {
    rv$conformance$value_conformance[[i]] <- value_conformance[[i]]
  }

  # completeness
  rv$completeness <- completeness(
    results = rv$results_descriptive,
    headless = rv$headless
  )

  # generate datamap
  rv$datamap <- generate_datamap(
    results = rv$results_descriptive,
    db = rv$db_target,
    mdr = rv$mdr,
    headless = rv$headless
  )

  # checks$value_conformance
  rv$checks$value_conformance <- value_conformance_checks(
    results = rv$conformance$value_conformance
  )

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
  rv$duration <- difftime(
    rv$end_time,
    rv$start_time,
    units = "mins"
  )

  print(rv$duration)
  return(TRUE)
}
