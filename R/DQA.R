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
#' @param mdr_filename A character string.
#'   The filename of the MDR e.g. "mdr_example_data.csv"
#'   For a detailed description please visit \url{#TODO}.
#' @param output_dir The path to the output folder where all the results will
#'   be stored.
#' @param logfile_dir The absolute path to folder where the logfile
#'   will be stored.
#'
#' @import data.table
#' @importFrom magrittr "%>%"
#' @import utils
#'
#' @examples
#' \dontrun{
#' DQA("type1_experimentaldata.csv", "type1_calibrationdata.csv",
#' samplelocusname = "BRAF")
#' }
#'
#' @export

dqa <- function(source_system_name,
                target_system_name,
                config_file,
                utils_path,
                mdr_filename = "mdr.csv",
                output_dir = "./output/",
                logfile_dir = tempdir()) {

  if (missing(target_system_name)) {
    target_system_name <- source_system_name
  }

  stopifnot(
    is.character(source_system_name),
    is.character(target_system_name),
    is.character(config_file),
    is.character(utils_path),
    is.character(mdr_filename),
    is.character(output_dir),
    is.character(logfile_dir),
    dir.exists(logfile_dir)
  )

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # clean paths (to append the ending slash)
  rv$utilspath <- clean_path_name(utils_path)
  output_dir <- clean_path_name(output_dir)

  # Save logfile_dir globally:
  rv$log$logfile_dir <- clean_path_name(logfile_dir)
  cleanup_old_logfile(logfile_dir = rv$log$logfile_dir)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")


  # get configs
  rv$source$settings <- get_config(
    config_file = config_file,
    config_key = tolower(rv$source$system_name),
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )
  rv$target$settings <- get_config(
    config_file = config_file,
    config_key = tolower(rv$target$system_name),
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )

  # read MDR
  rv$mdr <- read_mdr(utils_path = rv$utilspath,
                     mdr_filename = rv$mdr_filename)
  stopifnot(data.table::is.data.table(rv$mdr))

  # read system_types
  rv$source$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$source$system_name, unique(get("source_system_type"))]
  rv$target$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$target$system_name, unique(get("source_system_type"))]

  # We only allow one (system) type per system name. There can't e.g. be
  # system types "csv" and "postgres" both with the system_name "data":
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

  if (nrow(rv$pl$atemp_vars) > 0) {
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
        raw_data <- paste0("data_", w)
        rv[[raw_data]][[i]] <-
          rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
        rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
      }
      gc()
    }
  }

  # calculate descriptive results
  rv$results_descriptive <- descriptive_results(
    rv = rv,
    headless = rv$headless
  )

  if (!is.null(rv$data_plausibility$atemporal)) {
    # calculate plausibilites
    rv$results_plausibility_atemporal <- atemp_pausi_results(
      rv = rv,
      atemp_vars = rv$data_plausibility$atemporal,
      mdr = rv$mdr,
      headless = rv$headless
    )
  }

  if (nrow(rv$pl$uniq_vars) != 0) {
    rv$results_plausibility_unique <- uniq_plausi_results(
      rv = rv,
      uniq_vars = rv$pl$uniq_vars,
      mdr = rv$mdr,
      headless = rv$headless
    )
  }

  # conformance
  rv$conformance$value_conformance <-
    value_conformance(
      rv = rv,
      results = rv$results_descriptive,
      headless = rv$headless,
      logfile_dir = rv$log$logfile_dir
    )

  # delete raw data but atemporal plausis (we need them until
  # ids of errorneous cases are returend in value conformance)
  if (nrow(rv$pl$atemp_vars) > 0) {
    rv$data_source <- rv$data_source[names(rv$data_plausibility$atemporal)]
    rv$data_target <- rv$data_target[names(rv$data_plausibility$atemporal)]
  } else {
    rv$data_source <- NULL
    rv$data_target <- NULL
  }
  invisible(gc())

  # reduce categorical variables to display max. 25 values
  rv$results_descriptive <- reduce_cat(data = rv$results_descriptive,
                                       levellimit = 25)
  invisible(gc())

  if (!is.null(rv$results_plausibility_atemporal)) {
    add_value_conformance <- value_conformance(
      rv = rv,
      results = rv$results_plausibility_atemporal,
      headless = rv$headless,
      logfile_dir = rv$log$logfile_dir
    )

    # workaround, to keep "rv" an reactiveValues object in shiny app
    for (i in names(add_value_conformance)) {
      rv$conformance$value_conformance[[i]] <- add_value_conformance[[i]]
    }
    rm(add_value_conformance)
    rv$data_source <- NULL
    rv$data_target <- NULL
    invisible(gc())
  }

  # completeness
  rv$completeness <- completeness(results = rv$results_descriptive,
                                  headless = rv$headless,
                                  logfile_dir = rv$log$logfile_dir)

  # generate datamap
  rv$datamap <- generate_datamap(
    results = rv$results_descriptive,
    db = rv$target$system_name,
    mdr = rv$mdr,
    rv = rv,
    headless = rv$headless
  )

  # checks$value_conformance
  rv$checks$value_conformance <-
    value_conformance_checks(results = rv$conformance$value_conformance)

  # checks$etl
  rv$checks$etl <- etl_checks(results = rv$results_descriptive)

  # create report
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  export_aggregated(
    output_dir = output_dir,
    rv = rv
  )

  # export descriptive results (inkl. atemporal plausbility)
  export_affected_ids(
    rv = rv,
    output_dir = output_dir,
    object = rv$conformance$value_conformance
  )

  export_affected_ids(
    rv = rv,
    output_dir = output_dir,
    object = rv$results_plausibility_unique
  )

  create_markdown(
    rv = rv,
    utils_path = rv$utilspath,
    outdir = paste0(getwd(), "/", output_dir),
    headless = rv$headless
  )

  # set end_time
  rv$end_time <- format(Sys.time(), usetz = T, tz = "CET")
  # calc time-diff
  rv$duration <- difftime(rv$end_time,
                          rv$start_time,
                          units = "mins")

  print(rv$duration)
  return(rv)
}
