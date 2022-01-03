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


#' @title Export results to csv/zip file.
#'
#' @description This function exports aggregated results in csv files that
#'   are added to a zip archive.
#'
#' @inheritParams dqa
#' @inheritParams load_csv
#'
#' @return No return value. This function writes the aggregated results, namely
#'   the conformace results overview table, the plausibility check results
#'   overview, the completeness results overview and a combined version (named
#'   'all_results.csv') to csv files. These files are saved in
#'   `{output_dir}/export`.
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' rv <- list()
#' rv$mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename <- mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' rv <- c(rv, create_helper_vars(
#'   mdr = rv$mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' ))
#' # save source/target vars
#' rv$source$system_name <- source_system_name
#' rv$target$system_name <- target_system_name
#' rv$source$system_type <- "csv"
#' rv$target$system_type <- "csv"
#'
#' rv$log$logfile_dir <- tempdir()
#'
#' # set headless (without GUI, progressbars, etc.)
#' rv$headless <- TRUE
#'
#' # set configs
#' demo_files <- system.file("demo_data", package = "DQAstats")
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)
#'
#' # get configs
#' rv$source$settings <- DIZutils::get_config_env(
#'   system_name = rv$source$system_name,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#' rv$target$settings <- DIZutils::get_config_env(
#'   system_name = tolower(rv$target$system_name),
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#' # set start_time (e.g. when clicking the 'Load Data'-button in shiny
#' rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
#'
#' # define restricting date
#' rv$restricting_date$use_it <- FALSE
#'
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' rv$data_plausibility$atemporal <- get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # add the plausibility raw data to data_target and data_source
#' for (i in names(rv$data_plausibility$atemporal)) {
#'   for (k in c("source_data", "target_data")) {
#'     w <- gsub("_data", "", k)
#'     raw_data <- paste0("data_", w)
#'     rv[[raw_data]][[i]] <-
#'       rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
#'     rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
#'   }
#'   gc()
#' }
#'
#' # calculate descriptive results
#' rv$results_descriptive <- descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # completeness
#' rv$completeness <- completeness(results = rv$results_descriptive,
#'                                 headless = rv$headless,
#'                                 logfile_dir = rv$log$logfile_dir)
#'
#' rv$datamap <- generate_datamap(
#'   results = rv$results_descriptive,
#'   db = rv$target$system_name,
#'   mdr = rv$mdr,
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # checks$value_conformance
#' rv$checks$value_conformance <-
#'   value_conformance_checks(results = rv$conformance$value_conformance)
#'
#' # checks$etl
#' rv$checks$etl <- etl_checks(results = rv$results_descriptive)
#'
#' output_dir <- tempdir()
#' export_aggregated(
#'   output_dir = output_dir,
#'   rv = rv
#' )
#'
#' @export

export_aggregated <- function(output_dir, rv) {
  # create export dir
  exportdir <- paste0(output_dir, "/export/")

  if (!dir.exists(exportdir)) {
    DIZutils::feedback(
      paste0("Creating ", exportdir),
      findme = "4f10124602",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless
    )
    dir.create(exportdir)
  }

  # write files
  # datamap
  if (!is.null(rv$datamap$target_data)) {
    data.table::fwrite(
      x = rv$datamap$target_data,
      file = paste0(exportdir, "datamap_target.csv")
    )
  }
  if (!is.null(rv$datamap$source_data)) {
    data.table::fwrite(
      x = rv$datamap$source_data,
      file = paste0(exportdir, "datamap_source.csv")
    )
  }

  # completeness
  data.table::fwrite(
    x = rv$checks$etl,
    file = paste0(exportdir, "etl_checks.csv")
  )
  data.table::fwrite(
    x = rv$completeness,
    file = paste0(exportdir, "completeness.csv")
  )

  # conformance
  data.table::fwrite(
    x = rv$checks$value_conformance,
    file = paste0(exportdir, "value_conformance.csv")
  )

  # all results overview
  data.table::fwrite(
    x = all_results_overview(rv),
    file = paste0(exportdir, "all_results.csv")
  )
}


all_results_overview <- function(rv) {
  outlist <- data.table::data.table()
  for (name in names(rv$results_descriptive)) {

    cnt <- rv$results_descriptive[[name]]$counts

    if (!is.null(cnt$source_data$cnt) && !is.null(cnt$target_data$cnt)) {

      # source counts
      cnt_src <- cnt$source_data$cnt
      cnt_src <- cnt_src[, 2:(ncol(cnt_src) - 1)]
      colnames(cnt_src) <- paste0(colnames(cnt_src), "_src")

      # target counts
      cnt_tar <- cnt$target_data$cnt
      cnt_tar <- cnt_tar[, 2:(ncol(cnt_tar) - 1)]
      colnames(cnt_tar) <- paste0(colnames(cnt_tar), "_tar")

      outlist <- rbind(
        outlist,
        cbind(name, cnt_src, cnt_tar)
      )
    }
  }

  if (!is.null(rv$results_plausibility_atemporal)) {
    for (name in names(rv$results_plausibility_atemporal)) {
      # source counts
      cnt_src <-
        rv$results_plausibility_atemporal[[name]]$counts$source_data$cnt
      cnt_src <- cnt_src[, 2:(ncol(cnt_src) - 1)]
      colnames(cnt_src) <- paste0(colnames(cnt_src), "_src")

      # target counts
      cnt_tar <-
        rv$results_plausibility_atemporal[[name]]$counts$target_data$cnt
      cnt_tar <- cnt_tar[, 2:(ncol(cnt_tar) - 1)]
      colnames(cnt_tar) <- paste0(colnames(cnt_tar), "_tar")

      outlist <- rbind(
        outlist,
        cbind(name, cnt_src, cnt_tar)
      )
    }
  }

  # add conformance checks
  checks_conf <- rv$checks$value_conformance
  colnames(checks_conf)[2:3] <- c("check_conf_source",
                                  "check_conf_target")
  outlist <- merge(
    x = outlist,
    y = checks_conf,
    by.x = "name",
    by.y = "Variable",
    all = TRUE,
    suffixes = c("", "")
  )

  # add etl checks
  checks_etl <- rv$checks$etl
  colnames(checks_etl)[2:4] <- c("check_etl_distincts",
                                 "check_etl_valids",
                                 "check_etl_missings")
  outlist <- merge(
    x = outlist,
    y = checks_etl,
    by.x = "name",
    by.y = "Variable",
    all = TRUE,
    suffixes = c("", "")
  )

  return(outlist)
}




#' @title Export results to csv/zip file.
#'
#' @description This function exports export_affected_ids in csv files that
#'   are added to a zip archive.
#'
#' @param object The object to analyze.
#' @inheritParams dqa
#' @inheritParams load_csv
#'
#' @return No return value. If possible irregularities were identified during
#'   the data quality assessment, this function writes affected identifier
#'   values to csv files for a further detailed debugging. These files are
#'   saved in `{output_dir}/conspicuous_ids`.
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' rv <- list()
#' rv$mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename <- mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' rv <- c(rv, create_helper_vars(
#'   mdr = rv$mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' ))
#' # save source/target vars
#' rv$source$system_name <- source_system_name
#' rv$target$system_name <- target_system_name
#' rv$source$system_type <- "csv"
#' rv$target$system_type <- "csv"
#'
#' rv$log$logfile_dir <- tempdir()
#'
#' # set headless (without GUI, progressbars, etc.)
#' rv$headless <- TRUE
#'
#' # set configs
#' demo_files <- system.file("demo_data", package = "DQAstats")
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)
#'
#' # get configs
#' rv$source$settings <- DIZutils::get_config_env(
#'   system_name = rv$source$system_name,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#' rv$target$settings <- DIZutils::get_config_env(
#'   system_name = tolower(rv$target$system_name),
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#' # set start_time (e.g. when clicking the 'Load Data'-button in shiny
#' rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
#'
#' # define restricting date
#' rv$restricting_date$use_it <- FALSE
#'
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' rv$data_plausibility$atemporal <- get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # add the plausibility raw data to data_target and data_source
#' for (i in names(rv$data_plausibility$atemporal)) {
#'   for (k in c("source_data", "target_data")) {
#'     w <- gsub("_data", "", k)
#'     raw_data <- paste0("data_", w)
#'     rv[[raw_data]][[i]] <-
#'       rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
#'     rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
#'   }
#'   gc()
#' }
#'
#' # calculate descriptive results
#' rv$results_descriptive <- descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # calculate unique plausibilites
#' rv$results_plausibility_unique <- uniq_plausi_results(
#'   rv = rv,
#'   uniq_vars = rv$pl$uniq_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' output_dir <- tempdir()
#' export_affected_ids(
#'  rv = rv,
#'  output_dir = output_dir,
#'  object = rv$results_plausibility_unique
#' )
#'
#' @export

export_affected_ids <- function(object, output_dir, rv) {

  exportdir <- paste0(output_dir, "/conspicuous_ids/")

  if (!dir.exists(exportdir)) {
    DIZutils::feedback(
      paste0("Creating ", exportdir),
      findme = "4f10sfghs602",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless
    )
    dir.create(exportdir)
  }

  for (i in names(object)) {
    for (k in c("source_data", "target_data")) {

      if (!is.null(object[[i]][[k]]$affected_ids)) {
        data.table::fwrite(
          x = object[[i]][[k]]$affected_ids,
          file = paste0(exportdir,
                        gsub("[[:punct:]]|\\s", "", i), "_", k, ".csv")
        )
      }
    }
  }

}
