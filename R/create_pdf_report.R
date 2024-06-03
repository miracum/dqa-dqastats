# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2022 Universitätsklinikum Erlangen
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


#' @title create_pdf_report helper function
#'
#' @description Internal function to generate the final PDF report.
#'
#' @param outdir A character string. The directory to store the resulting
#'   PDF document. Default: \code{tempdir}.
#' @inheritParams load_csv
#' @inheritParams dqa
#'
#' @return No return value. This function renders the PDF markdown report with
#'   the data quality assessment results and saves it to `outdir`.
#'
#' @examples
#' \donttest{# runtime > 5 sec.
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
#' create_pdf_report(
#'   rv = rv,
#'   utils_path = rv$utilspath,
#'   outdir = output_dir,
#'   headless = rv$headless
#' )
#' }
#' @export
#'
create_pdf_report <- function(rv = rv,
                            utils_path,
                            outdir = tempdir(),
                            headless = FALSE) {

  msg <- "Creating report "
  DIZtools::feedback(
    print_this = msg,
    logjs = isFALSE(headless),
    findme = "aa5c87f7da",
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless)

  catch_msg <- paste0(
    "Something went wrong with tinytex.",
    " Is it installed correctly?",
    " Try reinstalling it by running ",
    "`remotes::update_packages('tinytex', upgrade = 'always')` ",
    "and `tinytex::install_tinytex()`\n\n",
    "!!! DQAstats is not able to render the PDF report !!!"
  )
  if (!is_latex_installed(
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless)
  ) {
    DIZtools::feedback(
      print_this = catch_msg,
      type = "Error",
      findme = "e50d001ed4",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless
    )
  } else {
    catch_msg <- "Error occured when rendering the PDF document"
    tryCatch({

      # address issues on some systems when rendering the markdown with the
      # template as input
      template_file <- tempfile(
        pattern = "DQAstats-",
        tmpdir = tempdir(),
        fileext = "-DQA_report.qmd"
      )

      # copy template file from package directory to tempdir()
      file.copy(
        from = paste0(utils_path, "RMD/DQA_report.qmd"),
        to = template_file,
        overwrite = TRUE
      )

      # copy header-folder to tempdir to make files available for
      # the next command
      if (dir.exists(paste0(utils_path, "RMD/_header"))) {
        file.copy(
          paste0(utils_path, "RMD/_header"),
          outdir,
          recursive = TRUE
        )
      }

      outfile <- file.path(paste0("DQA_report_", gsub(
        "\\-|\\:| ", "", substr(rv$start_time, 1, 16)
      ), ".pdf"))

      # save rv object for rendering document
      save(
        list = c(
          "rv", ls(getNamespace("DQAstats"), all.names = TRUE)
        ),
        file = file.path(tempdir(), "DQAstats.Rda")
      )

      # save metadata for quarto title
      params_file <- tempfile(
        pattern = "DQAstats-",
        tmpdir = tempdir(),
        fileext = "-params.yml"
      )
      writeLines(
        text = c(paste0("source_system_name: \"", rv$source$system_name, "\""),
                 paste0("target_system_name: \"", rv$target$system_name, "\""),
                 paste0("restricting_date: \"", get_restricting_date_info(
                   restricting_date = rv$restricting_date, time = FALSE), "\""
                 )),
        con = params_file
      )

      quarto::quarto_render(
        input = template_file,
        output_format = "pdf",
        metadata_file = params_file
      )
    }, error = function(e) {
      DIZtools::feedback(
        print_this = paste0(catch_msg, e),
        type = "Error",
        findme = "d70789cd83",
        logfile_dir = rv$log$logfile_dir,
        headless = rv$headless)
    }, warning = function(w) {
      DIZtools::feedback(
        print_this = paste0(catch_msg, w),
        type = "Warning",
        findme = "d70654cd83",
        logfile_dir = rv$log$logfile_dir,
        headless = rv$headless)
    })
  }
}


#' @title create_markdown helper function
#'
#' @description Internal function to generate the final PDF report.
#'
#' @param outdir A character string. The directory to store the resulting
#'   PDF document. Default: \code{tempdir}.
#' @inheritParams load_csv
#' @inheritParams dqa
#'
#' @return No return value. This function renders the PDF markdown report with
#'   the data quality assessment results and saves it to `outdir`.
#'
#' @examples
#' \donttest{# runtime > 5 sec.
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
#' create_markdown(
#'   rv = rv,
#'   utils_path = rv$utilspath,
#'   outdir = output_dir,
#'   headless = rv$headless
#' )
#' }
#' @export
#'
create_markdown <- function(rv = rv,
                            utils_path,
                            outdir = tempdir(),
                            headless = FALSE) {
  warning(paste0(
    "'create_markdown()' is deprecated ",
    "and will be removed in a future version. ",
    "Use 'create_pdf_report' instead."))
  create_pdf_report(
    rv = rv,
    utils_path,
    outdir = tempdir(),
    headless = FALSE
  )
}
