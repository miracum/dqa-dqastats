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
#' @param utils_path A character string. The path to the utils-folder,
#'   containing the required app utilities like the MDR and the settings folder.
#' @param mdr_filename A character string.
#'   The filename of the MDR e.g. "mdr_example_data.csv".
#' @param output_dir The path to the output folder where all the results will
#'   be stored (default: `paste0(tempdir(), "/output/")`).
#' @param logfile_dir The absolute path to folder where the logfile
#'   will be stored default(`tempdir()`).
#' @param parallel A boolean. If TRUE, initializing a `future::plan()`
#'   for running the code (default: FALSE).
#' @param ncores A integer. The number of cores to use. Caution: you would
#'   probably like to choose a low number when operating on large datasets.
#'   Default: 2.
#' @param restricting_date_start The date as the lower limit against which
#'   the data to be analyzed will be filtered. Your input must be able to be
#'   recognized as a date by `parsedate::parse_date("2021-02-25")`.
#'   Keep in mind: If you supply a date without a time here,
#'   the time will automatically be set to 00:00.
#' @param restricting_date_end The date as the lower limit against which
#'   the data to be analyzed will be filtered. Your input must be able to be
#'   recognized as a date by `parsedate::parse_date("2021-02-25")`
#'   Keep in mind: If you supply a date without a time here,
#'   the time will automatically be set to 00:00. This means, the end DAY
#'   you provide here won't be included: '2021-12-31' will become
#'   '2021-12-31 00:00:00'. If you want to include this day, you need to
#'   supply also a time '2021-12-31 23:59:59' or just use the next day
#'   without a time: '2022-01-01'.
#' @param restricting_date_format The format in which the input data is stored.
#'   See `?strptime` for possible parameters.
#'   Currently not implemented! So there is no effect if you pass a format here.
#'
#' @return This function is a wrapper around all helper functions in `DQAstats`
#'   to perform the data quality assessment. The results are summarized in a
#'   PDF report which is saved to `outdir`. The return value of this function is
#'   a nested list that contains all results as R objects.
#'
#' @examples
#' \donttest{# runtime > 5 sec.
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = system.file(
#'   "demo_data",
#'   package = "DQAstats")
#' )
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = system.file(
#'   "demo_data",
#'   package = "DQAstats")
#' )
#'
#' # Set path to utilities folder where to find the mdr and template files:
#' utils_path <- system.file(
#'   "demo_data/utilities",
#'   package = "DQAstats"
#' )
#'
#' # Execute the DQA and generate a PDF report:
#' results <- DQAstats::dqa(
#'   source_system_name = "exampleCSV_source",
#'   target_system_name = "exampleCSV_target",
#'   utils_path = utils_path,
#'   mdr_filename = "mdr_example_data.csv",
#'   output_dir = paste0(tempdir(), "/output/"),
#'   parallel = FALSE
#' )
#' }
#' @export
#'
dqa <- function(source_system_name,
                target_system_name,
                utils_path,
                mdr_filename = "mdr.csv",
                output_dir = paste0(tempdir(), "/output/"),
                logfile_dir = tempdir(),
                parallel = FALSE,
                ncores = 2,
                restricting_date_start = NULL,
                restricting_date_end = NULL,
                restricting_date_format = NULL) {

  ## Print versions of packages:
  for (p in c("DIZtools", "DIZutils", "DQAstats")) {
    message(paste0("Version of '", p, "': ", utils::packageVersion(p)))
  }

  if (missing(target_system_name)) {
    target_system_name <- source_system_name
  }

  logfile_dir %>%
    normalizePath(mustWork = FALSE) %>%
    dir.create(showWarnings = FALSE)

  stopifnot(
    is.character(source_system_name),
    is.character(target_system_name),
    is.character(utils_path),
    is.character(mdr_filename),
    is.character(output_dir),
    is.character(logfile_dir),
    dir.exists(logfile_dir),
    is.logical(parallel),
    is.numeric(ncores)
  )

  # initialize rv-list
  rv <- list()

  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # clean paths (to append the ending slash)
  rv$utilspath <- DIZtools::clean_path_name(utils_path)
  output_dir <- DIZtools::clean_path_name(output_dir)

  # Save logfile_dir globally:
  rv$log$logfile_dir <- DIZtools::clean_path_name(logfile_dir)
  DIZtools::cleanup_old_logfile(logfile_dir = rv$log$logfile_dir)

  # add mdr-filename
  rv$mdr_filename <- mdr_filename

  # current date
  rv$current_date <- format(Sys.Date(), "%d. %B %Y", tz = "CET")

  # set parallel backend
  rv$parallel <- parallel
  rv$ncores <- ncores
  parallel(
    parallel = rv$parallel,
    logfile_dir = rv$log$logfile_dir,
    ncores = rv$ncores
  )

  ## Save restricting date information to rv object:
  if (is.null(restricting_date_start) ||
      is.null(restricting_date_end)) {
    DIZtools::feedback(
      print_this = paste0(
        "No time contstraints will be applied to input data.",
        " Either `restricting_date_start` or `restricting_date_end` was null."
      ),
      logfile = rv$log$logfile_dir,
      findme = "08b1a85c61"
    )
    rv$restricting_date$use_it <- FALSE
    rv$restricting_date$start <- restricting_date_start
    rv$restricting_date$end <- restricting_date_end
  } else {
    restricting_date_start_posixct <-
      parsedate::parse_date(dates = restricting_date_start, approx = FALSE)
    restricting_date_end_posixct <-
      parsedate::parse_date(dates = restricting_date_end, approx = FALSE)

    ## Check the start date:
    if (is.na(restricting_date_start_posixct)) {
      DIZtools::feedback(
        print_this = paste0(
          "Couldn't identify input date format",
          " for `restricting_date_start`."
        ),
        logfile = rv$log$logfile_dir,
        type = "Error",
        findme = "bcbb8d759e"
      )
      stop("See above.")
    }

    ## Check the end date:
    if (is.na(restricting_date_end_posixct)) {
      DIZtools::feedback(
        print_this = paste0(
          "Couldn't identify input date format for `restricting_date_end`.",
          " Using current timestamp now."
        ),
        logfile = rv$log$logfile_dir,
        type = "Error",
        findme = "c802927140"
      )
      restricting_date_end_posixct <- as.POSIXct(Sys.time())
    }

    ## Check if start < end:
    if (restricting_date_end_posixct <= restricting_date_start_posixct) {
      DIZtools::feedback(
        print_this = paste0(
          "`restricting_date_start` needs to be a timestamp",
          " before `restricting_date_end`.",
          "'",
          restricting_date_start_posixct,
          "' !< '",
          restricting_date_end_posixct,
          "'. ",
          " Please change."
        ),
        logfile = rv$log$logfile_dir,
        type = "Error",
        findme = "6d92c1e74f"
      )
      stop("See above.")
    }

    rv$restricting_date$use_it <- TRUE
    rv$restricting_date$start <- restricting_date_start_posixct
    rv$restricting_date$end <- restricting_date_end_posixct

    DIZtools::feedback(
      print_this = paste0(
        "Time contstraints from ",
        rv$restricting_date$start,
        " to ",
        rv$restricting_date$end,
        " will be applied to input data."
      ),
      logfile = rv$log$logfile_dir,
      findme = "2403fb1aa3"
    )
  }


  # get configs (new: with env):
  rv$source$settings <- DIZutils::get_config_env(
    system_name = rv$source$system_name,
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )
  rv$target$settings <- DIZutils::get_config_env(
    system_name = rv$target$system_name,
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )

  # read MDR
  rv$mdr <- read_mdr(utils_path = rv$utilspath,
                     mdr_filename = rv$mdr_filename)
  stopifnot(data.table::is.data.table(rv$mdr))

  ## Check if the MDR contains valid information about the time restrictions:
  if (rv$restricting_date$use_it) {
    check_date_restriction_requirements(
      mdr = rv$mdr,
      system_names = c(rv$source$system_name, rv$target$system_name),
      #% restricting_date = rv$restricting_date,
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless
    )
  } else {
    DIZtools::feedback(
      print_this = paste0(
        "Don't checking the time filtering columns because time filtering",
        " is not necessarry. (`rv$restricting_date$use_it` is not TRUE)."
      ),
      logfile = rv$log$logfile_dir,
      findme = "feec24b71b"
    )
  }

  # read system_types
  rv$source$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$source$system_name, unique(get("source_system_type"))]
  rv$target$system_type <-
    rv$mdr[get("source_system_name") ==
             rv$target$system_name, unique(get("source_system_type"))]

  # We only allow one (system) type per system name. There can't e.g. be
  # system types "csv" and "postgres" both with the system_name "data":
  if (length(rv$source$system_type) != 1) {
    DIZtools::feedback(
      print_this = paste(
        "Need exactly one source database type but found",
        length(rv$source$system_type),
        ". If there are 0 database types: The database name you",
        "provided was not found in the mdr. Maybe there is",
        "a typo?"
      ),
      type = "Error",
      findme = "c96bf620ca",
      logfile_dir = logfile_dir
    )
    stop("See error above")
  }


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
  rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")

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
    rv$target$sql <- rv$source$sql
  }

  # here is the place to insert the new code for time-compare

   rv$time_compare_results <- time_compare(rv = rv,
                logfile_dir = rv$log$logfile_dir,
                headless = rv$headless)

  # delete the TIMESTAMP columns
  # ToDo: what happens if not deleted?

  fun <- function(x) {

    if ("TIMESTAMP" %in% names(x)) {
      x$TIMESTAMP <- NULL
    }
    return(x)
  }

  rv$data_source <- lapply(rv$data_source, fun)
  rv$data_target <- lapply(rv$data_target, fun)

  if (nrow(rv$pl$atemp_vars) > 0 && rv$pl$atemp_possible) {
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
    rv$results_plausibility_atemporal <- atemp_plausi_results(
      rv = rv,
      atemp_vars = rv$data_plausibility$atemporal,
      mdr = rv$mdr,
      headless = rv$headless
    )
  }

  if (nrow(rv$pl$uniq_vars) != 0 && rv$pl$uniq_possible) {
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
      scope = "descriptive",
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
  rv$results_descriptive <- reduce_cat(
    data = rv$results_descriptive,
    levellimit = 25
  )

  invisible(gc())

  if (!is.null(rv$results_plausibility_atemporal)) {
    add_value_conformance <- value_conformance(
      rv = rv,
      scope = "plausibility",
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

  # checks$differences
  rv$checks$differences <- difference_checks(results = rv$results_descriptive)

  # create report
  if (!dir.exists(output_dir)) {
    output_dir %>%
      dir.create(showWarnings = FALSE)
  }

  export_aggregated(
    output_dir = output_dir,
    rv = rv
  )

  create_markdown(
    rv = rv,
    utils_path = rv$utilspath,
    outdir = output_dir,
    headless = rv$headless
  )

  # set end_time
  rv$end_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
  # calc time-diff
  rv$duration <- difftime(rv$end_time,
                          rv$start_time,
                          units = "mins")

  # parallel fallback
  DIZtools::feedback(
    "using future::plan(\"sequential\")",
    logjs = FALSE,
    findme = "0875ba600d",
    logfile_dir = logfile_dir,
    headless = TRUE
  )
  suppressWarnings(future::plan("sequential"))

  print(rv$duration)
  return(rv)
}
