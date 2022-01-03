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


# report
render_results <- function(descriptive_results,
                           valueconformance_results) {

  # get names
  obj_names <- names(descriptive_results)

  # loop over objects
  tmp_firstline <- TRUE
  for (i in obj_names) {
    desc_out <- descriptive_results[[i]]$description
    count_out <- descriptive_results[[i]]$counts
    stat_out <- descriptive_results[[i]]$statistics

    if (tmp_firstline) {
      ## Skip newpage for the first variable
      tmp_firstline <- FALSE
    } else {
      cat("\\newpage")
    }
    # title of variable
    cat(paste0("\n## ", desc_out$source_data$name, "  \n"))
    # description of variable
    cat(paste0("\n", desc_out$source_data$description, "  \n"))

    # representation in the source system
    cat("\n### Representation in **source** data system  \n")
    render_representation(desc_out, "source_data")

    # overview
    cat("\n **Overview:**  \n")
    render_counts(count_out, stat_out, "source_data")

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      cat("\n **Value conformance:**  \n")
      render_value_conformance(
        valueconformance_results[[i]],
        desc_out,
        "source_data"
      )
    }

    cat("\\newpage")
    # representation in the target system
    cat("\n### Representation in **target** data system  \n")
    render_representation(desc_out, "target_data")

    # overview
    cat("\n **Overview:**  \n")
    render_counts(count_out, stat_out, "target_data")

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      cat("\n **Value conformance:**  \n")
      render_value_conformance(
        valueconformance_results[[i]],
        desc_out,
        "target_data"
      )
    }
  }
}

render_representation <- function(desc_out, source) {
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable: ", desc_out[[source]]$var_name, "\n"))
  cat(paste0("- Table: ", desc_out[[source]]$table_name, "  \n  \n"))
}

render_counts <- function(count_out,
                          stat_out,
                          source) {

  # source either "source_data" or "target_data"
  # n = sample size
  # N = population size
  if (!is.null(count_out[[source]]$cnt)) {
    cat(paste0("\n- Variable name: ",
               count_out[[source]]$cnt$variable,
               "\n"))
    cat(paste0("- Variable type: ",
               count_out[[source]]$type,
               "  \n"))
    cat(paste0("    + n: ",
               count_out[[source]]$cnt$n,
               "\n"))
    cat(paste0("    + Valid values: ",
               count_out[[source]]$cnt$valids,
               "\n"))
    cat(paste0("    + Missing values: ",
               count_out[[source]]$cnt$missings,
               "\n"))
    cat(paste0("    + Distinct values: ",
               count_out[[source]]$cnt$distinct,
               "  \n  \n"))

    cat("\n **Results:**  \n")
    print(kable_table(stat_out[[source]]))

  } else {
    cat("\nNo data available for reporting  \n  \n")
  }
}

render_value_conformance <- function(results,
                                     desc_out,
                                     source) {
  cat(paste0(
    "\n- Conformance check: ",
    ifelse(
      results[[source]]$conformance_error,
      "failed",
      "passed"
    ),
    "\n"
  ))

  # get value set
  if (desc_out[[source]]$checks$var_type != "datetime") {
    json_obj <- jsonlite::fromJSON(
      desc_out[[source]]$checks$constraints
    )
  }

  if (desc_out[[source]]$checks$var_type ==
      "enumerated") {
    cat("- Constraining values/rules: '", json_obj$value_set, "'")

  } else if (desc_out[[source]]$checks$var_type ==
             "string") {
    cat("- Constraining values/rules: '", json_obj$regex, "'")

  } else if (desc_out[[source]]$checks$var_type %in%
             c("integer", "float")) {
    cat(paste0("- Constraining values/rules:"))
    print(kable_table(as.data.table(json_obj$range)))
  } else if (desc_out[[source]]$checks$var_type ==
            "datetime") {
    cat(paste0("- Constraining values/rules: '", results[[source]]$rule, "'"))
  }

  if (isTRUE(results[[source]]$conformance_error)) {
    cat("\n- ", paste0(results[[source]]$conformance_results,
                       "  \n  \n"))
  } else {
    cat("  \n  \n")
  }
}

render_data_map <- function(datamap) {
  if (!is.null(datamap) && nrow(datamap) > 0) {
    colnames(datamap) <-
      c("Variable",
        "# n",
        "# Valid",
        "# Missing",
        "# Distinct")
    print(kable_table(datamap))
  } else {
    cat("No dataelements from the datamap where analysed.")
  }
}

render_uniq_plausis <- function(plausiresults) {
  # get names
  obj_names <- names(plausiresults)

  # loop over objects
  for (i in obj_names) {
    pl_item <- plausiresults[[i]]

    # title of variable
    cat(paste0("\n### ", i, "  \n"))

    # description of variable
    cat(paste0("\n", pl_item$description, "  \n"))

    # representation in the source system
    cat("\n#### Representation in source data system  \n")
    render_uniq_pl_representation(pl_item, "source_data")

    # representation in the source system
    cat("\n#### Representation in target data system  \n")
    render_uniq_pl_representation(pl_item, "target_data")
  }
}

render_uniq_pl_representation <- function(pl_item, source) {
  # source either "source_data" or "target_data"
  cat(paste0(
    "\n- Plausibility check: ",
    ifelse(pl_item[[source]]$error == "FALSE", "passed", "failed"),
    "\n"
  ))
  if (!is.null(pl_item[[source]]$filter)) {
    cat(paste0("- Filter criterion: ", pl_item[[source]]$filter, "\n"))
  }
  cat(paste0("- Message: ", pl_item[[source]]$message, "\n"))
}


render_atemp_plausis <- function(plausiresults,
                                 valueconformance_results) {
  # get names
  obj_names <- names(plausiresults)

  # loop over objects
  for (i in obj_names) {
    desc_out <- plausiresults[[i]]$description
    count_out <- plausiresults[[i]]$counts
    stat_out <- plausiresults[[i]]$statistics

    # title of variable
    cat(paste0("\n### ", desc_out$source_data$name, "  \n"))
    # description of variable
    cat(paste0("\n", desc_out$source_data$description, "  \n"))

    # representation in the source system
    cat("\n#### Representation in source data system  \n")
    render_atemp_pl_representation(desc_out, "source_data")

    # overview
    cat("\n **Overview:**  \n")
    render_counts(count_out, "source_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kable_table(stat_out$source_data))

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      cat("\n **Value conformance:**  \n")
      render_value_conformance(
        valueconformance_results[[i]],
        desc_out,
        "source_data"
      )
    }

    # representation in the target system
    cat("\n#### Representation in target data system  \n")
    render_atemp_pl_representation(desc_out, "target_data")

    # overview
    cat("\n **Overview:**  \n")
    render_counts(count_out, "target_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kable_table(stat_out$target_data))

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      cat("\n **Value conformance:**  \n")
      render_value_conformance(
        valueconformance_results[[i]],
        desc_out,
        "target_data"
      )
    }
  }
}

render_atemp_pl_representation <- function(desc_out, source) {
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable 1: ", desc_out[[source]]$var_dependent, "\n"))
  cat(paste0("- Variable 2: ", desc_out[[source]]$var_independent, "\n"))
  cat(paste0(
    "- Filter criterion variable 2 (regex): ",
    desc_out[[source]]$filter,
    "\n"
  ))
  cat(paste0("- Join criterion: ", desc_out[[source]]$join_crit, "\n"))
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

  msg <- "Creating report "
  DIZutils::feedback(
    msg,
    logjs = isFALSE(headless),
    findme = "aa5c87f7da",
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless)

  catch_msg <- "Something went wrong with tinytex: "
  tryCatch({
    remotes::update_packages("tinytex", upgrade = "always")
    tinytex::install_tinytex()
  }, error = function(e) {
    DIZutils::feedback(
      paste0(catch_msg, e),
      type = "Error",
      findme = "e50d001ed4",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless)
  }, warning = function(w) {
    DIZutils::feedback(
      paste0(catch_msg, w),
      type = "Warning",
      findme = "6c366260eb",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless)
  })

  tryCatch({
    if (tinytex::tinytex_root() == "") {
      tinytex::install_tinytex()
    }
  }, error = function(e) {
    DIZutils::feedback(
      paste0(catch_msg, e),
      type = "Error",
      findme = "d70293cd83",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless)
  }, warning = function(w) {
    DIZutils::feedback(
      paste0(catch_msg, w),
      type = "Warning",
      findme = "f72559b707",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless)
  })

  catch_msg <- "Error occured when rendering the PDF document"
  tryCatch({
    knitr::knit(
      input = paste0(utils_path, "RMD/DQA_report.Rmd"),
      output = paste0(outdir, "/DQA_report.md"),
      encoding = "UTF-8"
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

    rmarkdown::render(
      input = paste0(outdir, "/DQA_report.md"),
      output_dir = outdir,
      output_file = paste0("DQA_report_", gsub(
        "\\-|\\:| ", "", substr(rv$start_time, 1, 16)
      ), ".pdf"),
      encoding = "UTF-8"
    )
  }, error = function(e) {
    DIZutils::feedback(
      paste0(catch_msg, e),
      type = "Error",
      findme = "d70789cd83",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless)
  }, warning = function(w) {
    DIZutils::feedback(
      paste0(catch_msg, w),
      type = "Warning",
      findme = "d70654cd83",
      logfile_dir = rv$log$logfile_dir,
      headless = rv$headless)
  })


  # delete temporary files
  #% do.call(file.remove,
  #%         list(list.files(paste0(outdir, "_header"),
  #%                         full.names = TRUE)))
  #% unlink(paste0(outdir, "_header"), recursive = T)
}
