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


# report
render_results <- function(descriptive_results,
                           valueconformance_results) {

  # get names
  obj_names <- names(descriptive_results)

  # loop over objects
  for (i in obj_names) {
    desc_out <- descriptive_results[[i]]$description
    count_out <- descriptive_results[[i]]$counts
    stat_out <- descriptive_results[[i]]$statistics

    # title of variable
    feedback(paste0("Variable title: ", desc_out$source_data$name),
             findme = "e3e82667a9")
    # description of variable
    feedback(paste0("Variable description", desc_out$source_data$description),
             findme = "c6c1c389aa")

    # representation in the source system
    feedback("### Representation in source data system", findme = "09aafb991d")
    render_representation(desc_out, "source_data")

    # overview
    feedback(" **Overview:** ", findme = "dd2b160443")
    render_counts(count_out, "source_data")

    # statistics
    feedback(" **Results:** ", findme = "ea15e1342d")
    feedback(kable_table(stat_out$source_data), findme = "5dff68cb10")

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      feedback(" **Value conformance:** ", findme = "49bb7ce6f4")
      render_value_conformance(
        valueconformance_results[[i]],
        desc_out,
        "source_data"
      )
    }


    # representation in the target system
    feedback("### Representation in target data system ", findme = "d5838548d5")
    render_representation(desc_out, "target_data")

    # overview
    feedback(" **Overview:**  ", findme = "9400c38aa2")
    render_counts(count_out, "target_data")

    # statistics
    feedback(" **Results:**  ", findme = "837f030013")
    print(kable_table(stat_out$target_data))

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      feedback(" **Value conformance:**  ", findme = "87537dee88")
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
  feedback(paste0("- Variable: ", desc_out[[source]]$var_name, ""),
           findme = "1cd1a4aa6e")
  feedback(paste0("- Table: ", desc_out[[source]]$table_name, "    "),
           findme = "5d3379ad25")
}

render_counts <- function(count_out,
                          source) {

  # source either "source_data" or "target_data"
  # n = sample size
  # N = population size
  feedback(paste0("- Variable name: ",
             count_out[[source]]$cnt$variable), findme = "b25f7beade")
  feedback(paste0("- Variable type: ",
             count_out[[source]]$type), findme = "cbbc671470")
  feedback(paste0("    + n: ",
             count_out[[source]]$cnt$n), findme = "20653c3bf2")
  feedback(paste0("    + Valid values: ",
             count_out[[source]]$cnt$valids), findme = "ebd0662820")
  feedback(paste0("    + Missing values: ",
             count_out[[source]]$cnt$missings), findme = "6508d99ec5")
  feedback(paste0("    + Distinct values: ",
             count_out[[source]]$cnt$distinct), findme = "688aa7f542")
}

render_value_conformance <- function(results,
                                     desc_out,
                                     source) {
  feedback(paste0(
    "- Conformance check: ",
    ifelse(
      results[[source]]$conformance_error,
      "failed",
      "passed"
    )), findme = "bcd4409a02")

  # get value set
  json_obj <- jsonlite::fromJSON(
    desc_out[[source]]$checks$constraints
  )

  if (desc_out[[source]]$checks$var_type ==
      "permittedValues") {
    feedback(paste0("- Constraining values/rules: '",
                    json_obj$value_set,
                    "'"),
             findme = "0537cde62e")

  } else if (desc_out[[source]]$checks$var_type ==
             "string") {
    feedback(paste0("- Constraining values/rules: '",
                    json_obj$regex,
                    "'"),
             findme = "45b53db8d1")

  } else if (desc_out[[source]]$checks$var_type %in%
             c("integer", "float")) {
    feedback("- Constraining values/rules:", findme = "e75f50c2a4")
    feedback(kable_table(as.data.table(json_obj$range)), findme = "cc12fad6b6")
  }

  if (isTRUE(results[[source]]$conformance_error)) {
    feedback(paste0("- ", results[[source]]$conformance_results),
             findme = "08aec5b485")
  } else {
    feedback("    ", findme = "dd3df09145")
  }
}

render_data_map <- function(datamap) {
  colnames(datamap) <-
    c("Variable",
      "# n",
      "# Valid",
      "# Missing",
      "# Distinct")
  print(kable_table(datamap))
}

render_uniq_plausis <- function(plausiresults) {
  # get names
  obj_names <- names(plausiresults)

  # loop over objects
  for (i in obj_names) {
    pl_item <- plausiresults[[i]]

    # title of variable
    feedback(paste0("### ", i, "  "), findme = "60ee4df254")

    # description of variable
    feedback(paste0("", pl_item$description, "  "), findme = "3d08b9acdb")

    # representation in the source system
    feedback("#### Representation in source data system  ",
             findme = "19577d3353")
    render_uniq_pl_representation(pl_item, "source_data")

    # representation in the source system
    feedback("#### Representation in target data system  ",
             findme = "8efaede976")
    render_uniq_pl_representation(pl_item, "target_data")
  }
}

render_uniq_pl_representation <- function(pl_item, source) {
  # source either "source_data" or "target_data"
  feedback(paste0(
    "- Plausibility check: ",
    ifelse(pl_item[[source]]$error == "FALSE", "passed", "failed")),
    findme = "d7559637bc")
  if (!is.null(pl_item[[source]]$filter)) {
    feedback(paste0("- Filter criterion: ", pl_item[[source]]$filter),
             findme = "f18226f701")
  }
  feedback(paste0("- Message: ", pl_item[[source]]$message),
           findme = "1fc516792f")
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
    feedback(paste0("### ", desc_out$source_data$name), findme = "692ed21e7a")
    # description of variable
    feedback(desc_out$source_data$description, findme = "347554e930")

    # representation in the source system
    feedback("#### Representation in source data system  ",
             findme = "a219db1e31")
    render_atemp_pl_representation(desc_out, "source_data")

    # overview
    feedback(" **Overview:**  ", findme = "95ea976d6d")
    render_counts(count_out, "source_data")

    # statistics
    feedback(" **Results:**  ", findme = "c8c01ff344")
    print(kable_table(stat_out$source_data))

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      feedback(" **Value conformance:**  ", findme = "0bf2c35dd5")
      render_value_conformance(
        valueconformance_results[[i]],
        desc_out,
        "source_data"
      )
    }

    # representation in the target system
    feedback("#### Representation in target data system  ",
             findme = "9d65ad71af")
    render_atemp_pl_representation(desc_out, "target_data")

    # overview
    feedback(" **Overview:**  ", findme = "880d16732f")
    render_counts(count_out, "target_data")

    # statistics
    feedback(" **Results:**  ", findme = "1f6fcc70fd")
    print(kable_table(stat_out$target_data))

    # conformance checks
    if (i %in% names(valueconformance_results)) {
      feedback(" **Value conformance:**  ", findme = "2043793018")
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
  feedback(paste0("- Variable 1: ", desc_out[[source]]$var_dependent),
           findme = "70da187221")
  feedback(paste0("- Variable 2: ", desc_out[[source]]$var_independent),
           findme = "94ec97b1a8")
  feedback(paste0(
    "- Filter criterion variable 2 (regex): ",
    desc_out[[source]]$filter), findme = "d9ef877f2e")
  feedback(paste0("- Join criterion: ", desc_out[[source]]$join_crit),
           findme = "47aeca9c91")
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
#' @export
#'
create_markdown <- function(rv = rv,
                            utils_path,
                            outdir = tempdir(),
                            headless = FALSE) {

  msg <- "Creating report "
  feedback(msg, logjs = isFALSE(headless), findme = "1147c689ab")
  if (isFALSE(headless)) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(message = "Creating report", value = 0)

    # Increment the progress bar, and update the detail text.
    progress$inc(
      1 / 1,
      detail = "... working hard to create pdf ..."
    )
  }

  catch_msg <- "Something went wrong with tinytex: "
  tryCatch({
    if (tinytex::tinytex_root() == "") {
      tinytex::install_tinytex()
    }
  }, error = function(e) {
    feedback(paste0(catch_msg, e), type = "Error", findme = "210d434926")
  }, warning = function(w) {
    feedback(paste0(catch_msg, w), type = "Warning", findme = "6293ffd675")
  })

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
    output_file = paste0(outdir, "/DQA_report_", gsub(
      "\\-|\\:| ", "", substr(rv$start_time, 1, 16)
    ), ".pdf"),
    encoding = "UTF-8"
  )

  # delete temporary files
  #% do.call(file.remove,
  #%         list(list.files(paste0(outdir, "_header"),
  #%                         full.names = TRUE)))
  #% unlink(paste0(outdir, "_header"), recursive = T)
}
