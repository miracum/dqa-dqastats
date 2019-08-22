# DQAstats - A package, created to perform data quality assessment (DQA) of electronic health records (EHR)
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


# report
renderResults <- function(descriptive_results, valueconformance_results){

  # get names
  obj_names <- names(descriptive_results)

  # loop over objects
  for (i in obj_names){
    desc_out <- descriptive_results[[i]]$description
    count_out <- descriptive_results[[i]]$counts
    stat_out <- descriptive_results[[i]]$statistics

    # title of variable
    cat(paste0("\n## ", desc_out$source_data$name, "  \n"))
    # description of variable
    cat(paste0("\n", desc_out$source_data$description, "  \n"))

    # representation in the source system
    cat("\n### Representation in source data system  \n")
    renderRepresentation(desc_out, "source_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "source_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$source_data))

    # conformance checks
    if (i %in% names(valueconformance_results)){
      cat("\n **Value conformance:**  \n")
      renderValueConformance(valueconformance_results[[i]], desc_out, "source_data")
    }


    # representation in the target system
    cat("\n### Representation in target data system  \n")
    renderRepresentation(desc_out, "target_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "target_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$target_data))

    # conformance checks
    if (i %in% names(valueconformance_results)){
      cat("\n **Value conformance:**  \n")
      renderValueConformance(valueconformance_results[[i]], desc_out, "target_data")
    }
  }
}

renderRepresentation <- function(desc_out, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable: ", desc_out[[source]]$var_name, "\n"))
  cat(paste0("- Table: ", desc_out[[source]]$table_name, "  \n  \n"))
}

renderCounts <- function(count_out, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable name: ", count_out[[source]]$cnt$variable, "\n"))
  cat(paste0("- Variable type: ", count_out[[source]]$type), "  \n")
  cat(paste0("    + Distinct values: ", count_out[[source]]$cnt$distinct, "\n"))
  cat(paste0("    + Valid values: ", count_out[[source]]$cnt$valids, "\n"))
  cat(paste0("    + Missing values: ", count_out[[source]]$cnt$missings, "  \n  \n"))
}

renderValueConformance <- function(results, desc_out, source){

  cat(paste0("\n- Conformance check: ", ifelse(results[[source]]$conformance_error, "failed", "passed"), "\n"))

  # get value set
  json_obj <- jsonlite::fromJSON(desc_out[[source]]$checks$value_set)

  if (desc_out[[source]]$checks$var_type == "factor"){
    cat("- Value set: ", json_obj[["value_set"]])
  } else if (desc_out[[source]]$checks$var_type %in% c("integer", "numeric")){
    cat(paste0("- Value set:"))
    print(kableTable(as.data.table(json_obj)))
  }

  if (isTRUE(results[[source]]$conformance_error)){
    cat("\n- ", paste0(results[[source]]$conformance_results, "  \n  \n"))
  } else {
    cat("  \n  \n")
  }
}

renderDataMap <- function(datamap){
  colnames(datamap) <- c("Variable", "# Distinct", "# Valid", "# Missing")
  print(kableTable(datamap))
}

renderUniqPlausis <- function(plausiresults){
  # get names
  obj_names <- names(plausiresults)

  # loop over objects
  for (i in obj_names){

    pl.item <- plausiresults[[i]]

    # title of variable
    cat(paste0("\n### ", i, "  \n"))

    # description of variable
    cat(paste0("\n", pl.item$description, "  \n"))

    # representation in the source system
    cat("\n#### Representation in source data system  \n")
    renderUniqPlausiRepresentation(pl.item, "source_data")

    # representation in the source system
    cat("\n#### Representation in target data system  \n")
    renderUniqPlausiRepresentation(pl.item, "target_data")
  }
}

renderUniqPlausiRepresentation <- function(pl.item, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Plausibility check: ", ifelse(pl.item[[source]]$error == "FALSE", "passed", "failed"), "\n"))
  if (!is.null(pl.item[[source]]$filter)){
    cat(paste0("- Filter criterion: ", pl.item[[source]]$filter, "\n"))
  }
  cat(paste0("- Message: ", pl.item[[source]]$message, "\n"))
}


renderAtempPlausis <- function(plausiresults, valueconformance_results){

  # get names
  obj_names <- names(plausiresults)

  # loop over objects
  for (i in obj_names){
    desc_out <- plausiresults[[i]]$description
    count_out <- plausiresults[[i]]$counts
    stat_out <- plausiresults[[i]]$statistics

    # title of variable
    cat(paste0("\n### ", desc_out$source_data$name, "  \n"))
    # description of variable
    cat(paste0("\n", desc_out$source_data$description, "  \n"))

    # representation in the source system
    cat("\n#### Representation in source data system  \n")
    renderAtempPlausiRepresentation(desc_out, "source_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "source_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$source_data))

    # conformance checks
    if (i %in% names(valueconformance_results)){
      cat("\n **Value conformance:**  \n")
      renderValueConformance(valueconformance_results[[i]], desc_out, "source_data")
    }

    # representation in the target system
    cat("\n#### Representation in target data system  \n")
    renderAtempPlausiRepresentation(desc_out, "target_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "target_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$target_data))

    # conformance checks
    if (i %in% names(valueconformance_results)){
      cat("\n **Value conformance:**  \n")
      renderValueConformance(valueconformance_results[[i]], desc_out, "target_data")
    }
  }
}

renderAtempPlausiRepresentation <- function(desc_out, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable 1: ", desc_out[[source]]$var_dependent, "\n"))
  cat(paste0("- Variable 2: ", desc_out[[source]]$var_independent, "\n"))
  cat(paste0("- Filter criterion variable 2 (regex): ", desc_out[[source]]$filter, "\n"))
  cat(paste0("- Join criterion: ", desc_out[[source]]$join_crit, "\n"))
}


#' @title createMarkdown_ helper function
#'
#' @description Internal function to generate the final PDF report.
#'
#' @param outdir A character string. The directory to store the resulting PDF document. Default: \code{tempdir}.
#' @inheritParams loadSource_
#' @inheritParams DQA
#'
#' @export
#'
createMarkdown_ <- function(rv = rv, utils, outdir = tempdir(), headless = FALSE){
  msg <- "Creating report "
  cat("\n", msg, "\n")
  if (isFALSE(headless)){
    shinyjs::logjs(msg)

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating report", value = 0)

    # Increment the progress bar, and update the detail text.
    progress$inc(1/1, detail = "... working hard to create pdf ...")
  }

  tryCatch({
    if (tinytex::tinytex_root() == ""){
      tinytex::install_tinytex()
    }
  }, error = function(e){
    print(e)
  }, warning = function(w){
    print(w)
  })

  knitr::knit(input= paste0(utils, "RMD/DQA_report.Rmd"), output=paste0(outdir, "/DQA_report.md"), encoding = "UTF-8")
  # copy header-folder to tempdir to make files available for the next command
  file.copy(paste0(utils, "RMD/_header"), outdir, recursive=TRUE)
  rmarkdown::render(input=paste0(outdir, "/DQA_report.md"), output_file = paste0(outdir, "/DQA_report_", gsub("\\-|\\:| ", "", substr(rv$start.time, 1, 16)), ".pdf"), encoding = "UTF-8")

  # delete temporary files
  do.call(file.remove, list(list.files(paste0(outdir, "_header"), full.names = TRUE)))
  unlink(paste0(outdir, "_header"), recursive = T)
}
