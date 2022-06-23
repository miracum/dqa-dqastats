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

    # representation in the source database
    cat("\n### Representation in **source** database  \n")
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
    # representation in the target database
    cat("\n### Representation in **target** database  \n")
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

  results_text <- format_value_conformance_results(
    results = results,
    desc_out = desc_out,
    source = source
  )

  # conformance check (always)
  cat(paste0(
    "\n- ", results_text$conformance_check
  ))

  # rules (only if present)
  if (!is.null(results_text$constraining_rules)) {
    cat(paste0(
      "\n- ", results_text$constraining_rules
    ))
  }

  # table (only if present)
  if (!is.null(results_text$kable)) {
    print(kable_table(results_text$kable))
  }

  # conformance results (only if present)
  if (!is.null(results_text$conformance_results)) {
    cat(paste0(
      "\n- ", results_text$conformance_results
    ))
  }

  cat("  \n  \n")
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

    # representation in the source database
    cat("\n#### Representation in source database  \n")
    render_uniq_pl_representation(pl_item, "source_data")

    # representation in the source database
    cat("\n#### Representation in target database  \n")
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

    # representation in the source database
    cat("\n#### Representation in source database  \n")
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
        results = valueconformance_results[[i]],
        desc_out = desc_out,
        source = "source_data"
      )
    }

    # representation in the target database
    cat("\n#### Representation in target database  \n")
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
        results = valueconformance_results[[i]],
        desc_out = desc_out,
        source = "target_data"
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
