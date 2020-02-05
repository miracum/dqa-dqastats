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


#' @title Export results to csv/zip file.
#'
#' @description This function exports aggregated results in csv files that
#'   are added to a zip archive.
#'
#' @inheritParams dqa
#' @inheritParams load_csv
#'
#' @export

export_aggregated <- function(output_dir, rv) {
  # create export dir
  exportdir <- paste0(output_dir, "/export/")
  message("\nCreating ", exportdir, "\n")
  dir.create(exportdir)

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
    # source counts
    cnt_src <- rv$results_descriptive[[name]]$counts$source_data$cnt
    cnt_src <- cnt_src[, 2:(ncol(cnt_src) - 1)]
    colnames(cnt_src) <- paste0(colnames(cnt_src), "_src")

    # target counts
    cnt_tar <- rv$results_descriptive[[name]]$counts$target_data$cnt
    cnt_tar <- cnt_tar[, 2:(ncol(cnt_tar) - 1)]
    colnames(cnt_tar) <- paste0(colnames(cnt_tar), "_tar")

    outlist <- rbind(
      outlist,
      cbind(name, cnt_src, cnt_tar)
    )
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
