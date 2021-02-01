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

#' @title descriptive_results helper function
#'
#' @description Internal function to generate the descriptive results.
#'
#' @inheritParams load_csv
#' @inheritParams dqa
#'
#' @export
#'
descriptive_results <- function(rv,
                                headless = FALSE) {

  outlist <- future.apply::future_sapply(
    X = names(rv$variable_list),
    FUN = function(i) {
      local({
        # initialize outlist
        outlist <- list()

        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Getting variable descriptions of", i)
        DIZutils::feedback(
          msg,
          findme = "eb95542ec1",
          logfile_dir = rv$log$logfile_dir
        )

        # generate descriptions
        desc_dat <- rv$mdr[
          get("dqa_assessment") == 1 &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name),
        ][
          get("variable_name") == rv$variable_list[[i]] &
            get("designation") == i,
          c(
            "designation",
            "source_system_name",
            "source_variable_name",
            "source_table_name",
            "variable_name",
            "fhir",
            "definition",
            "variable_type",
            "constraints",
            "value_threshold",
            "missing_threshold",
            "filter"
          ), with = F
        ]

        if (nrow(desc_dat) > 1 ||
            rv$source$system_name == rv$target$system_name) {
          outlist$description <- calc_description(desc_dat, rv)
        } else {
          msg <- paste0("Error occured during creating ",
                        "descriptions of source system")
          DIZutils::feedback(
            msg,
            type = "Error",
            findme = "b640b3c662",
            logfile_dir = rv$log$logfile_dir
          )
          return()
        }

        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Calculating variable counts of", i)
        DIZutils::feedback(
          msg,
          findme = "056f1ee2e0",
          logfile_dir = rv$log$logfile_dir
        )

        # generate counts
        cnt_dat <- rv$mdr[
          get("dqa_assessment") == 1 &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name),
        ][
          get("variable_name") == rv$variable_list[[i]] &
            get("designation") == i, c(
              "source_system_name",
              "source_variable_name",
              "source_table_name",
              "variable_type",
              "key",
              "variable_name",
              "filter"
            ), with = F
        ]

        outlist$counts <- calc_counts(
          cnt_dat = cnt_dat,
          count_key = rv$variable_list[[i]],
          rv = rv,
          datamap = TRUE
        )


        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Calculating variable statistics of", i)
        DIZutils::feedback(
          msg,
          findme = "edf4f006a9",
          logfile_dir = rv$log$logfile_dir
        )


        # generate statistics
        stat_dat <- rv$mdr[
          get("dqa_assessment") == 1 &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name),
        ][
          get("variable_name") == rv$variable_list[[i]] &
            get("designation") == i, c(
              "source_system_name",
              "source_variable_name",
              "source_table_name",
              "variable_type",
              "key",
              "variable_name",
              "filter"
            ), with = F
        ]

        if (stat_dat[, unique(get("variable_type"))] %in%
            c("permittedValues", "string")) {
          outlist$statistics <- calc_cat_stats(
              stat_dat = stat_dat,
              stat_key = rv$variable_list[[i]],
              rv = rv
            )
          # for target_data; our data is in rv$list_target$key
        } else {
          outlist$statistics <- calc_num_stats(
            stat_dat = stat_dat,
            stat_key = rv$variable_list[[i]],
            rv = rv
          )
        }
        return(outlist)
      })
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  gc()

  return(outlist)
}
