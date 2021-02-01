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
#
calc_cat_stats <- function(stat_dat,
                           stat_key,
                           rv,
                           plausibility = FALSE,
                           plausibility_key) {

  if (base::missing(plausibility_key)) {
    stopifnot(isFALSE(plausibility))
  }

  statistics <- list()

  key_cols <- get_key_col(rv)
  key_col_name_src <- key_cols$source
  key_col_name_tar <- key_cols$target

  statistics$source_data <- tryCatch(
    expr = {
      f <- stat_dat[get("source_system_name") ==
                      rv$source$system_name,
                    get("filter")]
      f <- setdiff(f, NA)
      if (length(f) > 0) {
        where_filter <- get_where_filter(f)
      } else {
        where_filter <- NULL
      }
      if (isFALSE(plausibility)) {
        # for source_data; our data is in rv$data_source$source_table_name
        source_data <- categorical_analysis(
          data = rv$data_source[[stat_dat[get("source_system_name") ==
                                            rv$source$system_name,
                                          get(key_col_name_src)]]],
          var = stat_key,
          levellimit = Inf,
          filter = where_filter
        )
      } else {
        source_data <- categorical_analysis(
          data = rv$data_source[[plausibility_key]],
          var = stat_key,
          levellimit = Inf,
          filter = where_filter
        )
      }
      source_data
    }, error = function(e) {
      DIZutils::feedback(
        "Error occured when calculating source catStats\n",
        findme = "b8e039a302",
        logfile_dir = rv$log$logfile_dir
      )
      print(e)
      source_data <- NULL
      source_data
    }, finally = function(f) {
      return(source_data)
    })

  statistics$target_data <- tryCatch(
    expr = {
      f <- stat_dat[get("source_system_name") ==
                      rv$target$system_name,
                    get("filter")]
      f <- setdiff(f, NA)
      if (length(f) > 0) {
        where_filter <- get_where_filter(f)
      } else {
        where_filter <- NULL
      }
      if (isFALSE(plausibility)) {
        target_data <- categorical_analysis(
          data = rv$data_target[[stat_dat[get("source_system_name") ==
                                            rv$target$system_name,
                                          get(key_col_name_tar)]]],
          var = stat_key,
          levellimit = Inf,
          filter = where_filter
        )
      } else {
        target_data <- categorical_analysis(
          data = rv$data_target[[plausibility_key]],
          var = stat_key,
          levellimit = Inf,
          filter = where_filter
        )
      }
      target_data

    }, error = function(e) {
      DIZutils::feedback(
        "Error occured when calculating target catStats\n",
        findme = "5b1a5937e5",
        logfile_dir = rv$log$logfile_dir
      )
      print(e)
      target_data <- NULL
      target_data
    }, finally = function(f) {
      return(target_data)
    })

  return(statistics)
}
