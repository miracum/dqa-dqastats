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
#
calc_counts <- function(cnt_dat,
                        count_key,
                        rv,
                        datamap = TRUE,
                        plausibility = FALSE,
                        plausibility_key) {

  if (base::missing(plausibility_key)) {
    stopifnot(isFALSE(plausibility))
  }

  counts <- list()

  key_cols <- get_key_col(rv)
  print("key_cols")
  key_col_name_src <- key_cols$source
  print("key_col_name_src")
  key_col_name_tar <- key_cols$target

  counts$source_data$cnt <- tryCatch(
    expr = {
      f <- cnt_dat[get("source_system_name") ==
                     rv$source$system_name,
                   get("filter")]
      f <- setdiff(f, NA)
      if (length(f) > 0) {
        where_filter <- get_where_filter(f)
      } else {
        where_filter <- NULL
      }
      if (isTRUE(datamap)) {
        cnt <- count_uniques(
          data = rv$data_source[[
            cnt_dat[get("source_system_name") == rv$source$system_name,
                    get(key_col_name_src)]
          ]],
          var = count_key,
          sourcesystem = rv$source$system_name,
          datamap = datamap,
          utils_path = rv$utilspath,
          filter = where_filter
        )
      } else if (isTRUE(plausibility)) {
        cnt <- count_uniques(
          data = rv$data_source[[plausibility_key]],
          var = count_key,
          sourcesystem = rv$source$system_name,
          datamap = datamap,
          utils_path = rv$utilspath,
          filter = where_filter
        )
      } else {
        cnt <- count_uniques(
          data = rv$data_source[[cnt_dat[get("source_system_name") ==
                                           rv$source$system_name,
                                         get("key")]]],
          var = count_key,
          sourcesystem = rv$source$system_name,
          datamap = datamap,
          utils_path = rv$utilspath,
          filter = where_filter
        )
      }
      cnt
    }, error = function(e) {
      DIZtools::feedback(
        print_this = paste0("Error occured when counting source_data: ", e),
        findme = "0adf10abcc",
        type = "Error",
        logfile_dir = rv$log$logfile_dir
      )
      cnt <- NULL
      cnt
    })

  counts$source_data$type <-
    cnt_dat[get("source_system_name") ==
              rv$source$system_name, get("variable_type")]


  # for target_data; our data is in rv$data_target$key
  counts$target_data$cnt <- tryCatch(
    expr = {
      f <- cnt_dat[get("source_system_name") ==
                     rv$target$system_name,
                   get("filter")]
      f <- setdiff(f, NA)
      if (length(f) > 0) {
        where_filter <- get_where_filter(f)
      } else {
        where_filter <- NULL
      }
      if (isTRUE(datamap)) {
        cnt <- count_uniques(
          data = rv$data_target[[cnt_dat[get("source_system_name") ==
                                           rv$target$system_name,
                                         get(key_col_name_tar)]]],
          var = count_key,
          sourcesystem = rv$target$system_name,
          datamap = datamap,
          utils_path = rv$utilspath,
          filter = where_filter
        )
      } else if (isTRUE(plausibility)) {
        cnt <- count_uniques(
          data = rv$data_target[[plausibility_key]],
          var = count_key,
          sourcesystem = rv$target$system_name,
          datamap = datamap,
          utils_path = rv$utilspath,
          filter = where_filter
        )
      } else {
        cnt <- count_uniques(
          data = rv$data_target[[cnt_dat[get("source_system_name") ==
                                           rv$target$system_name,
                                         get("key")]]],
          var = count_key,
          sourcesystem = rv$target$system_name,
          datamap = datamap,
          utils_path = rv$utilspath,
          filter = where_filter
        )
      }
      cnt

    }, error = function(e) {
      DIZtools::feedback(
        paste0("Error occured when counting target_data: ", e),
        findme = "486bd17564",
        type = "Error",
        logfile_dir = rv$log$logfile_dir
      )
      cnt <- NULL
      cnt
    })

  counts$target_data$type <-
    cnt_dat[get("source_system_name") ==
              rv$target$system_name, get("variable_type")]

  return(counts)
}
