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

calc_atemp_plausi_description <- function(dat,
                                          plausis_atemporal,
                                          desc_dat,
                                          rv) {

  description <- list()
  description$source_data <- list(
    name = dat$source_data$name,
    description = dat$source_data$description,
    var_dependent = dat$source_data$var_dependent,
    var_independent = dat$source_data$var_independent,
    filter = dat$source_data$filter,
    join_crit = dat$source_data$join_crit,
    checks = c(
      plausis_atemporal$source_data$checks,
      list(
        var_type = desc_dat[get("source_system_name") ==
                              rv$source$system_name,
                            get("variable_type")]
      )
    )
  )
  description$target_data <- list(
    name = dat$target_data$name,
    var_dependent = dat$target_data$var_dependent,
    var_independent = dat$target_data$var_independent,
    filter = dat$target_data$filter,
    join_crit = dat$target_data$join_crit,
    checks = c(
      plausis_atemporal$target_data$checks,
      list(
        var_type = desc_dat[get("source_system_name") ==
                              rv$source$system_name,
                            get("variable_type")]
      )
    )
  )

  return(description)
}



calc_num_stats <- function(stat_dat,
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

  if (stat_dat[get("source_system_name") ==
               rv$source$system_name,
               get("variable_type")] != "calendar") {
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
          data <- rv$data_source[[stat_dat[get("source_system_name") ==
                                             rv$source$system_name,
                                           get(key_col_name_src)]]]
          # for source_data; our data is in rv$data_source$source_table_name
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          source_data <- extensive_summary(vector)
        } else {
          data <- rv$data_source[[plausibility_key]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          source_data <- extensive_summary(vector)
        }
        source_data
      }, error = function(e) {
        DIZutils::feedback(
          "Error occured when calculating simple source numStats\n",
          findme = "65c004f101",
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
          data <- rv$data_target[[stat_dat[get("source_system_name") ==
                                             rv$target$system_name,
                                           get(key_col_name_tar)]]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          target_data <- extensive_summary(vector)
        } else {
          data <- rv$data_target[[plausibility_key]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          target_data <- extensive_summary(vector)
        }
        target_data

      }, error = function(e) {
        DIZutils::feedback(
          "Error occured when calculating simple target numStats\n",
          findme = "7d01e3744a",
          logfile_dir = rv$log$logfile_dir
        )
        print(e)
        target_data <- NULL
        target_data
      }, finally = function(f) {
        return(target_data)
      })


  } else {
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
          data <- rv$data_source[[stat_dat[get("source_system_name") ==
                                             rv$source$system_name,
                                           get(key_col_name_src)]]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          source_data <- simple_summary(vector)
        } else {
          data <- rv$data_source[[plausibility_key]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          source_data <- simple_summary(vector)
        }
        source_data

      }, error = function(e) {
        DIZutils::feedback(
          "Error occured when calculating simple source numStats\n",
          findme = "0b7d075ee0",
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
          data <- rv$data_target[[stat_dat[get("source_system_name") ==
                                             rv$target$system_name,
                                           get(key_col_name_tar)]]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          target_data <- simple_summary(vector)
        } else {
          data <- rv$data_target[[plausibility_key]]
          if (length(where_filter) > 0) {
            vector <- data[grepl(where_filter$filter_logic,
                                 data[, get(where_filter$filter_var)]),
                           get(stat_key)]
          } else {
            vector <- data[, get(stat_key)]
          }
          target_data <- simple_summary(vector)
        }
        target_data

      }, error = function(e) {
        DIZutils::feedback(
          "Error occured when calculating simple target numStats",
          findme = "10b1904a51",
          logfile_dir = rv$log$logfile_dir
        )
        print(e)
        target_data <- NULL
        target_data
      }, finally = function(f) {
        return(target_data)
      })
  }

  return(statistics)
}
