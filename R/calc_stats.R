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

calc_description <- function(desc_dat,
                             rv) {
  if (nrow(desc_dat) > 1) {
    description <- list()
    description$source_data <- list(
      name = desc_dat[get("source_system_name") ==
                        rv$source$system_name, get("designation")],
      internal_variable_name = desc_dat[get("source_system_name") ==
                                          rv$source$system_name,
                                        get("variable_name")],
      description = desc_dat[get("source_system_name") ==
                               rv$source$system_name, get("definition")],
      var_name = desc_dat[get("source_system_name") ==
                            rv$source$system_name, get("source_variable_name")],
      table_name = desc_dat[get("source_system_name") ==
                              rv$source$system_name, get("source_table_name")],
      fhir_name = desc_dat[get("source_system_name") ==
                             rv$source$system_name, get("fhir")],
      checks = list(
        var_type = desc_dat[get("source_system_name") ==
                              rv$source$system_name, get("variable_type")],
        constraints = desc_dat[get("source_system_name") ==
                                 rv$source$system_name, get("constraints")],
        value_threshold = desc_dat[get("source_system_name") ==
                                     rv$source$system_name,
                                   get("value_threshold")],
        missing_threshold = desc_dat[get("source_system_name") ==
                                       rv$source$system_name,
                                     get("missing_threshold")]
      )
    )

    description$target_data <-
      list(
        var_name = desc_dat[get("source_system_name") ==
                              rv$target$system_name,
                            get("source_variable_name")],
        table_name = desc_dat[get("source_system_name") ==
                                rv$target$system_name,
                              get("source_table_name")],
        fhir_name = desc_dat[get("source_system_name") ==
                               rv$target$system_name,
                             get("fhir")],
        checks = list(
          var_type = desc_dat[get("source_system_name") ==
                                rv$target$system_name,
                              get("variable_type")],
          constraints = desc_dat[get("source_system_name") ==
                                   rv$target$system_name,
                                 get("constraints")],
          value_threshold = desc_dat[get("source_system_name") ==
                                       rv$target$system_name,
                                     get("value_threshold")],
          missing_threshold = desc_dat[get("source_system_name") ==
                                         rv$target$system_name,
                                       get("missing_threshold")]
        )
      )
    return(description)
  } else {
    return(NULL)
  }
}

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
      list(var_type = desc_dat[get("source_system_name") ==
                                 rv$source$system_name, get("variable_type")])
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
      list(var_type = desc_dat[get("source_system_name") ==
                                 rv$source$system_name, get("variable_type")])
    )
  )

  return(description)
}

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
  key_col_name_src <- key_cols$source
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
          data = rv$data_source[[cnt_dat[get("source_system_name") ==
                                           rv$source$system_name,
                                         get(key_col_name_src)]]],
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
      message("Error occured when counting source_data\n")
      print(e)
      cnt <- NULL
      cnt
    }, finally = function(f) {
      return(cnt)
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
      message("Error occured when counting target_data\n")
      print(e)
      cnt <- NULL
      cnt
    }, finally = function(f) {
      return(cnt)
    })

  counts$target_data$type <-
    cnt_dat[get("source_system_name") ==
              rv$target$system_name, get("variable_type")]

  return(counts)
}

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
          data = rv$data_source[[stat_dat[get(
            "source_system_name") == rv$source$system_name,
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
      message("Error occured when calculating source catStats\n")
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
          data = rv$data_target[[stat_dat[get(
            "source_system_name") ==
              rv$target$system_name, get(key_col_name_tar)]]],
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
      message("Error occured when calculating target catStats\n")
      print(e)
      target_data <- NULL
      target_data
    }, finally = function(f) {
      return(target_data)
    })

  return(statistics)
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
               rv$source$system_name, get("variable_type")] !=
               "calendar") {
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
          data <- rv$data_source[[stat_dat[get(
            "source_system_name"
          ) == rv$source$system_name, get(
            key_col_name_src
          )]]]
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
        message("Error occured when calculating simple source numStats\n")
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
          data <- rv$data_target[[stat_dat[get(
            "source_system_name"
          ) == rv$target$system_name, get(key_col_name_tar)]]]
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
        message("Error occured when calculating simple target numStats\n")
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
          data <- rv$data_source[[stat_dat[get(
            "source_system_name"
          ) == rv$source$system_name, get(
            key_col_name_src
          )]]]
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
        message("Error occured when calculating simple source numStats\n")
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
          data <- rv$data_target[[stat_dat[get(
            "source_system_name"
          ) == rv$target$system_name, get(
            key_col_name_tar
          )]]]
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
        message("Error occured when calculating simple target numStats\n")
        print(e)
        target_data <- NULL
        target_data
      }, finally = function(f) {
        return(target_data)
      })
  }

  return(statistics)
}




get_key_col <- function(rv) {
  # if system_type = "csv" --> our index to find
  # our table is the filename, stored in "source_table_name"
  if (rv$source$system_type == "csv") {
    key_col_name_src <- "source_table_name"

    # if system_type = "postgres" --> our index to find
    # our table is the "variable_name" (correspondingly,
    # the variable_name is used to store the sql statements)
  } else if (rv$source$system_type %in%
             c("postgres", "mysql", "fhir")) {
    # Back to key: 'variable_name' was assigned here:
    key_col_name_src <- "key"
  }
  if (rv$target$system_type == "csv") {
    key_col_name_tar <- "source_table_name"
  } else if (rv$target$system_type %in%
             c("postgres", "mysql", "fhir")) {
    # Back to key: 'variable_name' was assigned here:
    key_col_name_tar <- "key"
  }
  return(list(source = key_col_name_src,
              target = key_col_name_tar))
}
