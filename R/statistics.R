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


count_uniques <- function(data,
                          var,
                          sourcesystem,
                          datamap = TRUE,
                          utils_path,
                          filter) {

  valids <- NULL

  if (length(filter) > 0) {
    data <- data[grepl(filter$filter_logic,
                       data[, get(filter$filter_var)]), ]
  }

  if (isTRUE(datamap)) {
    # control for aggregated values
    grouping_file <- paste0(utils_path, "/MISC/grouping_variables.JSON")
    if (file.exists(grouping_file)) {
      grouping_vars <- jsonlite::fromJSON(grouping_file)

      if (length(grouping_vars) > 0) {
        for (name in names(grouping_vars)) {
          special_treatment_vars <- grouping_vars[[name]]
        }

        if (var %in% special_treatment_vars) {
          n <- unique(
            data[, get(var), by = name]
          )[, .N]
          valids <-
            unique(
              data[!is.na(get(var)), get(var), by = name]
            )[, .N]
          missings <-
            unique(
              data[is.na(get(var)), get(var), by = name]
            )[, .N]
        }
      }
    }
  }

  if (is.null(valids)) {
    n <- data[, .N]
    valids <- data[!is.na(get(var)), .N]
    missings <- data[is.na(get(var)), .N]
  }

  out <- data.table::data.table(
    "variable" = var,
    "n" = n,
    "valids" = valids,
    "missings" = missings,
    "distinct" = data[, nlevels(factor(get(var)))],
    "sourcesystem" = sourcesystem
  )
  return(out)
}

# extensive summary
extensive_summary <- function(vector) {
  quant <- stats::quantile(vector,
                       probs = c(.25, .75),
                       na.rm = T,
                       names = F)
  i_out <- stats::IQR(vector, na.rm = T) * 1.5

  ret <- data.table::data.table(rbind(
    c("Mean", round(base::mean(vector, na.rm = T), 2)),
    c("Minimum", round(base::min(vector, na.rm = T), 2)),
    c("Median", round(stats::median(vector, na.rm = T), 2)),
    c("Maximum", round(base::max(vector, na.rm = T), 2)),
    c("SD", round(stats::sd(vector, na.rm = T), 2)),
    c("Negativ", round(as.numeric(
      base::sum(vector < 0, na.rm = T)
    ), 2)),
    c("Zero", round(as.numeric(
      base::sum(vector == 0, na.rm = T)
    ), 2)),
    c("Positive", round(as.numeric(
      base::sum(vector > 0, na.rm = T)
    ), 2)),
    c("OutLo", round(as.numeric(
      base::sum(vector < (quant[1] - i_out), na.rm = T)
    ), 2)),
    c("OutHi", round(as.numeric(
      base::sum(vector > (quant[2] + i_out), na.rm = T)
    ), 2)),
    c("Skewness", round(as.numeric(
      e1071::skewness(vector, na.rm = T)
    ), 2)),
    c("Kurtosis", round(as.numeric(
      e1071::kurtosis(vector, na.rm = T)
    ), 2)),
    c("Variance", round(as.numeric(
      stats::var(vector, na.rm = T)
    ), 2)),
    c("Range", round(as.numeric(
      base::max(vector, na.rm = T) - base::min(vector, na.rm = T)
    ), 2))
  ))
  colnames(ret) <- c(" ", " ")
  return(ret)
}

# simple summary
simple_summary <- function(vector) {
  ar <- as.data.frame(as.array(summary(vector)))
  ar[, 2] <- as.character(ar[, 2])
  colnames(ar) <- c(" ", " ")
  return(ar)
}

# categorical_analysis
categorical_analysis <- function(data,
                                 var,
                                 levellimit = 25,
                                 filter) {

  if (length(filter) > 0) {
    data <- data[grepl(filter$filter_logic,
                       data[, get(filter$filter_var)]), ]
  }

  # TODO we need to define variable types at the dataimport
  data[, (var) := factor(get(var))]

  # if there are more levels than specified in levellimit
  #% (default = 20)
  if (data[, nlevels(get(var))] > levellimit) {
    tabdat <- data[, .N, by = var][
      order(get("N"), decreasing = T)
      ]
    tabdat_out <- tabdat[1:levellimit, ]
  } else {
    tabdat_out <- data[, .N, by = var][
      order(get("N"), decreasing = T)
      ]
  }
  tabdat_out[, "valid" := (get("N") / nrow(data)) * 100]
  colnames(tabdat_out)[c(2, 3)] <- c("Freq", "% Valid")
  return(tabdat_out)
}

#' @title reduce_cat helper function
#'
#' @description Internal function to reduce categorical variables
#'   to a maximum of values to be displayed.
#'
#' @param data A list object. The object `rv$results_descriptive`.
#' @param levellimit An integer value. The number of maximum
#'   values to be displayed (default: 25).
#'
#' @export
#'
reduce_cat <- function(data, levellimit = 25) {

  # loop over variable names in list
  for (i in names(data)) {

    if (is.null(data[[i]]$description)) {
      next
    }

    # only if variable type of interest
    if (data[[i]]$description$source_data$checks$var_type %in%
        c("permittedValues", "string")) {

      # loop over source and target data
      for (j in c("source_data", "target_data")) {
        # if number of rows > levellimit, reduce to levellimit
        if (nrow(data[[i]]$statistics[[j]]) > levellimit) {
          data[[i]]$statistics[[j]] <- data[[i]]$statistics[[j]][1:levellimit, ]
        }
      }
    }
  }
  return(data)
}
