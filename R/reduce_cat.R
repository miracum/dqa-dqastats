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
        if (!is.null(data[[i]]$statistics[[j]])) {
          if (nrow(data[[i]]$statistics[[j]]) > levellimit) {
            data[[i]]$statistics[[j]] <- data[[i]]$statistics[[j]][
              1:levellimit,
            ]
          }
        }
      }
    }
  }
  return(data)
}
