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
# categorical_analysis
categorical_analysis <- function(data,
                                 var,
                                 levellimit = 25,
                                 filter) {

  if (length(filter) > 0) {
    data <- data[grepl(
      filter$filter_logic,
      data[, get(filter$filter_var)],
      perl = TRUE
    ), ]
  }

  # remove missings
  data <- data[!is.na(get(var)), ]

  # TODO we need to define variable types at the dataimport
  data[, (var) := factor(get(var))]

  # if there are more levels than specified in levellimit
  #% (default = 20)
  if (data[, nlevels(get(var))] > levellimit) {
    tabdat <- data[, .N, by = var][
      order(get("N"), decreasing = TRUE)
    ]
    tabdat_out <- tabdat[1:levellimit, ]
  } else {
    tabdat_out <- data[, .N, by = var][
      order(get("N"), decreasing = TRUE)
    ]
  }
  tabdat_out[, "valid" := (get("N") / nrow(data)) * 100]
  colnames(tabdat_out)[c(2, 3)] <- c("Freq", "% Valid")
  return(tabdat_out)
}
