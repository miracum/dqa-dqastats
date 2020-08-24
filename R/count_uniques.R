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
            break
          }
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
