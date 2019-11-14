# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019 Universitätsklinikum Erlangen
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


#' @title clean_path_name helper function
#'
#' @description Internal function to clean paths to have a tailing slash
#'
#' @param pathname A character string. A pathname to be cleaned
#'   (to have a tailing slash).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Both function calls will return "home/test/"
#' clean_path_name("home/test")
#' clean_path_name("home/test/")
#' }
#'
clean_path_name <- function(pathname) {
  return(gsub("([[:alnum:]])$", "\\1/", pathname))
}



#' @title transform_factors helper function
#'
#' @description Internal function to transform factors of the German §21-EHR
#'   billing data format
#'
#' @param vector A vector containing the data that is to be transformed.
#' @param transformation A character string. The name of the (predefined)
#'   transformation.
#'
#' @export
#'
# transform some factor variables
transform_factors <- function(vector, transformation) {

  vector <- gsub("[[:alnum:]]*\\:", "", vector)

  # quick and dirty workaround
  # TODO better would be to work with regex

  # Discharge
  if (transformation == "encounter_hospitalization_dischargeDisposition") {
    keep_values <- c("059", "069", "079", "089", "099", "109",
                     "119", "139", "179", "229", "239", "249", "259")
    # lt. (https://www.g-drg.de/Datenlieferung_gem._21_KHEntgG/
    # Dokumente_zur_Datenlieferung/Datensatzbeschreibung)

    if (any(vector %in% keep_values)) {
      trans_out <- ifelse(
        vector %in%
          keep_values,
        as.character(vector),
        ifelse(
          !is.na(vector),
          paste0(substr(vector, 1, 2), "x"),
          vector
        )
      )
    } else {
      trans_out <- ifelse(
        !is.na(vector),
        paste0(substr(vector, 1, 2), "x"),
        vector
      )
    }

    # Admission
  } else if (transformation == "encounter_hospitalization_class") {
    trans_out <- ifelse(
      !is.na(vector),
      paste0(substr(vector, 1, 2), "xx"),
      vector
    )

    # ICD
  } else if (transformation == "condition_code_coding_code") {
    trans_out <- gsub("\\+|\\*|\\!|\\#", "", vector)

    # all other variables that need to be transformed
  } else {
    trans_out <- vector
  }

  return(factor(trans_out))
  invisible(gc())
}


# define %notin% function
"%!in%" <- function(x, y) {
  return(
    !("%in%"(x, y))
  )
}


# needed for markdown formating
kable_table <- function(data) {
  if (" " %in% colnames(data)) {
    return(knitr::kable(data,
                        digits = 3,
                        format = "latex",
                        col.names = NULL) %>%
             kableExtra::kable_styling(full_width = F)
    )
  } else {
    return(knitr::kable(data,
                        digits = 3,
                        format = "latex") %>%
             kableExtra::row_spec(0, bold = TRUE) %>%
             kableExtra::kable_styling(full_width = F)
    )
  }
}


# time interval
#' @title time_interval helper function
#'
#' @description Internal function to get the time interval
#'
#' @param data The list object
#'   'rv$results_descriptive$EpisodeOfCare_period_end'
#'
#' @export
#'
time_interval <- function(data) {
  outlist <- list()
  outlist$start <- substr(
    as.character(data$statistics$target_data[1, ])[2],
    1,
    4
  )
  outlist$end <- substr(
    as.character(data$statistics$target_data[6, ])[2],
    1,
    4
  )
  return(outlist)
}


#' @title get_config helper function
#'
#' @description Internal function to read config files
#'
#' @param config_file A character string. The path to the config.yml-file
#'   containing the database configuration.
#' @param config_key A character string. The name of the corresponding
#'   database. This string must be conform with the corresponding config
#'   section in the config.yml-file.
#'
#' @export
#'
get_config <- function(config_file, config_key) {
  return(
    config::get(config_key, file = config_file)
  )
}

#' @title load_sqls helper function
#'
#' @description Internal function to load the SQL statements.
#'
#' @inheritParams dqa
#' @param db A character string. The name of the corresponding database.
#'
#' @export
#'
load_sqls <- function(utils_path, db) {
  return(jsonlite::fromJSON(paste0(utils_path, "SQL/SQL_", db, ".JSON")))
}
