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


#' @title read_mdr helper function
#'
#' @description Internal function to read the meta data repository (MDR).
#'
#' @param mdr_filename A character string. The filename of your meta data
#'   repository (default: 'mdr.csv').
#'
#' @inheritParams dqa
#'
#' @export
#'
read_mdr <- function(utils_path, mdr_filename = "mdr.csv") {

  mdr <- data.table::fread(
    paste0(utils_path, "MDR/", mdr_filename),
    header = T
  )

  # fix columns that contain json strings (due to multiple quotation marks)
  mdr[, ("constraints") := gsub("\"\"", "\"", get("constraints"))][
    get("constraints") == "", ("constraints") := NA
    ]
  mdr[, ("plausibility_relation") := gsub(
    "\"\"",
    "\"",
    get("plausibility_relation")
  )][
    get("plausibility_relation") == "", ("plausibility_relation") := NA
    ]
  mdr[, ("filter") := gsub("\"\"", "\"", get("filter"))][
    get("filter") == "", ("filter") := NA
    ]

  # fix representation of missing values in all columns
  for (i in colnames(mdr)) {
    mdr[get(i) == "", (i) := NA]
  }

  return(mdr)
}


#' @title create_helper_vars helper function
#'
#' @description Internal function to create necessary variables from the
#'   meta data repository (MDR).
#'
#' @param mdr A data.table object containing the MDR.
#'
#' @param target_db A character string. The name of the target database.
#' This string must be conform with the corresponding config section
#' in the config.yml-file.
#'
#' @param source_db A character string. The name of the source database.
#' This string must be conform with the corresponding config section
#' in the config.yml-file.
#'
#' @export
#'
# create variables from mdr
create_helper_vars <- function(mdr,
                               target_db,
                               source_db) {

  # We only allow one (system) type per system name. There can't e.g. be
  # system types "csv" and "postgres" both with the system_name "data":
  stopifnot(
    length(mdr[get("source_system_name") ==
                 source_db, unique(get("source_system_type"))]) == 1,
    length(mdr[get("source_system_name") ==
                 target_db, unique(get("source_system_type"))]) == 1
  )

  outlist <- list()

  for (f in c("source", "target")) {
    if (mdr[get("source_system_name") ==
            eval(parse(text = paste0(f, "_db"))),
            unique(get("source_system_type"))] == "csv") {
      # if we have csv as input format, find the "keys" in "source_table_name"
      outlist[[paste0("keys_", f)]] <-
        mdr[get("variable_name") != "undefined", ][
          get("source_system_name") == eval(parse(text = paste0(f, "_db"))),
          unique(get("source_table_name"))
          ]
    } else if (mdr[get("source_system_name") ==
                   eval(parse(text = paste0(f, "_db"))),
                   unique(get("source_system_type"))] %in%
               c("postgres", "oracle")) {
      outlist[[paste0("keys_", f)]] <-
        mdr[get("variable_name") != "undefined", ][
          get("source_system_name") == eval(parse(text = paste0(f, "_db"))),
          unique(get("key"))]
    }
  }


  # get list of DQ-variables of interest (convention: definition has only to
  # be assigned for the source system)
  dqa_assessment <- mdr[get("source_system_name") == source_db &
                          get("dqa_assessment") == 1, ][
                            order(get("source_table_name")), c(
                              "designation",
                              "source_variable_name",
                              "variable_name",
                              "variable_type",
                              "key",
                              "source_table_name"
                            ), with = F]

  # get only keys with corresponding keys in target_db
  dqa_assessment_intersect <- intersect(
    dqa_assessment[, get("key")],
    mdr[get("source_system_name") == target_db, get("key")]
  )

  outlist$dqa_assessment <- dqa_assessment[get("key") %in%
                                             dqa_assessment_intersect, ]


  for (f in c("source", "target")) {
    if ("csv" != mdr[get("source_system_name") ==
                          eval(parse(text = paste0(f, "_db"))),
                          unique(get("source_system_type"))]) {
      outlist[[paste0("keys_", f)]] <-
        outlist[[paste0("keys_", f)]][outlist[[paste0("keys_", f)]] %in%
                                        dqa_assessment_intersect]
    }
  }

  # variable_list
  variable_list <- outlist$dqa_assessment[order(get("designation"))]
  outlist$variable_list <- sapply(
    variable_list[, get("designation")],
    function(x) {
      variable_list[get("designation") == x, get("variable_name")]
    }, simplify = F, USE.NAMES = T)

  # get list of pl_vars for plausibility analyses
  pl_vars <-
    mdr[!is.na(get("plausibility_relation")) &
          get("source_system_name") == source_db,
        ][
          order(get("source_table_name")), c(
            "designation",
            "variable_name",
            "variable_type",
            "plausibility_relation"
          ), with = F]
  pl_vars <- pl_vars[!duplicated(pl_vars), ]

  # atemporal plausibility
  ap_filter <- lapply(
    pl_vars[, get("plausibility_relation")],
    function(x) {
      names(jsonlite::fromJSON(x))
    }) == "atemporal"

  # uniqueness plausibility
  up_filter <- lapply(
    pl_vars[, get("plausibility_relation")],
    function(x) {
      names(jsonlite::fromJSON(x))
    }) == "uniqueness"

  outlist$pl$atemp_vars <- pl_vars[ap_filter, ]
  outlist$pl$uniq_vars <- pl_vars[up_filter, ]

  # get available variable names
  available_variables <-
    mdr[get("variable_name") != "undefined", ][
      get("source_system_name") == source_db &
        get("key") %in% dqa_assessment_intersect,
      unique(get("variable_name"))
    ]

  if (nrow(outlist$pl$atemp_vars) > 0) {
    # look, if all required variables are within dqa_assessment
    for (r in seq_len(nrow(outlist$pl$atemp_vars))) {
      # get required helper variables
      js <- jsonlite::fromJSON(
        outlist$pl$atemp_vars[r, get("plausibility_relation")]
      )
      js_names <- names(js$atemporal)
      js_names <- sapply(
        X = strsplit(js_names, ".", fixed = T),
        FUN = function(x) {
          x[1]
        },
        USE.NAMES = F,
        simplify = T
      )

      if (is.null(outlist$pl$atemp_helper_vars)) {
        outlist$pl$atemp_helper_vars <- js_names
      } else {
        outlist$pl$atemp_helper_vars <- c(
          outlist$pl$atemp_helper_vars,
          setdiff(js_names, outlist$pl$atemp_helper_vars)
        )
      }
    }

    # check, if all variable names, that are required for atemporal checks
    # are available
    if (sum(js_names %in% available_variables) !=
        length(js_names)) {
      outlist$pl$atemp_possible <- FALSE
    } else {
      outlist$pl$atemp_possible <- TRUE
    }
  } else {
    outlist$pl$atemp_possible <- FALSE
  }


  if (nrow(outlist$pl$uniq_vars) > 0) {
    # look, if all required variables are within dqa_assessment
    for (r in seq_len(nrow(outlist$pl$uniq_vars))) {
      # get required helper variables
      js <- jsonlite::fromJSON(
        outlist$pl$uniq_vars[r, get("plausibility_relation")]
      )
      js_names <- names(js$uniqueness)
      js_names <- sapply(
        X = strsplit(js_names, ".", fixed = T),
        FUN = function(x) {
          x[1]
        },
        USE.NAMES = F,
        simplify = T
      )

      if (is.null(outlist$pl$uniq_helper_vars)) {
        outlist$pl$uniq_helper_vars <- js_names
      } else {
        outlist$pl$uniq_helper_vars <- c(
          outlist$pl$uniq_helper_vars,
          setdiff(js_names, outlist$pl$uniq_helper_vars)
        )
      }
    }

    # check, if all variable names, that are required for atemporal checks
    # are available
    if (sum(js_names %in% available_variables) !=
        length(js_names)) {
      outlist$pl$uniq_possible <- FALSE
    } else {
      outlist$pl$uniq_possible <- TRUE
    }
  } else {
    outlist$pl$uniq_possible <- FALSE
  }

  return(outlist)
}
