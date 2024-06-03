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


#' @title read_mdr helper function
#'
#' @description Internal function to read the meta data repository (MDR).
#'
#' @param mdr_filename A character string. The filename of your meta data
#'   repository (default: 'mdr.csv').
#'
#' @inheritParams dqa
#'
#' @return A data.table containing the metadata repository which is imported
#'   from the CSV file provided with `{utils_path}/MDR/{mdr_filename}`.
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename = mdr_filename
#' )
#'
#' @export
#'
read_mdr <- function(utils_path = NULL, mdr_filename = "mdr.csv") {

  if (!is.null(utils_path)) {
    mdr_path <- file.path(
      file.path(
        DIZtools::clean_path_name(utils_path, remove.slash = TRUE),
        "MDR"
      ),
      mdr_filename
    )
  } else {
    mdr_path <- mdr_filename
  }

  stopifnot(
    file.exists(mdr_path)
  )

  mdr <- data.table::fread(
    file = mdr_path,
    header = TRUE
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
  mdr[mdr == ""] <- NA

  # make empty definitions readable to avoid errors
  mdr[
    is.na(get("definition")),
    ("definition") := "(The definition is missing in the MDR.)"
  ]

  ## Remove rows with "undefined" key or variablename:
  mdr <- mdr[!(get("key") == "undefined" |
                 get("variable_name") == "undefined"), ]

  return(mdr)
}


#' @title create_helper_vars helper function
#'
#' @description Internal function to create necessary variables from the
#'   meta data repository (MDR).
#'
#' @param mdr A data.table object containing the MDR.
#'
#' @param source_db A character string. The name of the source database.
#' This string must be conform with the corresponding config section
#' in the config.yml-file.
#'
#' @param target_db A character string. The name of the target database.
#' This string must be conform with the corresponding config section
#' in the config.yml-file.
#'
#' @return A list with results from the analysis of the metadata repository
#'   (MDR) with the following items:
#'   \describe{
#'   \item{keys_source}{A character vector with the different values of the
#'   'key' field from the MDR for the source data system.}
#'   \item{keys_target}{A character vector with the different values of the
#'   'key' field from the MDR for the target data system.}
#'   \item{dqa_assessment}{A data.table with a subset of the MDR for the
#'   dataelement entries with the field 'dqa_assessment' = 1.}
#'   \item{variable_list}{A mapping list from MDR variable names (MDR field
#'   'designation') to DQA tool internal variable names (MDR field
#'   'variable_name').}
#'   \item{pl}{A nested list with items regarding the plausibility checks}
#'   \describe{
#'   \item{atemp_vars}{A data.table with a subset of the MDR with dataelements
#'   that are associated with atemporal plausibility checks.}
#'   \item{uniq_vars}{A data.table with a subset of the MDR with dataelements
#'   that are associated with uniqueness plausibility checks.}
#'   \item{atemp_helper_vars}{A character vector with further dataelements that
#'   are required to perform the atemporal plausibility checks.}
#'   \item{atemp_possible}{A boolean to indicate if all dataelements required
#'   to perform the atemporal plausibility checks are available in the dataset.}
#'   \item{uniq_helper_vars}{A character vector with further dataelements that
#'   are required to perform the uniqueness plausibility checks.}
#'   \item{uniq_possible}{A boolean to indicate if all dataelements required
#'   to perform the uniqueness plausibility checks are available in the
#'   dataset.}
#'   }
#'   }
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename = mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' create_helper_vars(
#'   mdr = mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' )
#'
#' @export
#'
# create variables from mdr
create_helper_vars <- function(mdr,
                               source_db,
                               target_db) {
  # We only allow one (system) type per system name. There can't e.g. be
  # system types "csv" and "postgres" both with the system_name "data":
  if (!(length(mdr[get("source_system_name") ==
                   source_db, unique(get("source_system_type"))]) == 1 &&
        length(mdr[get("source_system_name") ==
                   target_db, unique(get("source_system_type"))]) == 1)) {
    DIZtools::feedback(
      print_this = paste0(
        "Error in create_helper_vars(mdr, ",
        "source_db = '",
        source_db,
        "', ",
        "target_db = '",
        target_db,
        "')."
      ),
      type = "Error",
      findme = "bee3956587"
    )
    stop("See error above.")
  }

  outlist <- list()

  for (f in c("source", "target")) {
    if (mdr[get("source_system_name") ==
            eval(parse(text = paste0(f, "_db"))),
            unique(get("source_system_type"))] == "csv") {
      # if we have csv as input format, find the "keys" in "source_table_name"
      outlist[[paste0("keys_", f)]] <-
        mdr[get("variable_name") != "undefined" & get("key") != "undefined", ][
          get("source_system_name") == eval(parse(text = paste0(f, "_db"))),
          unique(get("source_table_name"))
        ]
    } else if (mdr[get("source_system_name") ==
                   eval(parse(text = paste0(f, "_db"))),
                   unique(get("source_system_type"))] %in%
               c("postgres", "oracle")) {
      outlist[[paste0("keys_", f)]] <-
        mdr[get("variable_name") != "undefined" & get("key") != "undefined", ][
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
                            ), with = FALSE]

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
    X = variable_list[, get("designation")],
    FUN = function(x) {
      variable_list[get("designation") == x, get("variable_name")]
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

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
      ), with = FALSE]
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
    mdr[get("variable_name") != "undefined" & get("key") != "undefined", ][
      get("source_system_name") == source_db &
        get("key") %in% dqa_assessment_intersect,
      unique(get("variable_name"))
    ]

  if (nrow(outlist$pl$atemp_vars) > 0) {
    atemp_helper_vars <- character(0)
    # look, if all required variables are within dqa_assessment
    for (r in seq_len(nrow(outlist$pl$atemp_vars))) {
      # get required helper variables
      js <- jsonlite::fromJSON(
        outlist$pl$atemp_vars[r, get("plausibility_relation")]
      )
      js_names <- outlist$pl$atemp_vars[r, get("variable_name")]

      add_atemp_vars <- sapply(
        X = names(js$atemporal),
        FUN = function(x) {
          gsub("(\\.\\d+)?$", "", x)
        },
        USE.NAMES = FALSE,
        simplify = TRUE
      ) %>%
        unique()

      js_names <- c(js_names, add_atemp_vars)

      add_join_crit_vars <- sapply(
        X = js$atemporal,
        FUN = function(x) {
          x["join_crit"]
        },
        USE.NAMES = FALSE,
        simplify = TRUE
      ) %>%
        unlist() %>%
        unique()

      js_names <- c(js_names, add_join_crit_vars)

      atemp_helper_vars <- c(atemp_helper_vars, js_names)
    }

    outlist$pl$atemp_helper_vars <- unique(atemp_helper_vars)

    # check, if all variable names, that are required for atemporal checks
    # are available
    if (sum(outlist$pl$atemp_helper_vars %in% available_variables) !=
        length(outlist$pl$atemp_helper_vars)) {
      outlist$pl$atemp_possible <- FALSE
    } else {
      outlist$pl$atemp_possible <- TRUE
    }
  } else {
    outlist$pl$atemp_possible <- FALSE
  }


  if (nrow(outlist$pl$uniq_vars) > 0) {
    unique_helper_vars <- character(0)
    # look, if all required variables are within dqa_assessment
    for (r in seq_len(nrow(outlist$pl$uniq_vars))) {
      # get required helper variables
      js <- jsonlite::fromJSON(
        outlist$pl$uniq_vars[r, get("plausibility_relation")]
      )
      js_names <- outlist$pl$uniq_vars[r, get("variable_name")]

      add_unique_vars <- sapply(
        X = names(js$uniqueness),
        FUN = function(x) {
          gsub("(\\.\\d+)?$", "", x)
        },
        USE.NAMES = FALSE,
        simplify = TRUE
      ) %>%
        unique()

      js_names <- c(js_names, add_unique_vars)

      unique_helper_vars <- c(unique_helper_vars, js_names)
    }

    outlist$pl$uniq_helper_vars <- unique(unique_helper_vars)

    # check, if all variable names, that are required for atemporal checks
    # are available
    if (sum(outlist$pl$uniq_helper_vars %in% available_variables) !=
        length(outlist$pl$uniq_helper_vars)) {
      outlist$pl$uniq_possible <- FALSE
    } else {
      outlist$pl$uniq_possible <- TRUE
    }
  } else {
    outlist$pl$uniq_possible <- FALSE
  }

  return(outlist)
}
