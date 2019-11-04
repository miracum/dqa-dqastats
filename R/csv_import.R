#' @title Perform Data Quality Assessment of Electronic Health Records.
#'
#' @description This function performs a data quality assessment (DQA)
#'   of electronic health records (EHR).#'
#'
#' @param target_config A character string. The path to the config.yml-file
#'   containing the target database configuration.
#' @param source_config A character string. The path to the config.yml-file
#'   containing the source database configuration.
#' @param target_db A character string. The name of the target database.
#'   This string must be conform with the corresponding config section
#'   in the config.yml-file.
#' @param source_db A character string. The name of the source database.
#'   This string must be conform with the corresponding config section
#'   in the config.yml-file.
#' @param utils A character string. The path to the utils-folder,
#' containing the requires app utilities. For a detailed description
#' please visit \url{#TODO}.
#'
#' @inheritParams create_helper_vars
#'
csv_import <- function(mdr,
                       inputdir,
                       sourcesystem) {

  sourcesystem <- "exampleCSV"

  # for debuggin only define variables here
  inputdir <- system.file("demo_data", package = "DQAstats")
  selfutils <-
    system.file("demo_data/utilities", package = "DQAstats")
  mdr_filename <- "mdr_example_data.csv"

  inputdir <- cleanPathName_(inputdir)
  selfutils <- cleanPathName_(selfutils)

  mdr <- readMDR_(utils = selfutils,
                  mdr_filename = mdr_filename)

  # original beginning of function
  inputdir <- cleanPathName_(inputdir)
  selfutils <- cleanPathName_(selfutils)



  available_systems <- mdr[get("source_system") == sourcesystem &
                             get("system_type") == "csv", ]

  stopifnot(# fix test for multiple files
    (!any(
      !available_systems[, unique(get("source_table_name"))] %in%
        list.files(inputdir)
    ))
  )

  for (inputfile in available_systems[, unique(get("source_table_name"))]) {
    cat(paste0("\nLoading file ", inputfile, "\n\n"))

    input_vars <- available_systems[get("source_table_name") ==
                                      inputfile, c("source_variable_name",
                                                   "variable_type")]

    select_cols <- unlist(
      sapply(
        input_vars$source_variable_name,
        FUN = function(x) {
          map_var_types(
            input_vars[get("source_variable_name") == x, "variable_type"]
          )
        },
        simplify = TRUE,
        USE.NAMES = TRUE
      )
    )


    outdat <- data.table::fread(
      paste0(inputdir, inputfile),
      #   select = names(select_cols),
      #   colClasses = select_cols,
      header = T,
      na.strings = "",
      stringsAsFactors = TRUE
    )
  }

}


map_var_types <- function(string) {
  if (string == "permittedValues") {
    outdat <- "factor"
  } else if (string == "integer") {
    outdat <- "numeric"
  } else if (string == "string") {
    outdat <- "character"
  } else if (string == "calendar") {
    outdat <- "character"
  } else if (string == "float") {
    outdat <- "numeric"
  } else {
    outdat <- NULL
  }
  return(outdat)
}
