#' @title etlChecks_ helper function
#'
#' @description Internal function to perform etl conformance checks.
#'
#' @param results A list object. The list should contain the results 'rv$results_descriptive'.
#'
#' @export
#'
# quick checks
etlChecks_ <- function(results){

  # get names
  obj_names <- names(results)

  # initialize output table
  out <- data.table::data.table("Variable" = character(0),
                    "Check Distincts" = character(0),
                    "Check Valids" = character(0))


  for (i in obj_names){
    check_distinct <- ifelse(results[[i]]$counts$source_data$cnt$distinct == results[[i]]$counts$target_data$cnt$distinct,
                             "passed", "failed")
    check_valids <- ifelse(results[[i]]$counts$source_data$cnt$valids == results[[i]]$counts$target_data$cnt$valids,
                           "passed", "failed")
    out <- rbind(out, data.table::data.table("Variable" = i,
                                 "Check Distincts" = check_distinct,
                                 "Check Valids" = check_valids))
  }
  return(out)
}
