#' @title valueConformance_ helper function
#'
#' @description Internal function to perform value conformance checks.
#'
#' @inheritParams descriptiveResults_
#' @param results A list object. The list should contain the results of either 'rv$results_descriptive' or 'rv$results_plausibility_atemporal'.
#'
#' @export
#'
valueConformance_ <- function(results, headless = FALSE){
  # get names
  obj_names <- names(results)

  # initialize final list to output
  outlist <- list()

  # loop over objects
  for (i in obj_names){
    desc_out <- results[[i]]$description
    count_out <- results[[i]]$counts
    stat_out <- results[[i]]$statistics

    for (j in c("source_data", "target_data")){
      d_out <- desc_out[[j]]
      s_out <- stat_out[[j]]

      value_set <- d_out$checks$value_set

      if (!is.na(value_set)){

        msg <- paste("Performing value conformance check", i)
        cat("\n", msg, "\n")
        if (isFALSE(headless)){
          shinyjs::logjs(msg)

          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if there's an error
          on.exit(progress$close())
          progress$set(message = "Performing value conformance check", value = 0)

          # Increment the progress bar, and update the detail text.
          progress$inc(1/1, detail = paste("... checking", i, "..."))
        }

        # initialize outlist
        outlist2 <- list()

        # parse value_set
        value_set <- jsonlite::fromJSON(value_set)

        if (d_out$checks$var_type == "factor"){
          # get valueset from mdr
          value_set <- unlist(strsplit(value_set$value_set, ", ", fixed = T))
          # get levels from results
          levels_results <-s_out[,levels(get(colnames(s_out)[1]))]
          # compare levels from results to constraints from valueset (TRUE = constraint_error)
          outlist2$conformance_error <- any(levels_results %!in% value_set)
          # if TRUE, get those values, that do not fit
          outlist2$conformance_results <- ifelse(isTRUE(outlist2$conformance_error),
                                                 paste0("Levels that are not conform with value_set constraints:\n", paste(levels_results[levels_results %!in% value_set], collapse = "\n")),
                                                 "No 'value conformance' issues found.")
        } else if (d_out$checks$var_type %in% c("integer", "numeric")){
          error_flag <- FALSE

          # set colnames (we need them here to correctly select the data)
          colnames(s_out) <- c("name", "value")

          # TODO add value_thresholds here as tolerance-/border zone
          result_min <- as.numeric(s_out[get("name")=="Minimum",get("value")])
          result_max <- as.numeric(s_out[get("name")=="Maximum",get("value")])

          # compare levels from results to constraints from valueset (TRUE = constraint_error)
          if (result_min < value_set$min){
            cat(paste0(i, "/ ", j, ": result_min < value_set$min\n"))
            error_flag <- TRUE
          }

          if (result_max > value_set$max){
            cat(paste0(i, "/ ", j, ": result_max > value_set$max\n"))
            error_flag <- TRUE
          }
          outlist2$conformance_error <- error_flag
          outlist2$conformance_results <- ifelse(isTRUE(error_flag),
                                                      "Extrem values are not conform with value_set constraints.",
                                                      "No 'value conformance' issues found.")
        }
        outlist[[i]][[j]] <- outlist2
      }
    }

  }

  return(outlist)
}


#' @title valueConformanceChecks_ helper function
#'
#' @description Internal function to perform value conformance checks.
#'
#' @param results A list object. The list should contain the results of the function \code{valueConformance_}.
#'
#' @export
#'
valueConformanceChecks_ <- function(results){
  # get names
  obj_names <- names(results)

  # initialize output table
  out <- data.table::data.table("Variable" = character(0),
                                "Check Source Data" = character(0),
                                "Check Target Data" = character(0))


  for (i in obj_names){
    error_source <- ifelse(results[[i]]$source_data$conformance_error, "failed", "passed")
    error_target <- ifelse(results[[i]]$target_data$conformance_error, "failed", "passed")
    out <- rbind(out, data.table::data.table("Variable" = i,
                                             "Check Source Data" = error_source,
                                             "Check Target Data" = error_target))
  }
  return(out)
}
