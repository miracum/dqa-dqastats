#' @title generateDatamap_ helper function
#'
#' @description Internal function to generate the dashboard data maps
#'
#' @inheritParams etlChecks_
#' @inheritParams createHelperVars_
#' @inheritParams testTargetDB_
#' @inheritParams loadSQLs_
#'
#' @export
#'
generateDatamap_ <- function(results, mdr, db, headless = FALSE){

  # get names
  data_names <- mdr[get("data_map") == 1 & get("source_system") == db, c("variable_name", "name"), with=F]

  if (nrow(data_names) < 1){
    msg <- "No variables suitable for the data map found in the MDR"
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
    }
    return(NULL)
  } else {

    obj_names <- data_names[,get("variable_name")]

    outlist <- list()

    for (i in c("source_data", "target_data")){
      # initialize output table
      out <- data.table::data.table("variable" = character(0),
                                    "distinct" = character(0),
                                    "valids" = character(0),
                                    "missings" = character(0))

      for (j in obj_names){
        out <- rbind(out, data.table::data.table("variable" = data_names[get("variable_name")==j, get("name")],
                                                 "distinct" = results[[j]]$counts[[i]]$cnt$distinct,
                                                 "valids" = results[[j]]$counts[[i]]$cnt$valids,
                                                 "missings" = results[[j]]$counts[[i]]$cnt$missings))
      }
      outlist[[i]] <- out
    }
    return(outlist)
  }
}

