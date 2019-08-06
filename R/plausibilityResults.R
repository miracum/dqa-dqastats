#' @title atempPausiResults_ helper function
#'
#' @description Internal function to generate the results of the 'Atemporal Plausibility' checks.
#'
#' @inheritParams descriptiveResults_
#'
#' @export
#'
atempPausiResults_ <- function(rv, source_db, headless = FALSE){

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)){
    # Create a Progress object
    progress1 <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress1$close())
    progress1$set(message = "Getting plausibility descriptions", value = 0)

    # progress 2
    progress2 <- shiny::Progress$new()
    on.exit(progress2$close())
    progress2$set(message = "Calculating plausibility counts", value = 0)

    # progress 3
    progress3 <- shiny::Progress$new()
    on.exit(progress3$close())
    progress3$set(message = "Calculating plausibility statistics", value = 0)
  }

  for (i in names(rv$pl_vars_filter)){

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Getting plausibility descriptions of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress1$inc(1/length(names(rv$pl_vars_filter)), detail = paste("... working at description of", i, "..."))
    }

    # generate descriptions
    desc_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^pl\\.", get("key")),][get("name")==i,c("name", "source_system", "source_variable_name",
                                                                                                 "source_table_name", "description",
                                                                                                 "sql_from", "sql_join_on", "sql_join_table", "sql_join_type", "sql_where",
                                                                                                 "variable_type", "value_set", "value_threshold", "missing_threshold"), with=F]

    if (nrow(desc_dat)>1){
      outlist[[rv$pl_vars_filter[[i]]]]$description <- calcPlausiDescription(desc_dat, rv, sourcesystem = source_db)
    } else {
      cat("\nError occured during creating descriptions of source system\n")
    }


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating plausibility counts of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress2$inc(1/length(names(rv$pl_vars_filter)), detail = paste("... working at counts of", i, "..."))
    }

    # generate counts
    cnt_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^pl\\.", get("key")),][get("name")==i,c("source_system", "source_variable_name", "source_table_name", "variable_type", "key", "variable_name"), with=F]


    if (length(cnt_dat[,unique(get("variable_name"))]) == 1){
      outlist[[rv$pl_vars_filter[[i]]]]$counts <- calcCounts(cnt_dat, cnt_dat[,unique(get("variable_name"))], rv, sourcesystem = source_db, plausibility = TRUE)
    } else {
      cat("\nError occured during creating counts\n")
    }


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating plausibility statistics of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress3$inc(1/length(names(rv$pl_vars_filter)), detail = paste("... working at statistics of", i, "..."))
    }

    # generate counts
    stat_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^pl\\.", get("key")),][get("name")==i,c("source_system", "source_variable_name", "source_table_name", "variable_name", "variable_type", "key"),with=F]

    if (stat_dat[,unique(get("variable_type"))] == "factor"){
      outlist[[rv$pl_vars_filter[[i]]]]$statistics <- calcCatStats(stat_dat, stat_dat[,unique(get("variable_name"))], rv, sourcesystem = source_db, plausibility = TRUE)
      # for target_data; our data is in rv$list_target$key
    } else {
      outlist[[rv$pl_vars_filter[[i]]]]$statistics <- calcNumStats(stat_dat, stat_dat[,unique(get("variable_name"))], rv, sourcesystem = source_db, plausibility = TRUE)
    }
  }
  if (isFALSE(headless)){
    progress1$close()
    progress2$close()
    progress3$close()
  }
  return(outlist)
}
