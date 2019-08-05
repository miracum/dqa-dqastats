#' @title descriptiveResults_ helper function
#'
#' @description Internal function to generate the descriptive results.
#'
#' @inheritParams loadSource_
#' @inheritParams DQA
#'
#' @export
#'
descriptiveResults_ <- function(rv, source_db, headless = FALSE){

  # initialize outlist
  outlist <- list()

  for (i in names(rv$variable_list)){

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Getting variable descriptions of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Getting variable descriptions", value = 0)

      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(names(rv$variable_list)), detail = paste("... working at description of", i, "..."))
    }

    # generate descriptions
    desc_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^dt\\.", get("key")),][get("variable_name")==rv$variable_list[[i]],c("name", "source_system", "source_variable_name",
                                                                                                                              "source_table_name", "fhir", "description",
                                                                                                                              "variable_type", "value_set", "value_threshold", "missing_threshold"),with=F]

    if (nrow(desc_dat)>1){
      outlist[[rv$variable_list[[i]]]]$description <- calcDescription(desc_dat, rv, sourcesystem = source_db)
    } else {
      msg <- "Error occured during creating descriptions of source system"
      cat("\n", msg, "\n")
      if (isFALSE(headless)){
        shinyjs::logjs(msg)
      }
    }

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating variable counts of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Calculating variable counts", value = 0)

      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(names(rv$variable_list)), detail = paste("... calculating counts of", i, "..."))
    }

    # generate counts
    cnt_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^dt\\.", get("key")),][get("variable_name")==rv$variable_list[[i]],c("source_system", "source_variable_name", "source_table_name", "variable_type", "key"),with=F]

    outlist[[rv$variable_list[[i]]]]$counts <- calcCounts(cnt_dat, rv$variable_list[[i]], rv, sourcesystem = source_db)


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating variable statistics of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Calculating variable statistics", value = 0)

      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(names(rv$variable_list)), detail = paste("... calculating statistics of", i, "..."))
    }


    # generate statistics
    stat_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^dt\\.", get("key")),][get("variable_name")==rv$variable_list[[i]],c("source_system", "source_variable_name", "source_table_name", "variable_type", "key"),with=F]

    if (stat_dat[,unique(get("variable_type"))] == "factor"){
      outlist[[rv$variable_list[[i]]]]$statistics <- calcCatStats(stat_dat, rv$variable_list[[i]], rv, sourcesystem = source_db)
      # for target_data; our data is in rv$list_target$key
    } else {
      outlist[[rv$variable_list[[i]]]]$statistics <- calcNumStats(stat_dat, rv$variable_list[[i]], rv, sourcesystem = source_db)
    }
  }
  return(outlist)
}
