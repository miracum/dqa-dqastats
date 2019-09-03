# DQAstats - A package, created to perform data quality assessment (DQA) of electronic health records (EHR)
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


#' @title descriptiveResults_ helper function
#'
#' @description Internal function to generate the descriptive results.
#'
#' @inheritParams loadSource_
#' @inheritParams DQA
#'
#' @export
#'
descriptiveResults_ <- function(rv, headless = FALSE){

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)){
    # Create a Progress object
    progress1 <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress1$close())
    progress1$set(message = "Getting variable descriptions", value = 0)

    # progress 2
    progress2 <- shiny::Progress$new()
    on.exit(progress2$close())
    progress2$set(message = "Calculating variable counts", value = 0)

    # progress 3
    progress3 <- shiny::Progress$new()
    on.exit(progress3$close())
    progress3$set(message = "Calculating variable statistics", value = 0)
  }

  for (i in names(rv$variable_list)){

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Getting variable descriptions of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress1$inc(1/length(names(rv$variable_list)), detail = paste("... working at description of", i, "..."))
    }

    # generate descriptions
    desc_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^dt\\.", get("key")),][get("variable_name")==rv$variable_list[[i]],c("designation", "source_system", "source_variable_name",
                                                                                                                              "source_table_name", "fhir", "definition",
                                                                                                                              "variable_type", "constraints", "value_threshold", "missing_threshold"),with=F]

    if (nrow(desc_dat)>1){
      outlist[[rv$variable_list[[i]]]]$description <- calcDescription(desc_dat, rv)
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
      # Increment the progress bar, and update the detail text.
      progress2$inc(1/length(names(rv$variable_list)), detail = paste("... calculating counts of", i, "..."))
    }

    # generate counts
    cnt_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^dt\\.", get("key")),][get("variable_name")==rv$variable_list[[i]],c("source_system", "source_variable_name", "source_table_name", "variable_type", "key"),with=F]

    outlist[[rv$variable_list[[i]]]]$counts <- calcCounts(cnt_dat, rv$variable_list[[i]], rv, datamap = TRUE)


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating variable statistics of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress3$inc(1/length(names(rv$variable_list)), detail = paste("... calculating statistics of", i, "..."))
    }


    # generate statistics
    stat_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^dt\\.", get("key")),][get("variable_name")==rv$variable_list[[i]],c("source_system", "source_variable_name", "source_table_name", "variable_type", "key"),with=F]

    if (stat_dat[,unique(get("variable_type"))] %in% c("permittedValues", "string")){
      outlist[[rv$variable_list[[i]]]]$statistics <- calcCatStats(stat_dat, rv$variable_list[[i]], rv)
      # for target_data; our data is in rv$list_target$key
    } else {
      outlist[[rv$variable_list[[i]]]]$statistics <- calcNumStats(stat_dat, rv$variable_list[[i]], rv)
    }
  }
  gc()
  if (isFALSE(headless)){
    progress1$close()
    progress2$close()
    progress3$close()
  }
  return(outlist)
}
