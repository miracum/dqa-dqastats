# DQAstats - Perform data quality assessment (DQA) of electronic health records (EHR)
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


#' @title completeness_ helper function
#'
#' @description Internal function to perform missing analysis.
#'
#' @inheritParams valueConformance_
#'
#' @export
#'
completeness_ <- function(results, headless = FALSE){
  # get names
  obj_names <- names(results)

  # initialize final list to output
  outlist <- data.table::data.table(cbind(
    "Variable" = character(0),
    "Missings (source)" = integer(0),
    "Missings [%] (source)" = numeric(0),
    "Missings (target)" = integer(0),
    "Missings [%] (target)" = numeric(0)
  ))

  if (isFALSE(headless)){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Performing missing analysis", value = 0)
  }

  # loop over objects
  for (i in obj_names){

    msg <- paste("Performing missing analysis", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(obj_names), detail = paste("... checking", i, "..."))
    }

    count_out <- results[[i]]$counts


    outlist <- rbind(outlist,
                     data.table::data.table(cbind(
                       "Variable" = i,
                       "Missings (source)" = count_out$source_data$cnt$missings,
                       "Missings [%] (source)" = round(count_out$source_data$cnt$missings / count_out$source_data$cnt$n, 4) * 100,
                       "Missings (target)" = count_out$target_data$cnt$missings,
                       "Missings [%] (target)" = round(count_out$target_data$cnt$missings / count_out$source_data$cnt$n, 4) * 100
                     ))
    )
  }

  if (isFALSE(headless)){
    progress$close()
  }

  return(outlist)
}
