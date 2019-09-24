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
  data_names <- mdr[get("data_map") == 1 & get("source_system") == db, c("variable_name", "designation"), with=F]

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
                                    "n" = character(0),
                                    "valids" = character(0),
                                    "missings" = character(0),
                                    "distinct" = character(0))

      for (j in obj_names){
        out <- rbind(out, data.table::data.table("variable" = data_names[get("variable_name")==j, get("designation")],
                                                 "n" = results[[j]]$counts[[i]]$cnt$n,
                                                 "valids" = results[[j]]$counts[[i]]$cnt$valids,
                                                 "missings" = results[[j]]$counts[[i]]$cnt$missings,
                                                 "distinct" = results[[j]]$counts[[i]]$cnt$distinct))
      }
      outlist[[i]] <- out
    }
    return(outlist)
  }
}

