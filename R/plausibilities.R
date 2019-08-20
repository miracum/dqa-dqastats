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


# read hard coded source plausibilities
# loadSourcePlausibilities <- function(plausi, data_source, headless=FALSE){
#
#   if (plausi == "pl.atemp.item01_source"){
#     # item02
#     outlist <- merge(data_source[["ICD.CSV"]][grepl("O[0-9]", get("condition_code_coding_code")),c("condition_encounter_identifier_value", "condition_code_coding_code"), with=F],
#                      data_source[["FALL.CSV"]][,c("encounter_identifier_value", "patient_gender", "patient_identifier_value"), with=F],
#                      by.x = "condition_encounter_identifier_value",
#                      by.y = "encounter_identifier_value",
#                      all.x = TRUE)
#
#
#   } else if (plausi == "pl.atemp.item02_source"){
#
#     # item03
#     outlist <- merge(data_source[["ICD.CSV"]][grepl("C5[1-8]", get("condition_code_coding_code")),c("condition_encounter_identifier_value", "condition_code_coding_code"), with=F],
#                      data_source[["FALL.CSV"]][,c("encounter_identifier_value", "patient_gender", "patient_identifier_value"), with=F],
#                      by.x = "condition_encounter_identifier_value",
#                      by.y = "encounter_identifier_value",
#                      all.x = TRUE)
#
#   } else if (plausi == "pl.atemp.item03_source"){
#     # item04
#     outlist <- merge(data_source[["ICD.CSV"]][grepl("C6[0-3]", get("condition_code_coding_code")),c("condition_encounter_identifier_value", "condition_code_coding_code"), with=F],
#                      data_source[["FALL.CSV"]][,c("encounter_identifier_value", "patient_gender", "patient_identifier_value"), with=F],
#                      by.x = "condition_encounter_identifier_value",
#                      by.y = "encounter_identifier_value",
#                      all.x = TRUE)
#
#   } else if (plausi == "pl.atemp.item04_source"){
#     # item05
#     outlist <- data_source[["FALL.CSV"]][grepl("05xx", get("encounter_hospitalization_class")),c("encounter_identifier_value",
#                                                                            "patient_gender",
#                                                                            "patient_identifier_value",
#                                                                            "encounter_hospitalization_class"), with=F]
#   }
#   return(outlist)
# }


# calcPlausiDescription <- function(desc_dat, rv, sourcesystem){
#   if (nrow(desc_dat)>1){
#     description <- list()
#     description$source_data <- list(name = desc_dat[get("source_system")==sourcesystem, get("name")],
#                                     description = desc_dat[get("source_system")==sourcesystem, get("description")],
#                                     var_name = desc_dat[get("source_system")==sourcesystem, get("source_variable_name")],
#                                     table_name = desc_dat[get("source_system")==sourcesystem, get("source_table_name")],
#                                     sql_from = desc_dat[get("source_system")==sourcesystem, get("sql_from")],
#                                     sql_join_table = desc_dat[get("source_system")==sourcesystem, get("sql_join_table")],
#                                     sql_join_type = desc_dat[get("source_system")==sourcesystem, get("sql_join_type")],
#                                     sql_join_on = desc_dat[get("source_system")==sourcesystem, get("sql_join_on")],
#                                     sql_where = desc_dat[get("source_system")==sourcesystem, get("sql_where")],
#                                     checks = list(var_type = desc_dat[get("source_system")==sourcesystem, get("variable_type")],
#                                                   value_set = desc_dat[get("source_system")==sourcesystem, get("value_set")],
#                                                   value_threshold = desc_dat[get("source_system")==sourcesystem, get("value_threshold")],
#                                                   missing_threshold = desc_dat[get("source_system")==sourcesystem, get("missing_threshold")]))
#
#     description$target_data <- list(var_name = desc_dat[get("source_system")==rv$db_target, get("source_variable_name")],
#                                     table_name = desc_dat[get("source_system")==rv$db_target, get("source_table_name")],
#                                     sql_from = desc_dat[get("source_system")==rv$db_target, get("sql_from")],
#                                     sql_join_table = desc_dat[get("source_system")==rv$db_target, get("sql_join_table")],
#                                     sql_join_type = desc_dat[get("source_system")==rv$db_target, get("sql_join_type")],
#                                     sql_join_on = desc_dat[get("source_system")==rv$db_target, get("sql_join_on")],
#                                     sql_where = desc_dat[get("source_system")==rv$db_target, get("sql_where")],
#                                     checks = list(var_type = desc_dat[get("source_system")==rv$db_target, get("variable_type")],
#                                                   value_set = desc_dat[get("source_system")==rv$db_target, get("value_set")],
#                                                   value_threshold = desc_dat[get("source_system")==rv$db_target, get("value_threshold")],
#                                                   missing_threshold = desc_dat[get("source_system")==rv$db_target, get("missing_threshold")]))
#     return(description)
#   } else {
#     return(NULL)
#   }
# }




getPlausisFromMDR <- function(pl.atemp_vars){
  uniques <- list()
  for (i in pl.atemp_vars[, get("variable_name")]){
    uniques[[i]] <- jsonlite::fromJSON(pl.atemp_vars[get("variable_name") == i, get("plausibility_relation")])[["atemporal"]]
  }
  return(uniques)
}


#' @title getAtempPlausis_ helper function
#'
#' @description Internal function to generate raw data for the 'Atemporal Plausibility' checks.
#'
#' @param pl.atemp_vars A data.table object. The object is created by \code{createHelperVars_} from the data represented in the metadata repository.
#'
#' @inheritParams atempPausiResults_
#' @inheritParams createHelperVars_
#'
#' @export
#'
getAtempPlausis_ <- function(rv, pl.atemp_vars, mdr, headless = FALSE){
  # pl.atemp_vars = rv$pl.atemp_vars
  # mdr = rv$mdr
  # sourcesystem = "p21csv"
  # headless = T

  outlist <- list()

  # get uniqueness checks from json
  uniques <- getPlausisFromMDR(pl.atemp_vars = pl.atemp_vars)

  # iterate over uniqueness checks
  for (i in names(uniques)){

    if (isFALSE(headless)){
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste("Getting atemporal plausibilities for", i), value = 0)
    }

    for (j in 1:length(uniques[[i]])){
      u <- uniques[[i]][[j]]
      u$variable_name <- names(uniques[[i]])[j]

      # workaround to hide shiny-stuff, when going headless
      msg <- paste("Getting atemporal plausibility", u$name)
      cat("\n", msg, "\n")
      if (isFALSE(headless)){
        shinyjs::logjs(msg)
        # Increment the progress bar, and update the detail text.
        progress$inc(1/length(uniques[[i]]), detail = paste("... working hard ..."))
      }

      # save some additional information about variable in output data
      outname <- tolower(u$name)

      # get information on source data
      for (k in c("source_data", "target_data")){

        src_flag <- ifelse(k == "source_data", rv$db_source, rv$db_target)

        # get descriptions
        outlist[[outname]][[k]]$name = u$name
        outlist[[outname]][[k]]$description = u$description
        outlist[[outname]][[k]]$var_dependent = i
        outlist[[outname]][[k]]$var_independent = u$variable_name
        if (!is.null(u$filter[[src_flag]])){
          outlist[[outname]][[k]]$filter = u$filter[[src_flag]]
        }
        if (!is.null(u$join_crit)){
          outlist[[outname]][[k]]$join_crit = u$join_crit
        }

        # prepare specific valueset for conformance checks:
        val_set <- u$checks$value_set[[src_flag]]
        outlist[[outname]][[k]]$checks$value_set <- jsonlite::toJSON(list("value_set" = val_set))

        # TODO this is yet tailored to §21
        if (k == "source_data"){
          u.key <- mdr[get("source_system") == rv$db_source & get("variable_name") == u$variable_name & get("dqa_assessment") == 1, get("source_table_name")]
          raw_data <- "data_source"
        } else {
          u.key <- mdr[get("source_system") == rv$db_target & get("variable_name") == u$variable_name & get("dqa_assessment") == 1, get("key")]
          raw_data <- "data_target"
        }

        if (i %in% colnames(rv[[raw_data]][[u.key]])){
          if (!is.null(u$filter[[src_flag]])){
            group_data <- rv[[raw_data]][[u.key]][grepl(u$filter[[src_flag]], get(u$variable_name)),c(i, u$variable_name),with=F]
          } else {
            group_data <- rv[[raw_data]][[u.key]][!is.na(get(u$variable_name)),c(i, u$variable_name),with=F]
          }
        } else {

          msg <- paste(i, "not in", colnames(rv[[raw_data]][[u.key]]))
          cat("\n", msg, "\n")
          if (isFALSE(headless)){
            shinyjs::logjs(msg)
          }
          # we need to find the correct data and merge
          if (k == "source_data"){
            m.key <- mdr[get("source_system") == rv$db_source & get("variable_name") == i & get("dqa_assessment") == 1, get("source_table_name")]
          } else {
            m.key <- mdr[get("source_system") == rv$db_target & get("variable_name") == i & get("dqa_assessment") == 1, get("key")]
          }

          if (!is.null(u$filter[[src_flag]])){
            m.x <- rv[[raw_data]][[u.key]][grepl(u$filter[[src_flag]], get(u$variable_name)),]
          } else {
            m.x <- rv[[raw_data]][[u.key]]
          }

          # look, if join_crit is already in our target table, if so, create m.y directly
          if (any(grepl(u$join_crit, colnames(rv[[raw_data]][[m.key]])))){
            m.y <- rv[[raw_data]][[m.key]]
          }  else {
            # else join another table
            if (k == "source_data"){
              j.key <- mdr[get("source_system") == rv$db_source & get("variable_name") == u$join_crit & get("dqa_assessment") == 1, get("source_table_name")]
            } else {
              j.key <- mdr[get("source_system") == rv$db_target & get("variable_name") == u$join_crit & get("dqa_assessment") == 1, get("key")]
            }

            # get colnames
            coln.x <- colnames(rv[[raw_data]][[m.key]])
            coln.y <- colnames(rv[[raw_data]][[j.key]])
            # find matching colname
            coln.x <- coln.x[lapply(coln.x, function(i){any(grepl(i, coln.y))}) == TRUE]
            coln.y <- coln.y[grepl(coln.x, coln.y)]

            m.y <- merge(x = rv[[raw_data]][[m.key]],
                                y = rv[[raw_data]][[j.key]],
                                by.x = coln.x,
                                by.y = coln.y,
                                all = T,
                                suffixes = c("", ""))

          }

          group_data <- merge(x = m.x,
                              y = m.y,
                              by.x = colnames(m.x)[grepl(u$join_crit, colnames(m.x))],
                              by.y = colnames(m.y)[grepl(u$join_crit, colnames(m.y))],
                              all.x = T,
                              suffixes = c("", ""))
          rm(m.x, m.y)
          gc()
        }
        outlist[[outname]][[k]][[raw_data]] <- group_data
        rm(group_data)
        gc()
      }
    }

    if (isFALSE(headless)){
      progress$close()
    }

  }
  return(outlist)
}
