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


#' @title readMDR_ helper function
#'
#' @description Internal function to read the meta data repository (MDR).
#'
#' @inheritParams DQA
#'
#' @export
#'
readMDR_ <- function(utils){
  mdr <- data.table::fread(paste0(utils, "MDR/mdr.csv"), header = T)
  mdr[,("value_set"):=gsub("\"\"", "\"", get("value_set"))][get("value_set")=="",("value_set"):=NA]
  mdr[,("plausibility_relation"):=gsub("\"\"", "\"", get("plausibility_relation"))][get("plausibility_relation")=="",("plausibility_relation"):=NA]
  return(mdr)
}


#' @title createHelperVars_ helper function
#'
#' @description Internal function to create necessar variables from the meta data repository (MDR).
#'
#' @param mdr A data.table object containing the MDR.
#' @inheritParams DQA
#'
#' @export
#'
# create variables from mdr
createHelperVars_ <- function(mdr, target_db, source_db){

  outlist <- list()

  # get keys
  outlist$keys_target <- mdr[get("key")!="undefined",][get("source_system")==target_db,unique(get("key"))]
  if (source_db == "p21csv"){
    # TODO workaround for csv-files
    outlist$keys_source <- mdr[get("key")!="undefined",][get("source_system")==source_db & !grepl("^pl\\.", get("key")), unique(get("source_table_name"))]
  }


  # get list of DQ-variables of interest
  outlist$dqa_assessment <- mdr[get("source_system")==source_db & get("dqa_assessment") == 1,][order(get("source_table_name")),c("designation",
                                                                                                                                 "source_variable_name",
                                                                                                                                 "variable_name",
                                                                                                                                 "variable_type",
                                                                                                                                 "key",
                                                                                                                                 "source_table_name"), with=F]

  # get list of dqa_vars for catgeorical and numerical analyses
  outlist$dqa_vars <- outlist$dqa_assessment[grepl("^dt\\.", get("key")),]

  # variable_list
  variable_list <- outlist$dqa_vars[order(get("designation"))]
  outlist$variable_list <- sapply(variable_list[,get("designation")], function(x){
    variable_list[get("designation")==x, get("variable_name")]
  }, simplify = F, USE.NAMES = T)

  # # get list of pl_vars for plausibility analyses
  # pl.atemp_vars <- mdr[grepl("^pl\\.atemp\\.", get("key")) & get("dqa_assessment") == 1,][order(get("source_table_name")),c("designation",
  #                                                                                                                           "source_system",
  #                                                                                                                           "source_variable_name",
  #                                                                                                                           "variable_name",
  #                                                                                                                           "variable_type",
  #                                                                                                                           "key",
  #                                                                                                                           "source_table_name"), with=F]
  # outlist$pl.atemp_vars <- sapply(unique(pl.atemp_vars[,get("designation")]), function(x){
  #   pl.atemp_vars[get("designation")==x & get("source_system")==source_db, get("key")]
  # }, simplify = F, USE.NAMES = T)
  #
  # outlist$pl.atemp_vars <- c(outlist$pl.atemp_vars,
  #                            sapply(unique(pl.atemp_vars[,get("designation")]), function(x){
  #                              pl.atemp_vars[get("designation")==x & get("source_system")==target_db, get("key")]
  #                            }, simplify = F, USE.NAMES = T))
  #
  # outlist$pl.atemp_vars_filter <- sapply(unique(pl.atemp_vars[,get("designation")]), function(x){
  #   gsub("_source|_target", "", outlist$pl.atemp_vars[unique(names(outlist$pl.atemp_vars))][[x]])
  # }, simplify = F, USE.NAMES = T)

  # get list of pl_vars for plausibility analyses
  pl_vars <- mdr[!is.na(get("plausibility_relation")),][order(get("source_table_name")),c("designation",
                                                                                      "variable_name",
                                                                                      "variable_type",
                                                                                      "plausibility_relation"), with=F]
  ap.filter <- lapply(pl_vars[,get("plausibility_relation")], function(x){names(jsonlite::fromJSON(x))}) == "atemporal"
  up.filter <- lapply(pl_vars[,get("plausibility_relation")], function(x){names(jsonlite::fromJSON(x))}) == "uniqueness"

  outlist$pl.atemp_vars <- pl_vars[ap.filter,]
  outlist$pl.uniq_vars <- pl_vars[up.filter,]


  # get variables for type-transformations
  # get categorical variables
  outlist$cat_vars <- unique(mdr[get("variable_type") == "factor", get("variable_name")])

  # get date variables
  outlist$date_vars <- outlist$dqa_vars[get("variable_type") == "date", get("variable_name")]

  # get variable names, that need to be transformed (cleaning neccessary due to i2b2-prefixes)
  # this is yet hard-coded
  outlist$trans_vars <- c("encounter_hospitalization_dischargeDisposition", "encounter_hospitalization_class",
                          "condition_code_coding_code", "procedure_code_coding_code", "encounter_hospitalization_admitSource",
                          "condition_category_encounter_diagnosis")

  return(outlist)
}
