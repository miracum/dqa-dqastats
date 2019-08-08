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
loadSourcePlausibilities <- function(plausi, data_source, headless=FALSE){

  if (plausi == "pl.atemp.item01_source"){
    # item02
    outlist <- merge(data_source[["ICD.CSV"]][grepl("O[0-9]", get("condition_code_coding_code")),c("condition_encounter_identifier_value", "condition_code_coding_code"), with=F],
                     data_source[["FALL.CSV"]][,c("encounter_identifier_value", "patient_gender", "patient_identifier_value"), with=F],
                     by.x = "condition_encounter_identifier_value",
                     by.y = "encounter_identifier_value",
                     all.x = TRUE)


  } else if (plausi == "pl.atemp.item02_source"){

    # item03
    outlist <- merge(data_source[["ICD.CSV"]][grepl("C5[1-8]", get("condition_code_coding_code")),c("condition_encounter_identifier_value", "condition_code_coding_code"), with=F],
                     data_source[["FALL.CSV"]][,c("encounter_identifier_value", "patient_gender", "patient_identifier_value"), with=F],
                     by.x = "condition_encounter_identifier_value",
                     by.y = "encounter_identifier_value",
                     all.x = TRUE)

  } else if (plausi == "pl.atemp.item03_source"){
    # item04
    outlist <- merge(data_source[["ICD.CSV"]][grepl("C6[0-3]", get("condition_code_coding_code")),c("condition_encounter_identifier_value", "condition_code_coding_code"), with=F],
                     data_source[["FALL.CSV"]][,c("encounter_identifier_value", "patient_gender", "patient_identifier_value"), with=F],
                     by.x = "condition_encounter_identifier_value",
                     by.y = "encounter_identifier_value",
                     all.x = TRUE)

  } else if (plausi == "pl.atemp.item04_source"){
    # item05
    outlist <- data_source[["FALL.CSV"]][grepl("05xx", get("encounter_hospitalization_class")),c("encounter_identifier_value",
                                                                           "patient_gender",
                                                                           "patient_identifier_value",
                                                                           "encounter_hospitalization_class"), with=F]
  }
  return(outlist)
}


calcPlausiDescription <- function(desc_dat, rv, sourcesystem){
  if (nrow(desc_dat)>1){
    description <- list()
    description$source_data <- list(name = desc_dat[get("source_system")==sourcesystem, get("name")],
                                    description = desc_dat[get("source_system")==sourcesystem, get("description")],
                                    var_name = desc_dat[get("source_system")==sourcesystem, get("source_variable_name")],
                                    table_name = desc_dat[get("source_system")==sourcesystem, get("source_table_name")],
                                    sql_from = desc_dat[get("source_system")==sourcesystem, get("sql_from")],
                                    sql_join_table = desc_dat[get("source_system")==sourcesystem, get("sql_join_table")],
                                    sql_join_type = desc_dat[get("source_system")==sourcesystem, get("sql_join_type")],
                                    sql_join_on = desc_dat[get("source_system")==sourcesystem, get("sql_join_on")],
                                    sql_where = desc_dat[get("source_system")==sourcesystem, get("sql_where")],
                                    checks = list(var_type = desc_dat[get("source_system")==sourcesystem, get("variable_type")],
                                                  value_set = desc_dat[get("source_system")==sourcesystem, get("value_set")],
                                                  value_threshold = desc_dat[get("source_system")==sourcesystem, get("value_threshold")],
                                                  missing_threshold = desc_dat[get("source_system")==sourcesystem, get("missing_threshold")]))

    description$target_data <- list(var_name = desc_dat[get("source_system")==rv$db_target, get("source_variable_name")],
                                    table_name = desc_dat[get("source_system")==rv$db_target, get("source_table_name")],
                                    sql_from = desc_dat[get("source_system")==rv$db_target, get("sql_from")],
                                    sql_join_table = desc_dat[get("source_system")==rv$db_target, get("sql_join_table")],
                                    sql_join_type = desc_dat[get("source_system")==rv$db_target, get("sql_join_type")],
                                    sql_join_on = desc_dat[get("source_system")==rv$db_target, get("sql_join_on")],
                                    sql_where = desc_dat[get("source_system")==rv$db_target, get("sql_where")],
                                    checks = list(var_type = desc_dat[get("source_system")==rv$db_target, get("variable_type")],
                                                  value_set = desc_dat[get("source_system")==rv$db_target, get("value_set")],
                                                  value_threshold = desc_dat[get("source_system")==rv$db_target, get("value_threshold")],
                                                  missing_threshold = desc_dat[get("source_system")==rv$db_target, get("missing_threshold")]))
    return(description)
  } else {
    return(NULL)
  }
}
