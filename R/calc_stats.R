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

calcDescription <- function(desc_dat, rv){
  if (nrow(desc_dat)>1){
    description <- list()
    description$source_data <- list(name = desc_dat[get("source_system")==rv$db_source, get("name")],
                                    description = desc_dat[get("source_system")==rv$db_source, get("description")],
                                    var_name = desc_dat[get("source_system")==rv$db_source, get("source_variable_name")],
                                    table_name = desc_dat[get("source_system")==rv$db_source, get("source_table_name")],
                                    fhir_name = desc_dat[get("source_system")==rv$db_source, get("fhir")],
                                    checks = list(var_type = desc_dat[get("source_system")==rv$db_source, get("variable_type")],
                                                  value_set = desc_dat[get("source_system")==rv$db_source, get("value_set")],
                                                  value_threshold = desc_dat[get("source_system")==rv$db_source, get("value_threshold")],
                                                  missing_threshold = desc_dat[get("source_system")==rv$db_source, get("missing_threshold")]))

    description$target_data <- list(var_name = desc_dat[get("source_system")==rv$db_target, get("source_variable_name")],
                                    table_name = desc_dat[get("source_system")==rv$db_target, get("source_table_name")],
                                    fhir_name = desc_dat[get("source_system")==rv$db_target, get("fhir")],
                                    checks = list(var_type = desc_dat[get("source_system")==rv$db_target, get("variable_type")],
                                                  value_set = desc_dat[get("source_system")==rv$db_target, get("value_set")],
                                                  value_threshold = desc_dat[get("source_system")==rv$db_target, get("value_threshold")],
                                                  missing_threshold = desc_dat[get("source_system")==rv$db_target, get("missing_threshold")]))
    return(description)
  } else {
    return(NULL)
  }
}

calcAtempPlausiDescription <- function(dat, plausis_atemporal, desc_dat, rv){
  description <- list()
  description$source_data <- list(name = dat$source_data$name,
                                  description = dat$source_data$description,
                                  var_dependent = dat$source_data$var_dependent,
                                  var_independent = dat$source_data$var_independent,
                                  filter = dat$source_data$filter,
                                  join_crit =dat$source_data$join_crit,
                                  checks = c(plausis_atemporal$source_data$checks,
                                             list(var_type = desc_dat[get("source_system")==rv$db_source, get("variable_type")]))
                                  )
  description$target_data <- list(name = dat$target_data$name,
                                  var_dependent = dat$target_data$var_dependent,
                                  var_independent = dat$target_data$var_independent,
                                  filter = dat$target_data$filter,
                                  join_crit =dat$target_data$join_crit,
                                  checks = c(plausis_atemporal$target_data$checks,
                                             list(var_type = desc_dat[get("source_system")==rv$db_source, get("variable_type")]))
                                  )

  return(description)
}

calcCounts <- function(cnt_dat, count_key, rv, datamap = TRUE){
  counts <- list()
  counts$source_data$cnt <- tryCatch({
    if (isTRUE(datamap)){
      cnt <- countUnique(rv$data_source[[cnt_dat[get("source_system")==rv$db_source, get("source_table_name")]]], count_key, sourcesystem = rv$db_source, datamap = datamap)
    } else {
      cnt <- countUnique(rv$data_source[[cnt_dat[get("source_system")==rv$db_source, get("key")]]], count_key, sourcesystem = rv$db_source, datamap = datamap)
    }
  }, error=function(e){
    cat("\nError occured when counting source_data\n")
    print(e)
    cnt <- NULL
  }, finally = function(f){
    return(cnt)
  })

  counts$source_data$type <- cnt_dat[get("source_system")==rv$db_source, get("variable_type")]


  # for target_data; our data is in rv$data_target$key
  counts$target_data$cnt <- tryCatch({
    cnt <- countUnique(rv$data_target[[cnt_dat[get("source_system")==rv$db_target, get("key")]]], count_key, sourcesystem = rv$db_target, datamap = datamap)

  }, error=function(e){
    cat("\nError occured when counting target_data\n")
    print(e)
    cnt <- NULL
  }, finally = function(f){
    return(cnt)
  })

  counts$target_data$type <- cnt_dat[get("source_system")==rv$db_target, get("variable_type")]

  return(counts)
}

calcCatStats <- function(stat_dat, stat_key, rv, plausibility = FALSE){
  statistics <- list()

  statistics$source_data <- tryCatch({
    if (isFALSE(plausibility)){
      # for source_data; our data is in rv$data_source$source_table_name
      source_data <- categoricalAnalysis(rv$data_source[[stat_dat[get("source_system")==rv$db_source, get("source_table_name")]]], stat_key)
    } else {
      source_data <- categoricalAnalysis(rv$data_source[[stat_dat[get("source_system")==rv$db_source, get("key")]]], stat_key)
    }
  }, error=function(e){
    cat("\nError occured when calculating source catStats\n")
    print(e)
    source_data <- NULL
  }, finally = function(f){
    return(source_data)
  })

  statistics$target_data <- tryCatch({
    target_data <- categoricalAnalysis(rv$data_target[[stat_dat[get("source_system")==rv$db_target, get("key")]]], stat_key)
  }, error = function(e){
    cat("\nError occured when calculating target catStats\n")
    print(e)
    target_data <- NULL
  }, finally = function(f){
    return(target_data)
  })

  return(statistics)
}

calcNumStats <- function(stat_dat, stat_key, rv, plausibility = FALSE){
  statistics <- list()

  if (stat_dat[get("source_system")==rv$db_source,get("variable_type")!="date"]){
    statistics$source_data <- tryCatch({
      if (isFALSE(plausibility)){
        # for source_data; our data is in rv$data_source$source_table_name
        source_data <- extensiveSummary(rv$data_source[[stat_dat[get("source_system")==rv$db_source, get("source_table_name")]]][, get(stat_key)])
      } else {
        source_data <- extensiveSummary(rv$data_source[[stat_dat[get("source_system")==rv$db_source, get("key")]]][, get(stat_key)])
      }
    }, error=function(e){
      cat("\nError occured when calculating simple source numStats\n")
      print(e)
      source_data <- NULL
    }, finally = function(f){
      return(source_data)
    })

    statistics$target_data <- tryCatch({
      target_data <- extensiveSummary(rv$data_target[[stat_dat[get("source_system")==rv$db_target, get("key")]]][, get(stat_key)])
    }, error=function(e){
      cat("\nError occured when calculating simple target numStats\n")
      print(e)
      target_data <- NULL
    }, finally = function(f){
      return(target_data)
    })


  } else {

    statistics$source_data <- tryCatch({
      if (isFALSE(plausibility)){
        source_data <- simpleSummary(rv$data_source[[stat_dat[get("source_system")==rv$db_source, get("source_table_name")]]][, get(stat_key)])
      } else {
        source_data <- simpleSummary(rv$data_source[[stat_dat[get("source_system")==rv$db_source, get("key")]]][, get(stat_key)])
      }
    }, error=function(e){
      cat("\nError occured when calculating simple source numStats\n")
      print(e)
      source_data <- NULL
    }, finally = function(f){
      return(source_data)
    })

    statistics$target_data <- tryCatch({
      target_data <- simpleSummary(rv$data_target[[stat_dat[get("source_system")==rv$db_target, get("key")]]][, get(stat_key)])
    }, error=function(e){
      cat("\nError occured when calculating simple target numStats\n")
      print(e)
      target_data <- NULL
    }, finally = function(f){
      return(target_data)
    })
  }

  return(statistics)
}
