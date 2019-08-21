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


#' @title atempPausiResults_ helper function
#'
#' @description Internal function to generate the results of the 'Atemporal Plausibility' checks.
#'
#' @inheritParams descriptiveResults_
#'
#' @export
#'
atempPausiResults_ <- function(rv, headless = FALSE){
  # source_db = rv$db_source
  # headless = T

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

  for (i in names(rv$data_plausibility$atemporal)){

    dat <- rv$data_plausibility$atemporal[[i]]

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Getting plausibility descriptions of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress1$inc(1/length(names(rv$data_plausibility$atemporal)), detail = paste("... working at description of", i, "..."))
    }

    # # generate descriptions
    # desc_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^pl\\.atemp\\.", get("key")),][get("designation")==i,c("designation", "source_system", "source_variable_name",
    #                                                                                              "source_table_name", "definition",
    #                                                                                              "sql_from", "sql_join_on", "sql_join_table", "sql_join_type", "sql_where",
    #                                                                                              "variable_type", "value_set", "value_threshold", "missing_threshold"), with=F]
    #
    # if (nrow(desc_dat)>1){
    #   outlist[[rv$pl.atemp_vars_filter[[i]]]]$description <- calcPlausiDescription(desc_dat, rv, sourcesystem = source_db)
    # } else {
    #   cat("\nError occured during creating descriptions of source system\n")
    # }

    # add the raw data to data_target and data_source
    desc_dat <- rv$mdr[get("variable_name")==dat$source_data$var_dependent & get("source_system") %in% c(rv$db_source, rv$db_target) & get("dqa_assessment") == 1, c("source_system", "source_variable_name", "source_table_name", "variable_type", "key", "variable_name"), with=F]
    # workaround, to get old calcCounts function working with new cnt_dat
    desc_dat[get("source_system") == rv$db_source,("key"):=paste0(i, "_source")]
    desc_dat[get("source_system") == rv$db_target,("key"):=paste0(i, "_target")]

    outlist[[i]]$description <- calcAtempPlausiDescription(dat,
                                                           plausis_atemporal = rv$data_plausibility$atemporal[[i]],
                                                           desc_dat,
                                                           rv)


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating plausibility counts of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress2$inc(1/length(names(rv$data_plausibility$atemporal)), detail = paste("... working at counts of", i, "..."))
    }

    # generate counts
    # cnt_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^pl\\.atemp\\.", get("key")),][get("designation")==i,c("source_system", "source_variable_name", "source_table_name", "variable_type", "key", "variable_name"), with=F]

    cnt_dat <- desc_dat

    if (length(cnt_dat[,unique(get("variable_name"))]) == 1){
      outlist[[i]]$counts <- calcCounts(cnt_dat = cnt_dat,
                                        count_key = cnt_dat[,unique(get("variable_name"))],
                                        rv, datamap = FALSE)
    } else {
      cat("\nError occured during creating counts\n")
    }


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating plausibility statistics of", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress3$inc(1/length(names(rv$data_plausibility$atemporal)), detail = paste("... working at statistics of", i, "..."))
    }

    # generate counts
    # stat_dat <- rv$mdr[get("dqa_assessment")==1,][grepl("^pl\\.atemp\\.", get("key")),][get("designation")==i,c("source_system", "source_variable_name", "source_table_name", "variable_name", "variable_type", "key"),with=F]
    stat_dat <- cnt_dat

    if (stat_dat[,unique(get("variable_type"))] == "factor"){
      outlist[[i]]$statistics <- calcCatStats(stat_dat, stat_dat[,unique(get("variable_name"))], rv, plausibility = TRUE)
      # for target_data; our data is in rv$list_target$key
    } else {
      outlist[[i]]$statistics <- calcNumStats(stat_dat, stat_dat[,unique(get("variable_name"))], rv, plausibility = TRUE)
    }
  }
  if (isFALSE(headless)){
    progress1$close()
    progress2$close()
    progress3$close()
  }
  return(outlist)
}



#' @title uniqPausiResults_ helper function
#'
#' @description Internal function to generate the results of the 'Uniqueness Plausibility' checks.
#'
#' @param pl.uniq_vars A data.table object. The object is created by \code{createHelperVars_} from the data represented in the metadata repository.
#'
#' @inheritParams atempPausiResults_
#' @inheritParams createHelperVars_
#'
#' @export
#'
uniqPausiResults_ <- function(rv, pl.uniq_vars, mdr, headless = FALSE){
  # pl.uniq_vars = rv$pl.uniq_vars
  # mdr = rv$mdr
  # sourcesystem = "p21csv"
  # headless = T

  outlist <- list()

  # get uniqueness checks from json
  uniques <- list()
  for (i in pl.uniq_vars[,get("variable_name")]){
    uniques[[i]] <- jsonlite::fromJSON(pl.uniq_vars[get("variable_name") == i, get("plausibility_relation")])[["uniqueness"]]
  }

  # iterate over uniqueness checks
  for (i in names(uniques)){

    if (isFALSE(headless)){
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste("Getting uniqueness plausibilities for", i), value = 0)
    }

    for (j in 1:length(uniques[[i]])){
      u <- uniques[[i]][[j]]
      u$variable_name <- names(uniques[[i]])[j]

      # workaround to hide shiny-stuff, when going headless
      msg <- paste("Getting uniqueness plausibility", u$name)
      cat("\n", msg, "\n")
      if (isFALSE(headless)){
        shinyjs::logjs(msg)
        # Increment the progress bar, and update the detail text.
        progress$inc(1/length(names(uniques[[i]])), detail = paste("... working hard ..."))
      }

      outlist[[u$name]]$description = u$description

      # get information on source data
      for (k in c("source_data", "target_data")){

        src_flag <- ifelse(k == "source_data", rv$db_source, rv$db_target)


        if (!is.null(u$filter[[src_flag]])){
          outlist[[u$name]][[k]]$filter = u$filter[[src_flag]]
        }

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
            group_data <- unique(rv[[raw_data]][[u.key]][get(u$variable_name) %in% u$filter[[src_flag]],get(u$variable_name), by = get(i)])
          } else {
            group_data <- unique(rv[[raw_data]][[u.key]][,get(u$variable_name), by = get(u$variable_name)])
          }
        } else {

          msg <- paste(i, "not in", colnames(rv[[raw_data]][[u.key]]))
          cat("\n", msg, "\n")
          if (isFALSE(headless)){
            shinyjs::logjs(msg)
          }
          # we need to find the correct data and merge
          if (k == "source_data"){
            m.key <- mdr[!grepl("^pl\\.", get("key")),][get("source_system") == rv$db_source & get("variable_name") == i & get("dqa_assessment") == 1, get("source_table_name")]
          } else {
            m.key <- mdr[!grepl("^pl\\.", get("key")),][get("source_system") == rv$db_target & get("variable_name") == i & get("dqa_assessment") == 1, get("key")]
          }

          if (!is.null(u$filter[[src_flag]])){
            m.x <- rv[[raw_data]][[u.key]][get(u$variable_name) %in% u$filter[[src_flag]],]
          } else {
            m.x <- rv[[raw_data]][[u.key]]
          }

          m.y <- rv[[raw_data]][[m.key]]

          merge_data <- merge(x = m.x,
                              y = m.y,
                              by.x = u$variable_name,
                              by.y = colnames(m.y)[grepl(u$variable_name, colnames(m.y))],
                              all = T,
                              suffixes = c("", ""))
          group_data <- unique(merge_data[,get(u$variable_name), by = get(i)])
          rm(merge_data, m.x, m.y)
          gc()
        }
        colnames(group_data) <- c(i, u$variable_name)
        get_dupl <- as.character(group_data[duplicated(get(i)),get(i)])
        rm(group_data)
        gc()

        outlist[[u$name]][[k]]$message <- ifelse(length(get_dupl) > 0,
                                                 paste0("Found ", length(get_dupl), " duplicate occurrences of ", i, " in association with ", u$variable_name, "."),
                                                 paste0("No duplicate occurrences of ", i, " found in association with ", u$variable_name, "."))
        outlist[[u$name]][[k]]$error <- ifelse(length(get_dupl) > 0,
                                               "Error", #paste0(get_dupl, collapse = ", "),
                                               as.character(FALSE))
      }
    }

    if (isFALSE(headless)){
      progress$close()
    }

  }
  return(outlist[order(names(outlist))])
}
