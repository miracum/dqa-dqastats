# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2020 Universitätsklinikum Erlangen
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


#' @title atemp_pausi_results helper function
#'
#' @description Internal function to generate the results of the
#'   'Atemporal Plausibility' checks.
#'
#' @inheritParams descriptive_results
#'
#' @inheritParams create_helper_vars
#'
#' @param atemp_vars These are the atemporal variables
#'
#' @export
#'
atemp_pausi_results <- function(rv,
                                atemp_vars,
                                mdr,
                                headless = FALSE) {
  #% source_db = rv$source$system_name
  #% headless = T

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)) {
    # Create a Progress object
    progress1 <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress1$close())
    progress1$set(message = "Getting plausibility descriptions",
                  value = 0)

    # progress 2
    progress2 <- shiny::Progress$new()
    on.exit(progress2$close())
    progress2$set(message = "Calculating plausibility counts",
                  value = 0)

    # progress 3
    progress3 <- shiny::Progress$new()
    on.exit(progress3$close())
    progress3$set(message = "Calculating plausibility statistics",
                  value = 0)
  }

  for (i in names(atemp_vars)) {

    dat <- atemp_vars[[i]]

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Getting plausibility descriptions of", i)
    feedback(msg, logjs = isFALSE(headless), findme = "0888fa800d",
             logfile_dir = rv$log$logfile_dir,
             headless = rv$headless)
    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress1$inc(
        1 / length(names(atemp_vars)),
        detail = paste("... working at description of", i, "...")
      )
    }

    # add the raw data to data_target and data_source
    desc_dat <-
      mdr[get("variable_name") == dat$source_data$var_dependent &
            get("source_system_name") %in%
            c(rv$source$system_name,
              rv$target$system_name), ][
                get("dqa_assessment") == 1, c(
                  "source_system_name",
                  "source_variable_name",
                  "source_table_name",
                  "variable_type",
                  "key",
                  "variable_name",
                  "filter"
                ), with = F]
    # # workaround, to get old calc_counts function working with new cnt_dat
    # desc_dat[get("source_system_name") ==
    #            # Back to key: 'variable_name' was here, instead of 'key':
    #            rv$source$system_name, ("key") := paste0(i, "_source")]
    # desc_dat[get("source_system_name") ==
    #            # Back to key: 'variable_name' was here, instead of 'key':
    #            rv$target$system_name, ("key") := paste0(i, "_target")]

    outlist[[i]]$description <- calc_atemp_plausi_description(
      dat,
      plausis_atemporal = atemp_vars[[i]],
      desc_dat,
      rv
    )


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating plausibility counts of", i)
    feedback(msg, logjs = isFALSE(headless), findme = "0e918bd0fd",
             logfile_dir = rv$log$logfile_dir,
             headless = rv$headless)

    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress2$inc(
        1 / length(names(atemp_vars)),
        detail = paste("... working at counts of", i, "...")
      )
    }

    cnt_dat <- desc_dat

    if (length(cnt_dat[, unique(get("variable_name"))]) == 1) {
      outlist[[i]]$counts <- calc_counts(
        cnt_dat = cnt_dat,
        count_key = cnt_dat[, unique(get("variable_name"))],
        rv,
        datamap = FALSE,
        plausibility = TRUE,
        plausibility_key = i
      )
    } else {
      msg <- ("Error occured during creating counts.")
      feedback(msg,
               logjs = isFALSE(headless),
               type = "Error",
               findme = "c57cb255fe",
               logfile_dir = rv$log$logfile_dir,
               headless = rv$headless)
      stop("", msg, "\n")
    }


    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Calculating plausibility statistics of", i)
    feedback(msg, logjs = isFALSE(headless), findme = "cf90f11533",
             logfile_dir = rv$log$logfile_dir,
             headless = rv$headless)
    if (isFALSE(headless)) {
      # Increment the progress bar, and update the detail text.
      progress3$inc(
        1 / length(names(atemp_vars)),
        detail = paste("... working at statistics of", i, "...")
      )
    }

    # generate counts
    stat_dat <- cnt_dat

    if (stat_dat[, unique(get("variable_type"))] %in%
        c("permittedValues", "string")) {
      outlist[[i]]$statistics <- calc_cat_stats(
        stat_dat,
        stat_dat[, unique(get("variable_name"))],
        rv,
        plausibility = TRUE,
        plausibility_key = i
      )
      # for target_data; our data is in rv$list_target$key
    } else {
      outlist[[i]]$statistics <- calc_num_stats(
        stat_dat,
        stat_dat[, unique(get("variable_name"))],
        rv,
        plausibility = TRUE,
        plausibility_key = i
      )
    }
  }
  if (isFALSE(headless)) {
    progress1$close()
    progress2$close()
    progress3$close()
  }
  return(outlist)
}



#' @title uniq_plausi_results helper function
#'
#' @description Internal function to generate the results of
#'   the 'Uniqueness Plausibility' checks.
#'
#' @param uniq_vars A data.table object. The object is created
#'   by \code{create_helper_vars} from the data represented in
#'   the metadata repository.
#'
#' @inheritParams atemp_pausi_results
#' @inheritParams create_helper_vars
#'
#' @export
#'
uniq_plausi_results <- function(rv,
                                uniq_vars,
                                mdr,
                                headless = FALSE) {

  #% uniq_vars = rv$pl$uniq_vars
  #% mdr = mdr
  #% sourcesystem = "p21csv"
  #% headless = T

  outlist <- list()

  # get uniqueness checks from json
  uniques <- list()
  for (i in uniq_vars[, get("variable_name")]) {
    uniques[[i]] <- jsonlite::fromJSON(
      uniq_vars[get("variable_name") ==
                  i, get("plausibility_relation")]
    )[["uniqueness"]]
  }

  # iterate over uniqueness checks
  for (i in names(uniques)) {
    if (isFALSE(headless)) {
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even
      # if there's an error
      on.exit(progress$close())
      progress$set(
        message = paste("Getting uniqueness plausibilities for", i),
        value = 0
      )
    }

    for (j in seq_len(length(uniques[[i]]))) {

      u <- uniques[[i]][[j]]
      u$variable_name <- names(uniques[[i]])[j]

      # workaround to hide shiny-stuff, when going headless
      msg <- paste("Getting uniqueness plausibility", u$name)
      feedback(msg, logjs = isFALSE(headless), findme = "e4fe49cd9e",
               logfile_dir = rv$log$logfile_dir,
               headless = rv$headless)
      if (isFALSE(headless)) {
        # Increment the progress bar, and update the detail text.
        progress$inc(
          1 / length(names(uniques[[i]])),
          detail = paste("... working hard ...")
        )
      }

      outlist[[u$name]]$description <- u$description

      key_cols <- get_key_col(rv)
      key_col_name_src <- key_cols$source
      key_col_name_tar <- key_cols$target

      # get information on source data
      for (k in c("source_data", "target_data")) {
        src_flag <- ifelse(
          k == "source_data",
          rv$source$system_name,
          rv$target$system_name
        )


        if (!is.null(u$filter[[src_flag]])) {
          outlist[[u$name]][[k]]$filter <- u$filter[[src_flag]]
        }
        if (!is.null(u$all_observations)) {
          outlist[[u$name]][[k]]$all_observations <- u$all_observations
        }

        # TODO this is yet tailored to §21
        if (k == "source_data") {
          u_key <-
            mdr[get("source_system_name") == rv$source$system_name &
                  get("variable_name") == u$variable_name &
                  get("dqa_assessment") == 1, get(key_col_name_src)]
          raw_data <- "data_source"
        } else {
          u_key <-
            mdr[get("source_system_name") == rv$target$system_name &
                  get("variable_name") == u$variable_name &
                  # Back to key: 'variable_name' was here, instead of 'key':
                  get("dqa_assessment") == 1, get(key_col_name_tar)]
          raw_data <- "data_target"
        }

        if (i %in% colnames(rv[[raw_data]][[u_key]])) {

          if (is.null(u$all_observations) || u$all_observations == "0") {

            if (!is.null(u$filter[[src_flag]])) {
              group_data <- unique(
                rv[[raw_data]][[u_key]][get(u$variable_name) %in%
                                          u$filter[[src_flag]], get(
                                            u$variable_name
                                          ), by = get(i)]
              )
            } else {
              group_data <- unique(
                rv[[raw_data]][[u_key]][
                  , get(u$variable_name), by = get(i)]
              )
            }
          } else if (u$all_observations == "1") {

            if (!is.null(u$filter[[src_flag]])) {
              group_data <- rv[[raw_data]][[u_key]][get(u$variable_name) %in%
                                          u$filter[[src_flag]], get(
                                            u$variable_name
                                          ), by = get(i)]
            } else {
              group_data <- rv[[raw_data]][[u_key]][
                  , get(u$variable_name), by = get(i)]
            }
          } else {
            msg <- paste(
              "Error: wrong character in u$all_observations.",
              collapse = "\n"
            )
            feedback(msg, logjs = isFALSE(headless), findme = "39a123470b",
                     logfile_dir = rv$log$logfile_dir,
                     headless = rv$headless)
            next
          }

        } else {

          msg <- paste(
            paste(i, "not in", colnames(rv[[raw_data]][[u_key]])),
            collapse = "\n"
          )
          feedback(msg, logjs = isFALSE(headless), findme = "39a4eeb70b",
                   logfile_dir = rv$log$logfile_dir,
                   headless = rv$headless)

          # we need to find the correct data and merge
          if (k == "source_data") {
            m_key <-
              mdr[!grepl("^pl\\.", get("variable_name")), ][
                get("source_system_name") == rv$source$system_name &
                  get("variable_name") == i &
                  get("dqa_assessment") == 1, get(key_col_name_src)
                ]

          } else {
            m_key <-
              mdr[!grepl("^pl\\.", get("variable_name")), ][
                get("source_system_name") == rv$target$system_name &
                  get("variable_name") == i &
                  # Back to key: 'variable_name' not 'key' was assigned here:
                  get("dqa_assessment") == 1, get(key_col_name_tar)
                ]
          }

          if (!is.null(u$filter[[src_flag]])) {
            m_x <-
              rv[[raw_data]][[u_key]][
                get(u$variable_name) %in% u$filter[[src_flag]],
                ]
          } else {
            m_x <- rv[[raw_data]][[u_key]]
          }

          # look, if join_crit is already in our target table, if so,
          # create m_y directly
          if (any(grepl(i, colnames(rv[[raw_data]][[m_key]])))) {
            msg <- paste("--> found", i, "in", m_key)
            feedback(msg, logjs = isFALSE(headless), findme = "39a4e4b70b",
                     logfile_dir = rv$log$logfile_dir,
                     headless = rv$headless)
            m_y <- rv[[raw_data]][[m_key]]
          }  else {
            # else join another table
            if (k == "source_data") {
              j_key <-
                mdr[!grepl("^pl\\.", get("variable_name")), ][
                  get("source_system_name") == rv$source$system_name &
                      get("variable_name") == i &
                      get("dqa_assessment") == 1, get(key_col_name_src)]
            } else {
              j_key <-
                mdr[!grepl("^pl\\.", get("variable_name")), ][
                  get("source_system_name") == rv$target$system_name &
                      get("variable_name") == i &
                      # Back to key: 'variable_name' was assigned here:
                      get("dqa_assessment") == 1, get(key_col_name_tar)]
            }
            msg <- paste("--> found", i, "in", j_key)
            feedback(msg, logjs = isFALSE(headless), findme = "39a4e6b70b",
                     logfile_dir = rv$log$logfile_dir,
                     headless = rv$headless)

            # get colnames
            coln_x <- colnames(rv[[raw_data]][[m_key]])
            coln_y <- colnames(rv[[raw_data]][[j_key]])

            # find matching colname
            coln_x <- coln_x[lapply(coln_x, function(i) {
              any(grepl(i, coln_y))
            }) == TRUE]
            coln_y <- coln_y[grepl(coln_x, coln_y)]

            m_y <- data.table::merge.data.table(
              x = rv[[raw_data]][[m_key]],
              y = rv[[raw_data]][[j_key]],
              by.x = coln_x,
              by.y = coln_y,
              all = T,
              suffixes = c("", ""),
              allow.cartesian = T
            )
          }

          merge_data <- data.table::merge.data.table(
            x = m_x,
            y = m_y,
            by.x = u$variable_name,
            by.y = colnames(m_y)[grepl(u$variable_name, colnames(m_y))],
            all.x = T,
            suffixes = c("", ""),
            allow.cartesian = T
          )

          if (is.null(u$all_observations) || u$all_observations == "0") {
            group_data <- unique(
              merge_data[, get(u$variable_name), by = get(i)]
            )
          } else if (u$all_observations == "1") {
            group_data <- merge_data[, get(u$variable_name), by = get(i)]
          } else {
            msg <- paste(
              "Error: wrong character in u$all_observations.",
              collapse = "\n"
            )
            feedback(msg, logjs = isFALSE(headless), findme = "39a456770b",
                     logfile_dir = rv$log$logfile_dir,
                     headless = rv$headless)
            next
          }
          rm(merge_data, m_x, m_y)
          gc()
        }

        colnames(group_data) <- c(i, u$variable_name)
        get_dupl <- unique(group_data[duplicated(get(i)), i, with = F])

        rm(group_data)
        gc()

        outlist[[u$name]][[k]]$message <-
          ifelse(
            nrow(get_dupl) > 0,
            paste0(
              "Found ",
              nrow(get_dupl),
              " duplicate occurrences of ",
              i,
              " in association with ",
              u$variable_name,
              "."
            ),
            paste0(
              "No duplicate occurrences of ",
              i,
              " found in association with ",
              u$variable_name,
              "."
            )
          )
        outlist[[u$name]][[k]]$error <- ifelse(
          nrow(get_dupl) > 0,
          "Error", #% paste0(get_dupl, collapse = ", "),
          as.character(FALSE)
        )

        if (nrow(get_dupl) > 0) {
          outlist[[u$name]][[k]]$affected_ids <- get_dupl
        }
      }
    }

    if (isFALSE(headless)) {
      progress$close()
    }

  }
  return(outlist[order(names(outlist))])
}
