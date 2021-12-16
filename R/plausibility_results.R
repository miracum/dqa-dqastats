# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2021 Universitätsklinikum Erlangen
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
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' rv <- list()
#' rv$mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename <- mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' rv <- c(rv, create_helper_vars(
#'   mdr = rv$mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' ))
#' # save source/target vars
#' rv$source$system_name <- source_system_name
#' rv$target$system_name <- target_system_name
#' rv$source$system_type <- "csv"
#' rv$target$system_type <- "csv"
#'
#' rv$log$logfile_dir <- tempdir()
#'
#' # set headless (without GUI, progressbars, etc.)
#' rv$headless <- TRUE
#'
#' # set configs
#' demo_files <- system.file("demo_data", package = "DQAstats")
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)
#'
#' # get configs
#' rv$source$settings <- DIZutils::get_config_env(
#'   system_name = rv$source$system_name,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#' rv$target$settings <- DIZutils::get_config_env(
#'   system_name = tolower(rv$target$system_name),
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#' # set start_time (e.g. when clicking the 'Load Data'-button in shiny
#' rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
#'
#' # define restricting date
#' rv$restricting_date$use_it <- FALSE
#'
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' rv$data_plausibility$atemporal <- get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # add the plausibility raw data to data_target and data_source
#' for (i in names(rv$data_plausibility$atemporal)) {
#'   for (k in c("source_data", "target_data")) {
#'     w <- gsub("_data", "", k)
#'     raw_data <- paste0("data_", w)
#'     rv[[raw_data]][[i]] <-
#'       rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
#'     rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
#'   }
#'   gc()
#' }
#'
#' # calculate descriptive results
#' rv$results_descriptive <- descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # calculate atemporal plausibilites
#' atemp_pausi_results(
#'   rv = rv,
#'   atemp_vars = rv$data_plausibility$atemporal,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' @export
#'
atemp_pausi_results <- function(rv,
                                atemp_vars,
                                mdr,
                                headless = FALSE) {
  #% source_db = rv$source$system_name
  #% headless = T

  outlist <- future.apply::future_sapply(
    X = names(atemp_vars),
    FUN = function(i) {
      local({

        # initialize outlist
        outlist <- list()

        dat <- atemp_vars[[i]]

        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Getting plausibility descriptions of", i)
        DIZutils::feedback(
          msg,
          findme = "0888fa800d",
          logfile_dir = rv$log$logfile_dir,
        )

        # add the raw data to data_target and data_source
        desc_dat <-
          mdr[
            get("variable_name") == dat$source_data$var_dependent &
              get("source_system_name") %in%
              c(rv$source$system_name,
                rv$target$system_name),
          ][
            get("dqa_assessment") == 1, c(
              "source_system_name",
              "source_variable_name",
              "source_table_name",
              "variable_type",
              "key",
              "variable_name",
              "filter"
            ), with = FALSE
          ]

        outlist$description <- calc_atemp_plausi_description(
          dat,
          plausis_atemporal = dat,
          desc_dat,
          rv
        )


        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Calculating plausibility counts of", i)
        DIZutils::feedback(
          msg,
          findme = "0e918bd0fd",
          logfile_dir = rv$log$logfile_dir
        )

        cnt_dat <- desc_dat

        if (length(cnt_dat[, unique(get("variable_name"))]) == 1) {
          outlist$counts <- calc_counts(
            cnt_dat = cnt_dat,
            count_key = cnt_dat[, unique(get("variable_name"))],
            rv,
            datamap = FALSE,
            plausibility = TRUE,
            plausibility_key = i
          )
        } else {
          msg <- ("Error occured during creating counts.")
          DIZutils::feedback(
            msg,
            type = "Error",
            findme = "c57cb255fe",
            logfile_dir = rv$log$logfile_dir
          )
          stop("", msg, "\n")
        }


        # workaround to hide shiny-stuff, when going headless
        msg <- paste("Calculating plausibility statistics of", i)
        DIZutils::feedback(
          msg,
          findme = "cf90f11533",
          logfile_dir = rv$log$logfile_dir
        )

        # generate counts
        stat_dat <- cnt_dat

        if (stat_dat[, unique(get("variable_type"))] %in%
            c("enumerated", "string")) {
          outlist$statistics <- calc_cat_stats(
            stat_dat = stat_dat,
            stat_key = stat_dat[, unique(get("variable_name"))],
            rv,
            plausibility = TRUE,
            plausibility_key = i
          )
          # for target_data; our data is in rv$list_target$key
        } else {
          outlist$statistics <- calc_num_stats(
            stat_dat,
            stat_dat[, unique(get("variable_name"))],
            rv,
            plausibility = TRUE,
            plausibility_key = i
          )
        }
        return(outlist)
      })
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  gc()
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
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' rv <- list()
#' rv$mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename <- mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' rv <- c(rv, create_helper_vars(
#'   mdr = rv$mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' ))
#' # save source/target vars
#' rv$source$system_name <- source_system_name
#' rv$target$system_name <- target_system_name
#' rv$source$system_type <- "csv"
#' rv$target$system_type <- "csv"
#'
#' rv$log$logfile_dir <- tempdir()
#'
#' # set headless (without GUI, progressbars, etc.)
#' rv$headless <- TRUE
#'
#' # set configs
#' demo_files <- system.file("demo_data", package = "DQAstats")
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)
#'
#' # get configs
#' rv$source$settings <- DIZutils::get_config_env(
#'   system_name = rv$source$system_name,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#' rv$target$settings <- DIZutils::get_config_env(
#'   system_name = tolower(rv$target$system_name),
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#' # set start_time (e.g. when clicking the 'Load Data'-button in shiny
#' rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
#'
#' # define restricting date
#' rv$restricting_date$use_it <- FALSE
#'
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' rv$data_plausibility$atemporal <- get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' # add the plausibility raw data to data_target and data_source
#' for (i in names(rv$data_plausibility$atemporal)) {
#'   for (k in c("source_data", "target_data")) {
#'     w <- gsub("_data", "", k)
#'     raw_data <- paste0("data_", w)
#'     rv[[raw_data]][[i]] <-
#'       rv$data_plausibility$atemporal[[i]][[k]][[raw_data]]
#'     rv$data_plausibility$atemporal[[i]][[k]][[raw_data]] <- NULL
#'   }
#'   gc()
#' }
#'
#' # calculate descriptive results
#' rv$results_descriptive <- descriptive_results(
#'   rv = rv,
#'   headless = rv$headless
#' )
#'
#' # calculate unique plausibilites
#' uniq_plausi_results(
#'   rv = rv,
#'   uniq_vars = rv$pl$uniq_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' @export
#'
uniq_plausi_results <- function(rv,
                                uniq_vars,
                                mdr,
                                headless = FALSE) {


  # initialize outlist (outer)
  outlist <- list()

  #% uniq_vars = rv$pl$uniq_vars
  #% mdr = mdr
  #% sourcesystem = "p21csv"
  #% headless = T

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

    seq_names <- sapply(
      X = uniques[[i]],
      FUN = function(x) {
        x$name
      },
      simplify = TRUE
    )

    outlist_append <- future.apply::future_sapply(
      X = seq_names,
      FUN = function(j) {
        local({

          # initialize inner_outlist
          outlist <- list()

          # set list_index
          list_index <- grep(j, uniques[[i]])

          u <- uniques[[i]][[list_index]]
          u$variable_name <- names(uniques[[i]])[list_index]

          # workaround to hide shiny-stuff, when going headless
          msg <- paste("Getting uniqueness plausibility", u$name)
          DIZutils::feedback(
            msg,
            findme = "e4fe49cd9e",
            logfile_dir = rv$log$logfile_dir
            )

          outlist$description <- u$description

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
              outlist[[k]]$filter <- u$filter[[src_flag]]
            }
            if (!is.null(u$all_observations)) {
              outlist[[k]]$all_observations <- u$all_observations
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
                  group_data <- rv[[raw_data]][[u_key]][
                    get(u$variable_name) %in%
                      u$filter[[src_flag]],
                    get(
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
                DIZutils::feedback(
                  msg,
                  findme = "39a123470b",
                  logfile_dir = rv$log$logfile_dir
                )
                return()
              }

            } else {

              msg <- paste(
                paste(i, "not in", colnames(rv[[raw_data]][[u_key]])),
                collapse = "\n"
              )
              DIZutils::feedback(
                msg,
                findme = "39a4eeb70b",
                logfile_dir = rv$log$logfile_dir
                )

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
                      # Back to key: 'variable_name' not 'key'
                      # was assigned here:
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
                DIZutils::feedback(
                  msg,
                  findme = "39a4e4b70b",
                  logfile_dir = rv$log$logfile_dir
                )
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
                DIZutils::feedback(
                  msg,
                  findme = "39a4e6b70b",
                  logfile_dir = rv$log$logfile_dir
                )

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
                  all = TRUE,
                  suffixes = c("", ""),
                  allow.cartesian = TRUE
                )
              }

              merge_data <- data.table::merge.data.table(
                x = m_x,
                y = m_y,
                by.x = u$variable_name,
                by.y = colnames(m_y)[grepl(u$variable_name, colnames(m_y))],
                all.x = TRUE,
                suffixes = c("", ""),
                allow.cartesian = TRUE
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
                DIZutils::feedback(
                  msg,
                  findme = "39a456770b",
                  logfile_dir = rv$log$logfile_dir
                )
                return()
              }
              rm(merge_data, m_x, m_y)
              gc()
            }

            colnames(group_data) <- c(i, u$variable_name)
            get_dupl <- unique(group_data[duplicated(get(i)), i, with = FALSE])

            rm(group_data)
            gc()

            outlist[[k]]$message <-
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
            outlist[[k]]$error <- ifelse(
              nrow(get_dupl) > 0,
              "Error", #% paste0(get_dupl, collapse = ", "),
              as.character(FALSE)
            )

            if (nrow(get_dupl) > 0) {
              outlist[[k]]$affected_ids <- get_dupl
            }
          }
          return(outlist)
        })
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    gc()

    outlist <- c(outlist, outlist_append)
  }
  return(outlist[order(names(outlist))])
}
