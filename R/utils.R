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


#' @title clean_path_name helper function
#'
#' @description Internal function to clean paths to have a tailing slash
#'
#' @param pathname A character string. A pathname to be cleaned
#'   (to have a tailing slash).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Both function calls will return "home/test/"
#' clean_path_name("home/test")
#' clean_path_name("home/test/")
#' }
#'
clean_path_name <- function(pathname) {
  return(gsub("([[:alnum:]])$", "\\1/", pathname))
}


# define %notin% function
"%!in%" <- function(x, y) {
  return(
    !("%in%"(x, y))
  )
}


# needed for markdown formating
kable_table <- function(data) {
  if (" " %in% colnames(data)) {
    return(knitr::kable(data,
                        digits = 3,
                        format = "latex",
                        col.names = NULL) %>%
             kableExtra::kable_styling(full_width = F)
    )
  } else {
    return(knitr::kable(data,
                        digits = 3,
                        format = "latex") %>%
             kableExtra::row_spec(0, bold = TRUE) %>%
             kableExtra::kable_styling(full_width = F)
    )
  }
}


# time interval
#' @title time_interval helper function
#'
#' @description Internal function to get the time interval
#'
#' @param data The list object
#'   'rv$results_descriptive$EpisodeOfCare_period_end'
#'
#' @export
#'
time_interval <- function(data) {
  outlist <- list()
  outlist$start <- substr(
    as.character(data$statistics$target_data[1, ])[2],
    1,
    4
  )
  outlist$end <- substr(
    as.character(data$statistics$target_data[6, ])[2],
    1,
    4
  )
  return(outlist)
}


#' @title get_config helper function
#'
#' @description Internal function to read config files
#'
#' @param config_file A character string. The path to the config.yml-file
#'   containing the database configuration.
#' @param config_key A character string. The name of the corresponding
#'   database. This string must be conform with the corresponding config
#'   section in the config.yml-file.
#'
#' @export
#'
get_config <- function(config_file, config_key) {
  res <- tryCatch({
    config::get(config_key, file = config_file)
  },
  error = function(cond) {
    cond <- paste(unlist(cond), collapse = " ")
    feedback(
      print_this = paste0("Cannot access config_file. ", cond),
      type = "Error",
      findme = "e3e1b9c5f9"
    )
    return(NULL)
  },
  warning = function(cond) {
    cond <- paste(unlist(cond), collapse = " ")
    feedback(
      print_this = paste0("Cannot access config_file. ", cond),
      type = "Warning",
      findme = "718e0f3d88"
    )
    return(NULL)
  })
  return(res)
}

#' @title load_sqls helper function
#'
#' @description Internal function to load the SQL statements.
#'
#' @inheritParams dqa
#' @param db A character string. The name of the corresponding database.
#'
#' @export
#'
load_sqls <- function(utils_path, db) {
  return(jsonlite::fromJSON(paste0(utils_path, "SQL/SQL_", db, ".JSON")))
}


get_where_filter <- function(filter) {
  return(jsonlite::fromJSON(filter))
}

#' @title Function to feedback messages either to the user and/or to the
#'   console and to the logfile.
#' @description This function provides the functionality to publish
#'   any kind of information to the user, the console and/or to the logfile.
#'   This might be a simple info, a warning or an error.
#'   The function can be used to select the output (console, ui, logfile).
#'   If no output is selected, the print_this string will be printed to the
#'   console and to logfile.
#'   One of these must be a string with length > 0: print_me, console, ui
#' @param print_this (Optional, String)
#' @param type (Optional, String) E.g. "Warning", "Error. Default: "Info"
#' @param ui (Optional, Boolean/String) If true, the message will
#'   also be printed to the user in form of a modal. Can also be a string.
#' @param console (Optional, Boolean/String) If true, the message will also
#'   be printed to the console as is. Can also be a string.
#' @param logfile (Optional, Boolean) If true (default) the print_this string
#'   will also be printed to the console.
#' @param logjs (Optional, Boolean) If true (default: false) the print_this
#'   string will also be printed to the javascript-console.
#'   This only makes sense, if the gui is active.
#' @param prefix Prefix (Optional, String) This is useful if
#'   print_this is an array/list.
#'   Each entry will then be new row with this prefix.
#' @param suffix Suffix (Optional, String). Same like prefix but at the
#'   end of each line.
#' @param findme (Optional, String) String to find the message in the code.
#'   E.g. 10-digit random hex from https://www.browserling.com/tools/random-hex
#'   or https://onlinerandomtools.com/generate-random-hexadecimal-numbers
#'
#' @export
#'
feedback <-
  function(print_this = "",
           type = "Info",
           ui = FALSE,
           console = TRUE,
           logfile = TRUE,
           logjs = FALSE,
           prefix = "",
           suffix = "",
           findme = "") {
    # Make the first letter of type Uppercase:
    type <- firstup(type)

    # we need parts of the rv object here so if it is not set yet
    # initialize it now:
    if (!exists("rv")) {
      rv <- c()
    }

    # If the gui is active, show the message to the user.
    # If its an error message, also show the error messages in the gui
    # even if the user did not explicitely said it should be displayed
    # in the gui
    if (isTRUE(ui) ||
        (isFALSE(rv$headless) && isTRUE(type == "Error") && isFALSE(ui))) {
      feedback_to_ui(print_this = print_this, type = type)
    }

    if (isTRUE(console) && isFALSE(print_this == "")) {
      feedback_to_console(
        print_this = print_this,
        type = type,
        findme = findme,
        prefix = prefix,
        suffix = suffix,
        logjs = logjs
      )
    }

    # If there is text defined in 'ui' and/or 'console', print this one
    # (this is uesful if one will provide both, feedback to the ui AND
    # feedback to the console but with different texts).
    # Hint: Everything printed to the console will also
    #       be printed to the logfile.
    if (isTRUE(typeof(ui) == "character")) {
      feedback_to_ui(print_this = print_this, type = type)
    }
    if (isTRUE(typeof(console) == "character")) {
      feedback_to_console(
        print_this = print_this,
        type = type,
        findme = findme,
        prefix = prefix,
        suffix = suffix,
        logjs = logjs
      )
    }
  }

#' @title Print to the console. Internal use.
#' @description  Helper function for the feedback function to print
#'   stuff to the console. Everything will also be added to the logfile.
#'   Internal use. Use the robust 'feedback' function instead.
#'
#' @inheritParams feedback
#'
feedback_to_console <-
  function(print_this,
           type,
           findme,
           prefix,
           suffix,
           logjs) {
    if (length(print_this) == 1) {
      res <-
        feedback_get_formatted_string(
          print_this = print_this,
          type = type,
          findme = findme,
          prefix = prefix,
          suffix = suffix
        )
      # To console:
      message(res)
      # To logjs:
      if (isTRUE(logjs)) {
        feedback_to_logjs(res)
      }
      # To logfile:
      feedback_to_logfile(
        print_this = print_this,
        type = type,
        findme = findme,
        prefix = prefix,
        suffix = suffix
      )
    } else if (length(print_this) > 1) {
      i <- 1
      for (tmp in print_this) {
        res <-
          feedback_get_formatted_string(
            print_this = tmp,
            type = type,
            findme = findme,
            prefix = paste0(prefix, i, ": "),
            suffix = suffix
          )
        # To console:
        message(res)
        # To logjs:
        if (isTRUE(logjs)) {
          feedback_to_logjs(res)
        }
        # To logfile:
        feedback_to_logfile(
          print_this = tmp,
          type = type,
          findme = findme,
          prefix = prefix,
          suffix = suffix
        )
        i <- i + 1
      }
    }
  }

#' @title Feedback to the user with a modal. Internal use.
#' @description  Helper function for the feedback function to show modals
#'   to the gui/user. Everything will also be added to the logfile.
#'   Internal use. Use the robust 'feedback' function instead.
#' @inheritParams feedback
#'
feedback_to_ui <- function(print_this, type) {
  catch_msg <- paste0("Something went wrong while trying",
                      " to show feedback to the UI: ")
  tryCatch({
    if (isTRUE(type == "Error")) {
      title <- "Sorry, an error has occured"
    } else {
      title <- type
    }
    shiny::showModal(
      shiny::modalDialog(
        title = title,
        easyClose = TRUE,
        print_this
        )
      )
  },
  error = function(cond) {
    feedback(print_this = paste0(catch_msg, cond),
             type = "Error",
             findme = "58eb015c10"
    )
  },
  warning = function(cond) {
    feedback(print_this = paste0(catch_msg, cond),
             type = "Warning",
             findme = "ef7fa319a5"
    )
  })
}

#' @title Feedback to the gui/browser-console with logjs. Internal use.
#' @description  Helper function for the feedback function to also show the
#'   messages to the gui/user via the browser console.
#'   Internal use. Use the robust 'feedback' function instead.
#' @inheritParams feedback
#'
feedback_to_logjs <- function(print_this) {
  catch_msg <- paste0("Something went wrong while trying",
                      " to print feedback to the browser console: ")
  tryCatch({
    shinyjs::logjs(print_this)
  },
  error = function(cond) {
    feedback(print_this = paste0(catch_msg, cond),
             type = "Error",
             findme = "2e68833975"
    )
  },
  warning = function(cond) {
    feedback(print_this = paste0(catch_msg, cond),
             type = "Warning",
             findme = "f3600cc9d2"
    )
  })
}

#' @title Add to the logfile. Internal use.
#' @description  Helper function for the feedback function to add content
#'   to the logfile. Internal use.
#'   Use the robust 'feedback' function instead.
#' @inheritParams feedback
#'
feedback_to_logfile <- function(print_this, type, findme, prefix, suffix) {
  # Get the formatted string out of the parameters which looks like
  # "[Info] System is running (1234567890)":
  res <- feedback_get_formatted_string(print_this = print_this,
                                       type = type,
                                       findme = findme,
                                       prefix = prefix,
                                       suffix = suffix)
  # Set the string for the logfile containing the current time and date
  # and a linebreak at the end:
  res <- paste0("[", Sys.time(), "] ", res, "\n")

  # Try to create the path if it does not exist:or assign tempdir
  tryCatch({
    dir.create(logfile_dir, showWarnings = F)
  },
  error = function(cond) {
    print("Can't create logfile_dir. Assigning tempdir now.")
    logfile_dir <- tempdir()
  },
  warning = function(cond) {
    print("Can't create logfile_dir. Assigning tempdir now.")
    logfile_dir <- tempdir()
  })

  path_with_file <- paste0(logfile_dir, "logfile.log")

  # Open the connection to the logfile:
  log_con <- file(path_with_file, open = "a")
  # Write to the logfile:
  cat(res, file = log_con)
  # Close the connection to logfile:
  close(log_con)
}

#' @title Format the feedback string
#' @description  Helper function for the feedback function to combine the input
#'   parameters in proper manner to ge a pretty and informative string which
#'   than can be added to the logfile and/or be displayed in the console.
#'   CAUTION: 'print_this' must be of length 1! For arrays loop through them
#'   by hand and call this function several times!
#'   Internal use. Use the robust 'feedback' function instead.
#' @inheritParams feedback
#'
feedback_get_formatted_string <-
  function(print_this, type, findme, prefix, suffix) {
    if (length(print_this) == 1) {
      if (findme == "") {
        res <- paste0("[", type, "] ", prefix, print_this, suffix)
      } else {
        res <- paste0("[", type, "] ",
                      prefix, print_this, suffix, " (", findme, ")")
      }
    } else {
      res <- paste0("Length of input 'print_this' is not == 1. ",
                    "See function description. (55a445fe57)")
    }
    return(res)
  }

#' @title Archives the current logfile and creates a new blank one.
#' @description  This function is called once at the beginning of the
#'   runtime of the tool. It checks whether there is an old logfile
#'   and renames it (if existing) to "logfile_20xx-xx-xx-xxxxxx.log".
#'   Then a new, empty, logfile "logfile.log" is created.
#' @export
#'
cleanup_old_logfile <- function() {
  path_with_file <- paste0(logfile_dir, "logfile.log")
  # Check if logfile.log is already the logfile for this session:
  if (isTRUE(file.exists(path_with_file))) {
    ## There is an old logfile, so rename the logfile.log to
    ## logfile_2020-01-01-1234h:
    filename_datetime <- format(Sys.time(), "%Y-%m-%d-%H%M%OS")
    path_with_file_datetime <-
      paste0(logfile_dir, "logfile_", filename_datetime, ".log")
    file.rename(from = path_with_file, to = path_with_file_datetime)
    ## ... and create a new logfile:
    file.create(path_with_file)
  }
}


#' @title Converts the first letter of the input string to uppercase
#' @description Converts the first letter of the input string to uppercase
#'
#' @param x A character string. E.g. "hello world" will become "Hello world".
#'
#' @export
#'
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#' @title global_env_hack
#' @description Hack variable into global env (bypasses R CMD checks).
#'
#' @param key A character string. The name of the assigned variable
#' @param val An object. The object that will be assigned to 'key'.
#' @param pos An integer. The position of the environment (default: 1).
#'
#' @seealso \url{http://adv-r.had.co.nz/Environments.html}
#'
#' @export
#'
global_env_hack <- function(key,
                            val,
                            pos = 1) {
  assign(
    key,
    val,
    envir = as.environment(pos)
  )
}


#' Stores logfile_dir to global environment
#' @param logfile_dir_tmp Path to the logfile folder
logfile_assign_to_global <- function(logfile_dir_tmp) {
  global_env_hack(
    key = "logfile_dir",
    val = logfile_dir_tmp
  )
}

#' @title Cleanup function to unset/close all open connections
#' @description This function is meant to be called at the end of a
#'   run of the app. It will close all open connections to files.
#'
#' @export
#'
close_all_connections <- function() {
  feedback("Doing application cleanup", findme = "8b224d503c")
  lapply(showConnections(), close)
  feedback("Closed all file-connections.", findme = "0c5cb72ecc")
}
