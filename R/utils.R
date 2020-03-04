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
  return(
    config::get(config_key, file = config_file)
  )
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
        suffix = suffix
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
        suffix = suffix
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
feedback_to_console <- function(print_this, type, findme, prefix, suffix) {
  if (length(print_this) == 1) {
    res <-
      feedback_get_formatted_string(
        print_this = print_this,
        type = type,
        findme = findme,
        prefix = prefix,
        suffix = suffix
      )
    message(res)
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
      message(res)
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
  if (isTRUE(type == "Error")) {
    title <- "Sorry, an error has occured"
  } else {
    title <- type
  }
  shiny::showModal(modalDialog(title = title,
                               easyClose = TRUE,
                               print_this))
}

#' @title Add to the logfile. Internal use.
#' @description  Helper function for the feedback function to add content
#'   to the logfile. Internal use.
#'   Use the robust 'feedback' function instead.
#' @param input The input string to be added to the logfile.
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

  # Check if there is a logfile path set. If not, set tempdir() as logfile_dir:
  if (exists("logfile_dir") &&
      !is.na(logfile_dir) && nchar(logfile_dir) > 1) {
    # Check if last character of the path is a slash and add one if not:
    logfile_dir <- clean_path_name(logfile_dir)
  } else {
    print("Cannot determine logfile_dir. (912df0da89)")
    assign("logfile_dir", clean_path_name(tempdir()), envir = .GlobalEnv)
  }



  path_with_file <- paste0(logfile_dir, "logfile.log")

  # Check if logfile.log is already the logfile for this session:
  if (isTRUE(check_file_current_runtime_id(path_with_file = path_with_file))) {
    # There is a logfile for the current runtime id,
    # so append the existing logfile:
    # Open the connection to the logfile:
    log_con <- file(path_with_file, open = "a")
    # Write to the logfile:
    cat(res, file = log_con)
    # Close the connection to logfile:
    close(log_con)
  } else {
    # There is no logfile for the current runtime id,
    # so rename the logfile.log to logfile_2020-01-01-1234h and
    # create a new logfile and write the current runtime id to it:
    filename_datetime <- format(Sys.time(), "%Y-%m-%d-%H%M%OS")
    path_with_file_datetime <-
      paste0(logfile_dir, "logfile_", filename_datetime, ".log")
    file.rename(from = path_with_file, to = path_with_file_datetime)
    # ... create a new logfile.log and paste the current runtime_id here:
    if (!file.exists(path_with_file)) {
      # Open the connection to the logfile:
      log_con <- file(path_with_file, open = "a")
      # Write current runtime_id to the logfile:
      runtime_id <- paste0("runtime_id=", get_runtime_id(), "\n\n")
      cat(runtime_id, file = log_con)
      # Write current message to the logfile:
      cat(res, file = log_con)
      # Close the connection to logfile:
      close(log_con)
    }
  }
}

#' @title Returns the current runtime_id and stores it to rv$runtime_id
#' @description  Helper function for the feedback function, especially
#'   the logfile function. If there is already a runtime_id, the current
#'   one will be returned. Otherwise a new one will be set,
#'   stored to rv$runtime_id and also be returned.
#' @param force If true, a new runtime_id will be created.
#'   If false (default) it depends wether there already is one or not.
#'
get_runtime_id <- function(force = FALSE) {
  # Length of the new runtime_id:
  runtime_id_length <- 20

  # Determine wether we run in console mode (headless) only
  # or if there is a gui. Depending on this the "global env hack" (gui)
  # or the rv object (console) is available:
  if (exists("rv") && isTRUE(rv$headless == T)) {
    ## Console only - no GUI
    if (isTRUE(exists("rv$runtime_id") &&
               !is.na(rv$runtime_id)) &&
        nchar(rv$runtime_id) == runtime_id_length && isFALSE(force)) {
      # There is already a runtime_id so return it:
      return(rv$runtime_id)
    } else {
      print("Getting a new runtime_id... (ddb97805b0)")
      rv$runtime_id <- get_new_runtime_id(runtime_id_length)
      return(rv$runtime_id)
    }
  } else {
    ## Console AND GUI
    if (isTRUE(exists("runtime_id") &&
               !is.na(runtime_id)) &&
        nchar(runtime_id) == runtime_id_length && isFALSE(force)) {
      # There is already a runtime_id so return it:
      return(runtime_id)
    } else {
      print("Getting a new runtime_id... (845836d17a)")
      runtime_id <- get_new_runtime_id(runtime_id_length)
      return(runtime_id)
    }
  }

}

#' @title Creates a new random runtime_id with certain length
#' @description  Helper function for the feedback function, especially
#'   the logfile function. Creates a new random runtime_id
#'   with the given length.
#' @param runtime_id_length The length of the resulting runtime_id
#'
get_new_runtime_id <- function(runtime_id_length){
  # Random hex-number:
  res <-
    paste0(sample(c(0:9, LETTERS[1:6]), runtime_id_length, T), collapse = "")
  return(res)
}

#' @title Is current runtime_id the one in this file?
#' @description  Helper function for the feedback function, especially
#'   the logfile function. Extracts the runtime_id from the
#'   logfile and compares it to the current runtime_id.
#'   If equal, return = TRUE.
#' @param path_with_file The path with the file to look at
#'
check_file_current_runtime_id <- function(path_with_file) {
  tryCatch({
    con <- file(path_with_file, "r")
    first_line <- readLines(con, n = 1)
    runtime_id_tmp <- gsub("([runtime_id\\=])", "", first_line)
    if (isTRUE(runtime_id_tmp == runtime_id)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    close(con)
  },
  error = function(cond) {
    return(FALSE)
  },
  warning = function(cond) {
    return(FALSE)
  }, finally = close(con))
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


#' @title Converts the first letter of the input string to uppercase
#' @description Converts the first letter of the input string to uppercase
#'
#' @param x A character string. E.g. "hello world" will become "Hello world".
#'
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}
