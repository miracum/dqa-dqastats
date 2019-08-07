# fire SQL to database
fireSQL <- function(rv, db_con, sql, headless = FALSE){

  # for debugging:
  #print(jsonobj)

  # Errorhandling
  if (!is.null(sql)){
    # avoid sql-injection
    # https://db.rstudio.com/best-practices/run-queries-safely/
    sql <- DBI::sqlInterpolate(db_con, sql)
    outdat <- data.table::data.table(RPostgres::dbGetQuery(db_con, sql), stringsAsFactors = TRUE)
    return(outdat)
  } else {
    return(NULL)
  }
}

# load csv files
loadCSV <- function(rv, filename){

  if (tolower(filename) == "fall.csv"){
    # only import necessary columns
    select_cols <- c(ENTLASSENDER_STANDORT = "factor",
                     KH_INTERNES_KENNZEICHEN = "factor",
                     GEBURTSJAHR = "factor",
                     GEBURTSMONAT = "factor",
                     GESCHLECHT = "factor",
                     PLZ = "factor",
                     AUFNAHMEDATUM = "integer64",
                     AUFNAHMEANLASS = "factor",
                     AUFNAHMEGRUND = "factor",
                     ENTLASSUNGSDATUM = "integer64",
                     ENTLASSUNGSGRUND = "factor",
                     ALTER_IN_TAGEN_AM_AUFNAHMETAG = "integer",
                     ALTER_IN_JAHREN_AM_AUFNAHMETAG = "integer",
                     PATIENTENNUMMER = "factor",
                     BEATMUNGSSTUNDEN = "integer")
  } else if (tolower(filename) == "fab.csv"){
    select_cols <- c(KH_INTERNES_KENNZEICHEN = "factor",
                     FAB = "factor",
                     FAB_AUFNAHMEDATUM = "integer64",
                     FAB_ENTLASSUNGSDATUM = "integer64")
  } else if (tolower(filename) == "icd.csv"){
    select_cols <- c(KH_internes_Kennzeichen = "factor",
                     Diagnoseart = "factor",
                     ICD_Kode = "factor")
  } else if (tolower(filename) == "ops.csv"){
    select_cols <- c(KH_internes_Kennzeichen = "factor",
                     OPS_Kode = "factor",
                     OPS_Datum = "integer64")
  }

  outdat <- data.table::fread(paste0(rv$sourcefiledir, "/", filename), select = names(select_cols), colClasses = select_cols, header = T, na.strings = "", stringsAsFactors = TRUE)

  return(outdat)
}



#' @title loadSource_ helper function
#'
#' @description Internal function to load the source data
#'
#' @param rv A list object. Internal list simulating Shiny's 'reactive values'.
#' @param keys_to_test A vector containing the names (keys) of the variables to test.
#' @inheritParams testTargetDB_
#'
#' @export
loadSource_ <- function(rv, keys_to_test, headless = FALSE){

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Reading CSV file from directory", value = 0)
  }

  # read sourcedata
  outlist <- sapply(keys_to_test, function(i){

    msg <- paste("Reading", i, "from CSV.")
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(keys_to_test), detail = paste("... working hard to read", i, "..."))
    }

    loadCSV(rv, i)
  }, simplify = F, USE.NAMES = T)
  if (isFALSE(headless)){
    progress$close()
  }


  if (isFALSE(headless)){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Transforming source variable types", value = 0)
  }
  # datatransformation source:
  for (i in keys_to_test){

    # get column names
    col_names <- colnames(outlist[[i]])

    # check, if column name in variables of interest
    # var_names of interest:
    var_names <- rv$mdr[get("source_table_name")==i,][grepl("dt\\.", get("key")),get("source_variable_name")]

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming source variable types", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(keys_to_test), detail = paste("... transforming", i, "..."))
    }

    for (j in col_names){

      if (j %in% var_names){
        vn <- rv$mdr[get("source_table_name")==i,][grepl("dt\\.", get("key")),][get("source_variable_name")==j,get("variable_name")]
        colnames(outlist[[i]])[which(col_names==j)] <- vn

        # transform date_vars to dates
        if (vn %in% rv$date_vars){
          outlist[[i]][,(vn):=as.Date(substr(as.character(get(vn)), 1, 8), format="%Y%m%d")]
        }

        if (vn %in% rv$trans_vars){
          outlist[[i]][,(vn):=transformFactors_(vector = get(vn), transformation = vn)]
        }

        # transform cat_vars to factor
        if (vn %in% rv$cat_vars){
          outlist[[i]][,(vn):=factor(get(vn))]
        }
      }
    }
  }
  if (isFALSE(headless)){
    progress$close()
  }

  # load plausis
  # read source plausibilities after data transformation
  if (isFALSE(headless)){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Getting plausibilities", value = 0)
  }

  for (i in unique(names(rv$pl_vars))){

    if (grepl("_source", rv$pl_vars[[i]])){
      j <- rv$pl_vars[[i]]

      msg <- paste("Getting source plausibilities:", i)
      cat("\n", msg, "\n")
      # workaround to hide shiny-stuff, when going headless
      if (isFALSE(headless)){
        shinyjs::logjs(msg)
        # Increment the progress bar, and update the detail text.
        progress$inc(1/length(unique(names(rv$pl_vars))), detail = paste("... getting", j, "..."))
      }

      outlist[[j]] <- loadSourcePlausibilities(j, outlist, headless = headless)
    }
  }
  if (isFALSE(headless)){
    progress$close()
  }
  return(outlist)
}


#' @title loadTarget_ helper function
#'
#' @description Internal function to load the target data
#'
#' @inheritParams loadSource_
#'
#' @export
loadTarget_ <- function(rv, keys_to_test, headless = FALSE){

  # initialize outlist
  outlist <- list()

  if (isFALSE(headless)){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Reading data from database", value = 0)
  }

  # read target data
  outlist <- sapply(keys_to_test, function(i){

    msg <- paste("Getting", i, "from database.")
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)

      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(keys_to_test), detail = paste("... working hard to read", i, "..."))
    }

    fireSQL(rv = rv, db_con = rv$db_con_target, sql = rv$sql_target[[i]], headless = headless)
  }, simplify = F, USE.NAMES = T)

  if (isFALSE(headless)){
    progress$close()
  }

  RPostgres::dbDisconnect(rv$db_con_target)

  if (isFALSE(headless)){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Transforming target variable types", value = 0)
  }

  for (i in keys_to_test){

    # workaround to hide shiny-stuff, when going headless
    msg <- paste("Transforming target variable types", i)
    cat("\n", msg, "\n")
    if (isFALSE(headless)){
      shinyjs::logjs(msg)
      # Increment the progress bar, and update the detail text.
      progress$inc(1/length(keys_to_test), detail = paste("... transforming", i, "..."))
    }

    # get column names
    col_names <- colnames(outlist[[i]])

    # check, if column name in variables of interest
    for (j in col_names){

      if (j %in% rv$trans_vars){
        outlist[[i]][,(j):=transformFactors_(vector = get(j), transformation = j)]
      }

      # transform cat_vars to factor
      if (j %in% rv$cat_vars){
        outlist[[i]][,(j):=factor(get(j))]
      }
    }
  }
  if (isFALSE(headless)){
    progress$close()
  }
  return(outlist)
}

