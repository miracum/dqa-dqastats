# report
renderResults <- function(results){

  # get names
  obj_names <- names(results)

  # loop over objects
  for (i in obj_names){
    desc_out <- results[[i]]$description
    count_out <- results[[i]]$counts
    stat_out <- results[[i]]$statistics

    # title of variable
    cat(paste0("\n## ", desc_out$source_data$name, "  \n"))
    # description of variable
    cat(paste0("\n", desc_out$source_data$description, "  \n"))

    # representation in the source system
    cat("\n### Representation in source data system  \n")
    renderRepresentation(desc_out, "source_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "source_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$source_data))


    # representation in the target system
    cat("\n### Representation in target data system  \n")
    renderRepresentation(desc_out, "target_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "target_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$target_data))
  }
}

renderRepresentation <- function(desc_out, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable: ", desc_out[[source]]$var_name, "  \n"))
  cat(paste0("- Table: ", desc_out[[source]]$table_name, "  \n  \n"))
}

renderCounts <- function(count_out, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable name: ", count_out[[source]]$cnt$variable, "  \n"))
  cat(paste0("\n- Variable type: ", count_out[[source]]$type, "  \n  \n"))
  cat(paste0("    + Distinct values: ", count_out[[source]]$cnt$distinct, "  \n"))
  cat(paste0("    + Valid values: ", count_out[[source]]$cnt$valids, "  \n"))
  cat(paste0("    + Missing values: ", count_out[[source]]$cnt$missings, "  \n  \n"))
}


renderPlausis <- function(results){

  # get names
  obj_names <- names(results)

  # loop over objects
  for (i in obj_names){
    desc_out <- results[[i]]$description
    count_out <- results[[i]]$counts
    stat_out <- results[[i]]$statistics

    # title of variable
    cat(paste0("\n## ", desc_out$source_data$name, "  \n"))
    # description of variable
    cat(paste0("\n", desc_out$source_data$description, "  \n"))

    # representation in the source system
    cat("\n### Representation in source data system  \n")
    renderPlausiRepresentation(desc_out, "source_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "source_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$source_data))

    # representation in the target system
    cat("\n### Representation in target data system  \n")
    renderPlausiRepresentation(desc_out, "target_data")

    # overview
    cat("\n **Overview:**  \n")
    renderCounts(count_out, "target_data")

    # statistics
    cat("\n **Results:**  \n")
    print(kableTable(stat_out$target_data))
  }
}

renderPlausiRepresentation <- function(desc_out, source){
  # source either "source_data" or "target_data"
  cat(paste0("\n- Variable: ", desc_out[[source]]$var_name, "  \n"))
  cat(paste0("- Tabele: ", desc_out[[source]]$table_name, "  \n"))
  cat(paste0("- FROM (SQL): ", desc_out[[source]]$sql_from, "  \n"))
  cat(paste0("- JOIN TABLE (SQL): ", desc_out[[source]]$sql_join_table, "  \n"))
  cat(paste0("- JOIN TYPE (SQL): ", desc_out[[source]]$sql_join_type, "  \n"))
  cat(paste0("- JOIN ON (SQL): ", desc_out[[source]]$sql_join_on, "  \n"))
  cat(paste0("- WHERE (SQL): ", desc_out[[source]]$sql_where, "  \n  \n"))
}


#' @title createMarkdown_ helper function
#'
#' @description Internal function to generate the final PDF report.
#'
#' @param outdir A character string. The directory to store the resulting PDF document. Default: \code{tempdir}.
#' @inheritParams loadSource_
#' @inheritParams DQA
#'
#' @export
#'
createMarkdown_ <- function(utils, outdir = tempdir(), headless = FALSE){
  msg <- "Creating report "
  cat("\n", msg, "\n")
  if (isFALSE(headless)){
    shinyjs::logjs(msg)

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating report", value = 0)

    # Increment the progress bar, and update the detail text.
    progress$inc(1/1, detail = "... working hard to create pdf ...")
  }

  knitr::knit(input= paste0(utils, "RMD/DQA_report.Rmd"), output=paste0(outdir, "/DQA_report.md"), encoding = "UTF-8")
  # copy header-folder to tempdir to make files available for the next command
  file.copy(paste0(utils, "RMD/_header"), outdir, recursive=TRUE)
  rmarkdown::render(input=paste0(outdir, "/DQA_report.md"), output_file = paste0(outdir, "/DQA_report.pdf"), encoding = "UTF-8")

  # delete temporary files
  do.call(file.remove, list(list.files(paste0(outdir, "_header"), full.names = TRUE)))
  unlink(paste0(outdir, "_header"), recursive = T)
}
