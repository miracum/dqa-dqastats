#' @import data.table
#' @importFrom magrittr "%>%"
#
NULL

# https://stackoverflow.com/a/77323812
.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 2)
}
