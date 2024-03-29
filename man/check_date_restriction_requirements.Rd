% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime_restrictions.R
\name{check_date_restriction_requirements}
\alias{check_date_restriction_requirements}
\title{Checking the mdr integrity for time restrictions}
\usage{
check_date_restriction_requirements(
  mdr,
  system_names,
  logfile_dir,
  headless = TRUE,
  enable_stop = TRUE
)
}
\arguments{
\item{mdr}{The mdr as data.table}

\item{system_names}{(String) The name of the systems (source and target)
to check for possible date restriction in the mdr.}

\item{logfile_dir}{The absolute path to folder where the logfile
will be stored default(\code{tempdir()}).}

\item{headless}{(Boolean) Is this a console application? Otherwise
(if \code{headless = FALSE}) there is a GUI and there will be GUI-feedback.}

\item{enable_stop}{(Boolean, default = TRUE) If true (default) this function
will call \code{stop()} in case of the check fails. If \code{enable_stop = FALSE}
it will return \code{TRUE} if the check was successful and \code{FALSE} if the
check failed. Use \code{enable_stop = FALSE} to avoid the need of a try/catch
block around this function.}
}
\value{
TRUE/FALSE: TRUE if the check was successful and the given
systems can be time filtered, FALSE if something went wrong and no time
filtering is possible.

A boolean to indicate if the date restriction requirements are met
(TRUE) or not (FALSE).
}
\description{
Internal function to check if for every input table there is
one single (or empty) column where to apply the time restriction to.
If the input is valid, it will just print a success-message, if the
data is invalid, the function will call \code{stop()}.
}
\examples{
utils_path <- system.file(
  "demo_data/utilities/",
  package = "DQAstats"
)
mdr_filename <- "mdr_example_data.csv"
mdr <- read_mdr(
  utils_path = utils_path,
  mdr_filename = mdr_filename
)

source_system_name <- "exampleCSV_source"
target_system_name <- "exampleCSV_target"

DIZtools::cleanup_old_logfile(logfile_dir = tempdir())

check_date_restriction_requirements(
  mdr = mdr,
  system_names = c(source_system_name, target_system_name),
  logfile_dir = tempdir(),
  headless = TRUE,
  enable_stop = TRUE
)

}
