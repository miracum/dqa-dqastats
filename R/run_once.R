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


#' @title Perform Data Quality Assessment of Electronic Health Records.
#'
#' @description This function performs a data quality assessment (DQA)
#'   of electronic health records (EHR) in an automatical way.
#'   IMPORTANT Prerequirements:
#'   The .env file with the settings for source and target needs to be stored/
#'   mounted in `/data/input/.env`. Name (= Prefix) of the source system:
#'   `source` and `target` for the target system.
#'
#' @inheritParams dqa
#'
#' @examples
#' \dontrun{
#' DQAstats::run_once()
#' }
#'
#' @export
run_once <- function(source_system_name = "source",
                     target_system_name = "target",
                     utils_path = system.file("application/_utilities/",
                                              package = "miRacumDQA"),
                     mdr_filename = "mdr.csv",
                     output_dir = "/data/output/",
                     logfile_dir = "/data/output/logs/",
                     parallel = TRUE,
                     ncores = 4,
                     restricting_date_start = NULL,
                     restricting_date_end = NULL) {
  DIZutils::set_env_vars(env_file = "/data/input/.env")


  all_results <- DQAstats::dqa(
    source_system_name = source_system_name,
    target_system_name = target_system_name,
    utils_path = utils_path,
    mdr_filename = mdr_filename,
    output_dir = output_dir,
    logfile_dir = logfile_dir,
    parallel = parallel,
    ncores = ncores,
    restricting_date_start = restricting_date_start,
    restricting_date_end = restricting_date_end
  )
  rm(all_results)
}
