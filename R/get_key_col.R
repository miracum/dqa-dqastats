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
#
get_key_col <- function(rv) {
  # if system_type = "csv" --> our index to find
  # our table is the filename, stored in "source_table_name"
  if (rv$source$system_type == "csv") {
    key_col_name_src <- "source_table_name"

    # if system_type = "postgres" --> our index to find
    # our table is the "variable_name" (correspondingly,
    # the variable_name is used to store the sql statements)
  } else if (rv$source$system_type %in%
             c("postgres", "mysql", "fhir", "oracle")) {
    # Back to key: 'variable_name' was assigned here:
    key_col_name_src <- "key"
  }
  if (rv$target$system_type == "csv") {
    key_col_name_tar <- "source_table_name"
  } else if (rv$target$system_type %in%
             c("postgres", "mysql", "fhir", "oracle")) {
    # Back to key: 'variable_name' was assigned here:
    key_col_name_tar <- "key"
  }
  return(list(source = key_col_name_src,
              target = key_col_name_tar))
}
