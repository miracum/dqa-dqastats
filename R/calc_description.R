# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2024 Universitätsklinikum Erlangen
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

calc_description <- function(desc_dat,
                             rv) {
  if (nrow(desc_dat) > 1 ||
      rv$source$system_name == rv$target$system_name) {
    description <- list()
    description$source_data <- list(
      name = desc_dat[get("source_system_name") ==
                        rv$source$system_name, get("designation")],
      internal_variable_name = desc_dat[get("source_system_name") ==
                                          rv$source$system_name,
                                        get("variable_name")],
      description = desc_dat[get("source_system_name") ==
                               rv$source$system_name, get("definition")],
      var_name = desc_dat[get("source_system_name") ==
                            rv$source$system_name, get("source_variable_name")],
      table_name = desc_dat[get("source_system_name") ==
                              rv$source$system_name, get("source_table_name")],
      # fhir_name = desc_dat[get("source_system_name") ==
      #                        rv$source$system_name, get("fhir")],
      checks = list(
        var_type = desc_dat[get("source_system_name") ==
                              rv$source$system_name, get("variable_type")],
        constraints = desc_dat[get("source_system_name") ==
                                 rv$source$system_name, get("constraints")]
        # value_threshold = desc_dat[get("source_system_name") ==
        #                              rv$source$system_name,
        #                            get("value_threshold")],
        # missing_threshold = desc_dat[get("source_system_name") ==
        #                                rv$source$system_name,
        #                              get("missing_threshold")]
      )
    )

    description$target_data <-
      list(
        var_name = desc_dat[get("source_system_name") ==
                              rv$target$system_name,
                            get("source_variable_name")],
        table_name = desc_dat[get("source_system_name") ==
                                rv$target$system_name,
                              get("source_table_name")],
        # fhir_name = desc_dat[get("source_system_name") ==
        #                        rv$target$system_name,
        #                      get("fhir")],
        checks = list(
          var_type = desc_dat[get("source_system_name") ==
                                rv$target$system_name,
                              get("variable_type")],
          constraints = desc_dat[get("source_system_name") ==
                                   rv$target$system_name,
                                 get("constraints")]
          # value_threshold = desc_dat[get("source_system_name") ==
          #                              rv$target$system_name,
          #                            get("value_threshold")],
          # missing_threshold = desc_dat[get("source_system_name") ==
          #                                rv$target$system_name,
          #                              get("missing_threshold")]
        )
      )
    return(description)
  } else {
    return(NULL)
  }
}
