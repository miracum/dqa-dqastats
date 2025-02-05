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
#
# simple summary
simple_summary <- function(vector) {
  ar <- data.table::as.data.table(
    as.array(summary(vector)),
    keep.rownames = TRUE,
    na.rm = FALSE
  )
  ar[, 2] <- as.character(ar[, 2])
  colnames(ar) <- c(" ", " ")
  return(ar)
}
