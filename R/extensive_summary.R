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

# extensive summary
extensive_summary <- function(vector) {
  quant <- stats::quantile(vector,
                           probs = c(.25, .75),
                           na.rm = TRUE,
                           names = FALSE)
  i_out <- stats::IQR(vector, na.rm = TRUE) * 1.5

  ret <- data.table::data.table(rbind(
    c("Mean", round(base::mean(vector, na.rm = TRUE), 2)),
    c("Minimum", round(base::min(vector, na.rm = TRUE), 2)),
    c("Median", round(stats::median(vector, na.rm = TRUE), 2)),
    c("Maximum", round(base::max(vector, na.rm = TRUE), 2)),
    c("SD", round(stats::sd(vector, na.rm = TRUE), 2)),
    c("Negativ", round(as.numeric(
      base::sum(vector < 0, na.rm = TRUE)
    ), 2)),
    c("Zero", round(as.numeric(
      base::sum(vector == 0, na.rm = TRUE)
    ), 2)),
    c("Positive", round(as.numeric(
      base::sum(vector > 0, na.rm = TRUE)
    ), 2)),
    c("OutLo", round(as.numeric(
      base::sum(vector < (quant[1] - i_out), na.rm = TRUE)
    ), 2)),
    c("OutHi", round(as.numeric(
      base::sum(vector > (quant[2] + i_out), na.rm = TRUE)
    ), 2)),
    c("Variance", round(as.numeric(
      stats::var(vector, na.rm = TRUE)
    ), 2)),
    c("Range", round(as.numeric(
      base::max(vector, na.rm = TRUE) - base::min(vector, na.rm = TRUE)
    ), 2))
  ))
  colnames(ret) <- c(" ", " ")
  return(ret)
}
