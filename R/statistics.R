# DQAstats - Perform data quality assessment (DQA) of electronic health records (EHR)
# Copyright (C) 2019 Universitätsklinikum Erlangen
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


countUnique <- function(data, var, sourcesystem, datamap = TRUE){

  valids <- NULL

  if (isTRUE(datamap)){
    # workaround to control for aggregated values in source system (CSV)
    special_treatment_vars <- c("patient_identifier_value", "patient_address_postalCode",
                                "patient_birthDate", "patient_gender")

    if (var %in% special_treatment_vars){
      n <- unique(data[, get(var), by="patient_identifier_value"])[,.N]
      valids <- unique(data[!is.na(get(var)), get(var), by="patient_identifier_value"])[,.N]
      missings <- unique(data[is.na(get(var)), get(var), by="patient_identifier_value"])[,.N]
    }
  }

  if (is.null(valids)){
    n <- data[,.N]
    valids <- data[!is.na(get(var)),.N]
    missings <- data[is.na(get(var)),.N]
  }

  out <- data.table::data.table("variable" = var,
                                "n" = n,
                                "valids" = valids,
                                "missings" = missings,
                                "distinct" = data[,nlevels(factor(get(var)))],
                                "sourcesystem" = sourcesystem)
  return(out)
}

# extensive summary
extensiveSummary <- function(vector){

  Q <- stats::quantile(vector, probs=c(.25, .75), na.rm=T, names=F)
  I_out <- stats::IQR(vector, na.rm=T)*1.5

  ret <- data.table::data.table(rbind(
    c("Mean", round(base::mean(vector, na.rm = T), 2)),
    c("Minimum", round(base::min(vector, na.rm = T), 2)),
    c("Median", round(stats::median(vector, na.rm = T), 2)),
    c("Maximum", round(base::max(vector, na.rm = T), 2)),
    c("SD", round(stats::sd(vector, na.rm = T), 2)),
    c("Negativ", round(as.numeric(base::sum(vector < 0, na.rm = T)), 2)),
    c("Zero", round(as.numeric(base::sum(vector == 0, na.rm = T)), 2)),
    c("Positive", round(as.numeric(base::sum(vector > 0, na.rm = T)), 2)),
    c("OutLo", round(as.numeric(base::sum(vector < (Q[1]-I_out), na.rm = T)), 2)),
    c("OutHi", round(as.numeric(base::sum(vector > (Q[2]+I_out), na.rm = T)), 2)),
    c("Skewness", round(e1071::skewness(vector, na.rm = T), 2)),
    c("Kurtosis", round(as.numeric(e1071::kurtosis(vector, na.rm=T)), 2)),
    c("Variance", round(as.numeric(stats::var(vector, na.rm=T)), 2)),
    c("Range", round(as.numeric(base::max(vector, na.rm=T) - base::min(vector, na.rm=T)), 2))
  ))
  colnames(ret) <- c(" ", " ")
  return(ret)
}

# simple summary
simpleSummary <- function(vector){
  ar <- as.data.frame(as.array(summary(vector)))
  ar[,2] <- as.character(ar[,2])
  colnames(ar) <- c(" ", " ")
  return(ar)
}

# categoricalAnalysis
categoricalAnalysis <- function(data, var, sourcesystem=NULL, levellimit=25){

  # TODO we need to define variable types at the dataimport
  data[,(var) := factor(get(var))]

  # if there are more levels than specified in levellimit (default = 20)
  if (data[,nlevels(get(var))] > levellimit){
    tabdat <- data[,.N,by=var][order(get("N"), decreasing = T)]
    tabdat_out <- tabdat[1:levellimit,]
  } else {
    tabdat_out <- data[,.N,by=var][order(get("N"), decreasing = T)]
  }
  tabdat_out[,"% Valid" := (get("N")/nrow(data)) * 100]
  colnames(tabdat_out)[2] <- "Freq"
  return(tabdat_out)
}
