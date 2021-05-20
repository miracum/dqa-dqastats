# DQAstats 
 
<!-- badges: start -->
[![R CMD Check via {tic}](https://github.com/miracum/dqa-dqastats/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=master)](https://github.com/miracum/dqa-dqastats/actions)
[![linting](https://github.com/miracum/dqa-dqastats/workflows/lint/badge.svg?branch=master)](https://github.com/miracum/dqa-dqastats/actions)
[![test-coverage](https://github.com/miracum/dqa-dqastats/workflows/test-coverage/badge.svg?branch=master)](https://github.com/miracum/dqa-dqastats/actions)
[![codecov](https://codecov.io/gh/miracum/dqa-dqastats/branch/master/graph/badge.svg)](https://codecov.io/gh/miracum/dqa-dqastats)
[![pipeline status](https://gitlab.miracum.org/miracum/dqa/dqastats/badges/master/pipeline.svg)](https://gitlab.miracum.org/miracum/dqa/dqastats/commits/master)
[![coverage report](https://gitlab.miracum.org/miracum/dqa/dqastats/badges/master/coverage.svg)](https://gitlab.miracum.org/miracum/dqa/dqastats/commits/master)
<!-- badges: end -->

The R package 'DQAstats' provides core functionalities to perfrom data quality assessment (DQA) of electronic health record data (EHR).  

Currently implemented features are:  

* descriptive (univariate) analysis of categorical and continuous variables of a source data system and a target data system 
* checks of the extract-transform-load (ETL) pipeline (by comparing distinct values and valid values between the source data system and the target data system)  
* value conformance checks by comparing the resulting statistics to value constraints (given in a meta data repository (MDR))  
* 'atemporal plausibility' checks (multivariate)  
* 'uniqueness plausibility' checks (multivariate)  

The tool provides one main function, `dqa()`, to create a comprehensive PDF document, which presents all statistics and results of the data quality assessment. 

Currently supported input data formats / databases:  

* CSV files (via R package [`data.table`](https://cran.r-project.org/package=data.table))  
* PostgreSQL (via R package [`RPostgres`](https://cran.r-project.org/package=RPostgres))  
* ORACLE (via R package [`RJDBC`](https://cran.r-project.org/package=RJDBC))  

## Installation

You can install the development version of `DQAstats` with:

``` r
install.packages("devtools")
options('repos' = 'https://ftp.fau.de/cran/')
devtools::install_github("miracum/dqa-dqastats")
```

Note: A working LaTeX installation is a prerequisite for using this software (e.g. using the R package [`tinytex`](https://yihui.org/tinytex/))!

## Configuration 

The configuration of data systems, be it CSV files or SQL-based databases, is done with environment variables, which can be set using the base R command `Sys.setenv()`.

A detailed description, which environment variables need to be set for the specific databases can be found [here](https://github.com/miracum/misc-dizutils/blob/master/README.md).

## Example

The following code example is intended to provide a minimal working example on how to apply the DQA tool to data. Example data and a corresponding MDR are provided with the R package *DQAstats* (a working LaTeX installation is a prerequisite for using this software, e.g. using the R package [`tinytex`](https://yihui.org/tinytex/)).  


* Example data: [https://github.com/miracum/dqa-dqastats/tree/master/inst/demo_data](https://github.com/miracum/dqa-dqastats/tree/master/inst/demo_data)  
* Example MDR: [https://github.com/miracum/dqa-dqastats/blob/master/inst/demo_data/utilities/MDR/mdr_example_data.csv](https://github.com/miracum/dqa-dqastats/blob/master/inst/demo_data/utilities/MDR/mdr_example_data.csv)  


```r
# load library DQAstats
library(DQAstats)

# Set environment vars to demo files paths
Sys.setenv("EXAMPLECSV_SOURCE_PATH" = system.file(
  "demo_data",
  package = "DQAstats"
))
Sys.setenv("EXAMPLECSV_TARGET_PATH" = system.file(
  "demo_data",
  package = "DQAstats"
))
utils_path <- system.file(
  "demo_data/utilities",
  package = "DQAstats"
)

# execute DQA to and generate PDF report
results <- dqa(
  source_system_name = "exampleCSV_source",
  target_system_name = "exampleCSV_target",
  utils_path = utils_path,
  mdr_filename = "mdr_example_data.csv",
  output_dir = "output/"
)

# the PDF report is stored at "./output/"
```

# More Infos:

- about MIRACUM: [https://www.miracum.org/](https://www.miracum.org/)
- about the Medical Informatics Initiative: [https://www.medizininformatik-initiative.de/index.php/de](https://www.medizininformatik-initiative.de/index.php/de)


