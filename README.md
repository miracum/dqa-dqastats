# DQAstats (!!! currently under development !!!)

The R package 'DQAstats' provides core functionalities to perfrom data quality assessment (DQA) of electronic health record data (EHR).  

Currently implemented features are: 

- descriptive (univariate) analysis of categorical and continuous variables of a source data system and a target data system 
- checks of the extract-transform-load (ETL) pipeline (by comparing distinct values and valid values between the source data system and the target data system) 
- value conformance checks by comparing the resulting statistics to value constraints (given in a meta data repository (MDR)) 
- 'atemporal plausibility' checks (multivariate)
- 'uniqueness plausibility' checks (multivariate)


The tool brings one main function, "DQA()", that creates a comprehensive PDF document, that presents all statistics and results of the data quality assessment. 

Currently, this packages is to be used together with the R packages [DQAgui](https://gitlab.miracum.org/miracum-dqa/dqagui) and [miRacumDQA](https://gitlab.miracum.org/miracum-dqa/miracumdqa) in order to work properly. 
Future work aims to make `DQAstats` applyable to other EHR data sources than those used within the MIRACUM consortium.

## Installation

You can install the development version of `DQAstats` with:

``` r
install.packages("devtools")
options('repos' = 'https://ftp.fau.de/cran/')
devtools::install_git("https://gitlab.miracum.org/miracum-dqa/dqastats.git")
```

## Example

This is a basic example to demonstrate how to perform the data quality assessment with DQAstats:

``` r
library(DQAstats)
DQA(target_config = "path/to/config_i2b2.yml",
    source_config = "path/to/config_csv.yml",
    target_db = "i2b2",
    source_db = "csv",
    utils = "path/to/_utilities")
```

# More Infos:

- about MIRACUM: [https://www.miracum.org/](https://www.miracum.org/)
- about the Medical Informatics Initiative: [https://www.medizininformatik-initiative.de/index.php/de](https://www.medizininformatik-initiative.de/index.php/de)


