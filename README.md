# DQAstats (!!! currently under development !!!)

This is the repository of the R package 'DQAstats'. It provides core functionalities to perfrom data quality assessment (DQA) of electronic health record data (EHR).  

Currently implemented features are: 

- descriptive analysis of categorical and continuous variables of the source data system and the target data system 
- checks of the extract-transform-load (ETL) pipeline (by comparing distinct values and valid values between the source data system and the target data system) 
- value conformance checks by comparing the resulting statistics to value constraints (given in a meta data repository (MDR)) 
- 'atemporal plausibility' checks 


The tool brings one main function, "DQA()", that creates a comprehensive PDF document presenting all statistics and results of the data quality assessment. 

(A meta data repository is needed for the package to work. Please refer to the FAQ to find out, how to integrate a MDR correctly.)


## Installation

You can install the development version of *DQAstats* with:

``` r
install.packages("devtools")
devtools::install_git("https://gitlab.miracum.org/miracum-dqa/dqastats.git", credentials = git2r::cred_user_pass(rstudioapi::askForPassword(prompt = "Username"), rstudioapi::askForPassword()))
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
