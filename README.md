# DQAstats

<!-- badges: start -->
[![R CMD Check via {tic}](https://github.com/miracum/dqa-dqastats/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=master)](https://github.com/miracum/dqa-dqastats/actions)
[![linting](https://github.com/miracum/dqa-dqastats/workflows/lint/badge.svg?branch=master)](https://github.com/miracum/dqa-dqastats/actions)
[![test-coverage](https://github.com/miracum/dqa-dqastats/workflows/test-coverage/badge.svg?branch=master)](https://github.com/miracum/dqa-dqastats/actions)
[![codecov](https://app.codecov.io/gh/miracum/dqa-dqastats/branch/master/graph/badge.svg)](https://app.codecov.io/gh/miracum/dqa-dqastats)
[![pipeline status](https://gitlab.miracum.org/miracum/dqa/dqastats/badges/master/pipeline.svg)](https://gitlab.miracum.org/miracum/dqa/dqastats/-/commits/master)
[![coverage report](https://gitlab.miracum.org/miracum/dqa/dqastats/badges/master/coverage.svg)](https://gitlab.miracum.org/miracum/dqa/dqastats/-/commits/master)
<!-- badges: end -->

The R package 'DQAstats' provides core functionalities to perform data quality assessment (DQA) of electronic health record data (EHR).  

Currently implemented features are:  

* descriptive (univariate) analysis of categorical and continuous variables of a source database and a target database
* checks of the extract-transform-load (ETL) pipeline (by comparing distinct values and valid values between the source database and the target database)  
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
install.packages("remotes")
remotes::install_github("miracum/dqa-dqastats")
```

Note: A working LaTeX installation is a prerequisite for using this software (e.g. using the R package [`tinytex`](https://yihui.org/tinytex/))!

:bulb: If you want to run this in a dockerized environment you can use the [`rocker/verse`](https://hub.docker.com/r/rocker/verse/) image which has TeX already installed.

## Demo Usage

You can test the package without needing to install anything execpt docker. To try out the package follow these instructions:

1. Make sure you have [docker](https://docs.docker.com/get-docker/) installed
2. Clone this repo

    ```bash
    git clone https://gitlab.miracum.org/miracum/dqa/dqastats.git dqastats
    cd dqastats
    ```

3. Run the containerized setup using

    ```bash
    docker-compose up
    ```

4. Go to `./output/` and see the created report.

### Advanced dockerized usage

If you want to use your own docker-compose and .env file(s) you can do this simply by using them in this command:

```bash
docker-compose \
  -f docker-compose_miracum.yml \
  --env-file ../dqastats.env \
  up --build
```

## Configuration of the tool

The configuration of databases, be it CSV files or SQL-based databases, is done with environment variables, which can be set using the base R command `Sys.setenv()`.

A detailed description, which environment variables need to be set for the specific databases can be found [here](https://github.com/miracum/misc-dizutils/blob/master/README.md).

## Example

The following code example is intended to provide a minimal working example on how to apply the DQA tool to data. Example data and a corresponding MDR are provided with the R package *DQAstats* (a working LaTeX installation is a prerequisite for using this software, e.g. by using the R package [`tinytex`](https://yihui.org/tinytex/); please refer to the [DQAstats wiki](https://github.com/miracum/dqa-dqastats/wiki/Installation) for further installation instructions).  

* Example data: [https://github.com/miracum/dqa-dqastats/tree/master/inst/demo_data](https://github.com/miracum/dqa-dqastats/tree/master/inst/demo_data)  
* Example MDR: [https://github.com/miracum/dqa-dqastats/blob/master/inst/demo_data/utilities/MDR/mdr_example_data.csv](https://github.com/miracum/dqa-dqastats/blob/master/inst/demo_data/utilities/MDR/mdr_example_data.csv)  

```r
# Load library DQAstats:
library(DQAstats)

# Set environment vars to demo files paths:
Sys.setenv("EXAMPLECSV_SOURCE_PATH" = system.file("demo_data",
                                                  package = "DQAstats"))
Sys.setenv("EXAMPLECSV_TARGET_PATH" = system.file("demo_data",
                                                  package = "DQAstats"))
# Set path to utilities folder where to find the mdr and template files:
utils_path <- system.file("demo_data/utilities",
                          package = "DQAstats")

# Execute the DQA and generate a PDF report:
results <- DQAstats::dqa(
  source_system_name = "exampleCSV_source",
  target_system_name = "exampleCSV_target",
  utils_path = utils_path,
  mdr_filename = "mdr_example_data.csv",
  output_dir = "output/"
)

# The PDF report is stored at "./output/"
```

## Citation

L.A. Kapsner, J.M. Mang, S. Mate, S.A. Seuchter, A. Vengadeswaran, F. Bathelt, N. Deppenwiese, D. Kadioglu, D. Kraska, and H.-U. Prokosch, Linking a Consortium-Wide Data Quality Assessment Tool with the MIRACUM Metadata Repository, Appl Clin Inform. 12 (2021) 826–835. doi:[10.1055/s-0041-1733847](https://www.thieme-connect.com/products/ejournals/abstract/10.1055/s-0041-1733847).

```
@article{kapsner2021,
  title = {Linking a {{Consortium}}-{{Wide Data Quality Assessment Tool}} with the {{MIRACUM Metadata Repository}}},
  author = {Kapsner, Lorenz A. and Mang, Jonathan M. and Mate, Sebastian and Seuchter, Susanne A. and Vengadeswaran, Abishaa and Bathelt, Franziska and Deppenwiese, Noemi and Kadioglu, Dennis and Kraska, Detlef and Prokosch, Hans-Ulrich},
  year = {2021},
  month = aug,
  journal = {Applied Clinical Informatics},
  volume = {12},
  number = {04},
  pages = {826--835},
  issn = {1869-0327},
  doi = {10.1055/s-0041-1733847},
  language = {en}
}
```

## More Infos

* about MIRACUM: [https://www.miracum.org/](https://www.miracum.org/)
* about the Medical Informatics Initiative: [https://www.medizininformatik-initiative.de/index.php/de](https://www.medizininformatik-initiative.de/index.php/de)
